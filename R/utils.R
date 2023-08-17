isModulePkgArchive <- function(modulePkg) {
  return(any(endsWith(modulePkg, c(".tar.gz", ".zip", ".tgz"))))
}

assertValidJASPmodule <- function(modulePkg) {

  if (isModulePkgArchive(modulePkg))
    return() # Let R and JASP handle it

  if (!file.exists(file.path(modulePkg, "DESCRIPTION")))
    stop("Your module is missing a 'DESCRIPTION' file!")

  if (!file.exists(file.path(modulePkg, "inst", "Description.qml")))
    stop("Your module is missing 'inst/Description.qml'!")

  if (!dir.exists(file.path(modulePkg, "R")))
    stop("Your module is missing an 'R' directory!")

  if (!dir.exists(file.path(modulePkg, "inst", "qml")))
    stop("Your module is missing the 'inst/qml' directory!")

}

getFileFromModule <- function(modulePkg, filename) {

  hereItGoes <- file.path(modulePkg, filename)

  if (isModulePkgArchive(modulePkg)) {
    temp <- tempdir()

    #The archive contains a folder first, which has the name of the package, which we could or could not guess here.
    #lets just look at all the files
    files <- utils::untar(tarfile = modulePkg, list = TRUE)
    found <- endsWith(files, filename)

    if (!any(found))
      stop(paste0("Can't find file '", filename, "' in archive '", modulePkg, "'"))

    #this will only work properly if the requested file is in there only once but for things like DESCRIPTION that should be no problem
    filename <- files[found]

    utils::untar(tarfile = modulePkg, files = filename, exdir = temp)
    hereItGoes <- file.path(temp, filename)
  }

  if (!file.exists(hereItGoes))
    stop(paste("Your module contains no ", filename, " file"))

  return(hereItGoes)
}

getModuleInfo <- function(modulePkg) {
  return(read.dcf(getFileFromModule(modulePkg, "DESCRIPTION"))[1, ])
}

getModuleName <- function(modulePkg) {
  getModuleInfo(modulePkg)[["Package"]]
}

getRenvLockFile <- function(modulePkg) {
  return(file.path(modulePkg, "renv.lock"))
}

hasRenvLockFile <- function(modulePkg) {
  return(file.exists(getRenvLockFile(modulePkg)))
}

validateCompilationAbilities <- function() {
  # renv uses the following output without checking for length but assuming it is 1.
  # if that isnt the case the (module) installation fails obscurely with an error like "Error in if (eval(cond, envir = environment(dot))) return(eval(expr, envir = environment(dot))): the condition has length > 1"
  cmdConfigCC <- system2(c(renv:::R(), "CMD", "config", "CC"), stdout = TRUE, stderr = TRUE)
  if (length(cmdConfigCC) > 1)
    stop(
      "R CMD config CC returns more than 1 line, this will break renv and thus your install.
  Most likely you are on mac and you should run `xcode-select --install` in a terminal.
  If that doesn't help or you aren't on a mac: feel free to open an issue at https://github.com/jasp-stats/jasp-issues/issues/new/choose

  The output was:
  ", paste0(cmdConfigCC, collapse = "\n"), domain = NA)
}

getflag <- function(envVar, default) {
  identical(toupper(Sys.getenv(envVar, unset = default)), "TRUE")
}

isVerbose <- function() {
  getflag("JASP_VERBOSE_JASPMODULEINSTALLER", TRUE)
}

devCatFile <- function(file) {
  return(Sys.getenv("JASP_JASPMODULEINSTALLER_LOG_REDIRECT", unset = ""))
}

devcat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {

  if (!isVerbose())
    return()

  if (identical(file, "")) # identical since stdout() and stderr() are also valid
    file <- devCatFile(file)

  cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
}

