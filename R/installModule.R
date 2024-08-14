createLocalRecord <- function(modulePkg, moduleInfo, cacheAble = TRUE, addJaspToVersion = TRUE, hash = computeModuleHash(modulePkg)) {
  record <- list(list(
    Package    = moduleInfo[["Package"]],
    Version    = if (addJaspToVersion) addLocalJaspToVersion(moduleInfo[["Version"]]) else moduleInfo[["Version"]],
    Source     = "Local",
    RemoteType = "local",
    RemoteUrl  = modulePkg,
    Cacheable  = cacheAble,
    Hash       = hash
  ))
  names(record) <- moduleInfo[["Package"]]
  record
}

createRecordsOfLocalJaspModules <- function(pathToModule) {

  engineRoot <- normalizePath(file.path(pathToModule, "..", "..", "Engine"))
  modulePaths <- c(
    file.path(engineRoot, "jaspGraphs"),
    file.path(engineRoot, "jaspBase"),
    pathToModule
  )

  modulePaths  <- union(normalizePath(modulePaths), normalizePath(extractModuleDependenciesFromStatusObject(pathToModule)))
  moduleHashes <- extractModuleHashesFromStatusObject(basename(modulePaths))

  records <- vector("list", length(modulePaths))
  for (i in seq_along(modulePaths)) {

    modulePath <- modulePaths[i]
    moduleInfo <- getModuleInfo(modulePath)
    record <- createLocalRecord(modulePath, moduleInfo, hash = moduleHashes[i])
    records[i] <- record
    names(records)[i] <- names(record)

  }

  return(records)

}

getRecordsFromPkgdepends <- function(modulePaths, timeout = 60) {
  rscript <- file.path(R.home("bin"), "R")
  path <- file.path(system.file(package = "jaspModuleInstaller"), "getModuleDependencies.R")

  pkgdependsLibrary <- getOption("PKGDEPENDS_LIBRARY")

  f <- tempfile()
  on.exit({
    if (file.exists(f))
      file.remove(f)
  })
  args <- c(
    "--slave",
    "--verbose",
    paste0("--file=", path),
    "--args",
    pkgdependsLibrary,
    getOption("repos"),
    f,
    paste0("local::", modulePaths)
  )

  result <- system2(command = rscript, args = args, timeout = timeout)

  if (result != 0) {
    warning("getRecordsFromPkgdepends failed!")
    return(list())
  } else {
    return(readRDS(f))
  }
}

processLockFile <- function(lockfile, pathToModule, installMode = "identicalToLockfile") {
  processedLockFile <- file.path(pathToModule, "_processedLockFile.lock")
  file.copy(lockfile, processedLockFile, overwrite=TRUE)

  if (installMode == "localizeAll") { #only use local JASP modules from the install folder where possible
    records <- createRecordsOfLocalJaspModules(pathToModule)
    processedLockFile <- renv::record(records = records, lockfile = processedLockFile)
  }
  else if(installMode == "localizeModuleOnly") { #fetch everything from remote except the packages containing the lockfile
    localizedRecord <- createLocalRecord(pathToModule, getModuleInfo(pathToModule))
    processedLockFile <- renv::record(records = localizedRecord, lockfile = processedLockFile)
  }
  # else "identicalToLockfile" head empty and only build dependencies

  return(processedLockFile)
}

cleanModuleLibrary <- function() {
  identical(toupper(Sys.getenv("JASP_CLEAN_MODULE_LIBRARY", unset = "TRUE")), "TRUE")
}

getInstallMode <- function() {
  Sys.getenv("MODULE_INSTALL_MODE")
}

#' @export
installJaspModule <- function(modulePkg, moduleLibrary, repos, onlyModPkg, force = FALSE, frameworkLibrary = NULL) {
  if (.Platform$OS.type == "windows") {
    prepareRtoolsEnv()
  }
  validateCompilationAbilities()

  isPkgArchive <- isModulePkgArchive(modulePkg)
  assertValidJASPmodule(modulePkg)

  r <- getOption("repos")
  r["CRAN"] <- repos
  options(repos = r)

  # if (!isPkgArchive && !(force || md5SumsChanged(modulePkg, moduleLibrary))) {
  #   moduleName <- getModuleInfo(modulePkg)[["Package"]]
  #   if (dir.exists(file.path(moduleLibrary, moduleName))) {
  #     print(sprintf("Nothing changed according to md5sums, not reinstalling %s.", moduleName))
  #     return("succes!")
  #   } else {
  #     print(sprintf("Checksums exist for %s but the package is missing, installing anyway!", moduleName))
  #   }
  # }

  maybeBuildTools <- if (require("pkgbuild", quietly = TRUE)) {
    function(x) getFromNamespace("with_build_tools", "pkgbuild")(x, required = FALSE)
  } else {
    identity
  }

  tryCatch({
    maybeBuildTools({
      installJaspModuleImpl(modulePkg, moduleLibrary, repos, onlyModPkg)
      # if (hasRenvLockFile(modulePkg)) installJaspModuleFromRenv(       modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg)
      # else                            installJaspModuleFromDescription(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, frameworkLibrary = frameworkLibrary)
    })
  }, error = function(e) {
    if (is.null(e[["output"]])) {
      stop(e, domain = NA)
    } else {
      return(stop(e[["output"]], domain = NA))
    }
  })

  return('succes!')
}

installJaspModuleImpl <- function(modulePkg, moduleLibrary, repos, onlyModPkg) {
  cat(sprintf(
    "Installing module with renv.\ninstallJaspModuleImpl('%s', '%s', '%s', %s)\nwith libPaths:\n%s\n",
    modulePkg, moduleLibrary, repos, onlyModPkg,
    paste(paste("    ", .libPaths()), collapse = "\n")
  ))

  setupRenv(moduleLibrary, modulePkg)
  loadModuleStatusObject()

  clean <- cleanModuleLibrary()
  moduleName <- getModuleName(modulePkg)

  lockfilePath <- getRenvLockFile(modulePkg)
  if (!file.exists(lockfilePath) | Sys.getenv("REGENERATE_LOCKFILE") == "ON")
  {
    print("lockfile does not exist or will be overwritten")
    generateBasicLockfile(lockfilePath, modulePkg)
  }

  installMode <- getInstallMode()
  processedLockFile <- processLockFile(lockfilePath, modulePkg, installMode)
  libs <- .libPaths()
  library <- c(moduleLibrary, libs[length(libs)])

  #this part do
  records <- renv::restore(lockfile = processedLockFile, clean = clean, library = library)
  

  # if (isAutoGenerated(lockfile)) {
  #
  #   updatedLockfile <- renv::record(records = records, lockfile = updatedLockfile)
  #
  #   cat("Autogenerated lockfile, updating all packages.\n")
  #   records <- renv::update(project = modulePkg)
  #
  #   if (is.list(records) && !is.logical(records))
  #     updatedLockfile <- renv::record(records = records, lockfile = updatedLockfile)
  #
  #   renv::lockfile_write(lockfile = updatedLockfile, file = lockfilePath)
  #
  # }

}

isAutoGenerated <- function(lockfile) {
  isTRUE(lockfile[["JASP"]][["autogenerated"]])
}

getModuleStatusObject <- function(ifNotExists = c("error", "warning", "ignore")) {
  moduleStatusObject <- getOption("jaspModuleInstallerModuleStatusObject")
  if (is.null(moduleStatusObject)) {
    ifNotExists <- match.arg(ifNotExists)
    if (ifNotExists == "error")
      stop("module status object does not exist but is required for autogenerating lockfiles.", domain = NA)
    if (ifNotExists == "warning")
      warning("module status object does not exist but is required for autogenerating lockfiles.", domain = NA)
  }

  return(moduleStatusObject)

}

extractModuleDependenciesFromStatusObject <- function(pathToModule) {

  moduleStatusObject <- getModuleStatusObject()

  pathsToAdd <- moduleStatusObject[["dependencies"]][[basename(pathToModule)]]
  if (is.null(pathsToAdd) || !all(file.exists(pathsToAdd)))
    warning(sprintf(
      "The module %s depends on other jasp modules (%s) but these do not exist locally in the Modules folder! Most likely these will be downloaded from GitHub.",
      basename(pathToModule),
      paste(basename(pathsToAdd[!file.exists(pathsToAdd)]), collapse = ", ")
    ))

  return(pathsToAdd)

}

extractModuleHashesFromStatusObject <- function(moduleNames) {

  moduleStatusObject <- getModuleStatusObject()
  # should we check if the names exist? probably otherwise the hash becomes: <NA> NA
  return(moduleStatusObject[["md5sums"]][moduleNames])

}


generateBasicLockfile <- function(lockfilePath, modulePkg) {

  warning("Creating a basic lockfile")
  if(file.exists(lockfilePath)) file.remove(lockfilePath)
  records <- getRecordsFromPkgdepends(modulePkg)
  lockfile <- renv:::renv_lockfile_init(NULL)
  lockfile <- renv::record(records, lockfile = lockfile)
  renv::lockfile_write(lockfile, lockfilePath)
}


updateLockfile <- function(modulePkg) {
  lockfilePath <- getRenvLockFile(modulePkg)
  if(!file.exists(lockfilePath)) { #make one if it doesnt exist yet
    return(generateBasicLockfile(lockfilePath, modulePkg))
  }

  warning("Updating lockfile")

  #get current records and extract the locked record
  currentRecords = renv::lockfile_read(lockfilePath)
  getLockedRecords <- function(pkg) {
    !is.null(pkg$JASP_LOCK)
  }
  lockedRecords <- Filter(getLockedRecords, currentRecords$Packages)
  lockedNames <- lapply(lockedRecords, function(x) { x$Package })

  #gather new ones using pkgdepends magic and filter out those who conflict with locked ones
  newRecords <- getRecordsFromPkgdepends(modulePkg)
  noConflicts <- function(pkg) {
    !(pkg$Package %in% lockedNames)
  }
  nonConflicting <- Filter(noConflicts, newRecords)
  processedRecords <- c(lockedRecords, nonConflicting)

  file.remove(lockfilePath)
  lockfile <- renv:::renv_lockfile_init(NULL)
  lockfile <- renv::record(processedRecords, lockfile = lockfile)
  renv::lockfile_write(lockfile, lockfilePath)

}