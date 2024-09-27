#' Write Status Object of JASP Modules
#'
#' @param jaspRoot the root of the jasp-desktop github repository
#' @param oldObject optional, the old module status object of a previous run
#' @param modulesBinaryPath the path to folder where the modules are installed
#' @param jaspModulesToBeInstalled the modules that are to be installed (as specified by cmake)
#'
#' @export
writeModuleStatusObject <- function(jaspRoot, oldObject = NULL, modulesBinaryPath = "", jaspModulesToBeInstalled = character()) {

  validateJaspRoot(jaspRoot)
  moduleStatusObject     <- computeModuleStatusObject(jaspRoot, oldObject, modulesBinaryPath, jaspModulesToBeInstalled)
  moduleStatusObjectPath <- getModuleStatusObjectPath()
  devcat(sprintf("writing module status to: %s\n", moduleStatusObjectPath))

  saveRDS(moduleStatusObject, file = moduleStatusObjectPath)
}

computeModuleStatusObject <- function(jaspRoot, oldObject, modulesBinaryPath, jaspModulesToBeInstalled) {

  devcat("creating module status object\n")

  jaspModules <- normalizePath(Filter(
    function(p) isJaspSourcePackage(p),
    list.dirs(file.path(jaspRoot, "Modules"), recursive = FALSE)
  ), winslash = "/")

  jaspPackages <- normalizePath(Filter(
    function(p) isJaspSourcePackage(p),
    list.dirs(file.path(jaspRoot, "Engine"), recursive = FALSE)
  ), winslash = "/")

  allPkgs <- c(jaspModules, jaspPackages)
  md5sums <- vapply(allPkgs, computeModuleHash, character(1L))
  names(md5sums) <- basename(allPkgs)
  names(allPkgs) <- basename(allPkgs) # assumes package name == folder name

  dependencies <- getJaspDependencies(jaspModules)
  dependenciesAndPaths <- lapply(dependencies, function(pkgs) {
    allPkgs[pkgs]
  })

  newObject <- list(
    jaspPackageNames    = basename(allPkgs),
    dependencies        = dependenciesAndPaths,
    md5sums             = md5sums
  )

  # NOTE: would needs to know the module build directory, could speed up building
  # NOTE: currently assumes that the old object mirrors the build directory, could alternatively read this info from the build directory instead?
  newObject[["needsReinstallation"]] <- findModulesThatNeedReinstallation(newObject, oldObject, modulesBinaryPath, jaspModulesToBeInstalled)

  # this object is not space efficient and uses a lot of strings, but hopefully nobody cares.
  return(newObject)

}

findModulesThatNeedReinstallation <- function(objNew, objOld, moduleBinaryPath, jaspModulesToBeInstalled = character()) {

  needsReinstallation        <- logical(length(objNew[["md5sums"]]))
  names(needsReinstallation) <- names(objNew[["md5sums"]])

  if (is.null(objOld)) {
    cat("No previous module status object found, assuming that all modules need to be reinstalled.\n")
    return(!needsReinstallation)
  }

  commonNames <- intersect(names(objOld[["md5sums"]]), names(objNew[["md5sums"]]))

  newNames <- setdiff(names(objNew[["md5sums"]]), commonNames)
  needsReinstallation[newNames] <- FALSE
  needsReinstallation[commonNames] <- objNew[["md5sums"]][commonNames] != objOld[["md5sums"]][commonNames]

  # these sources changed according to md5sums
  sourcesChanged <- needsReinstallation
  nmsSourcesChanged <- names(sourcesChanged[sourcesChanged])

  # validate that at least there is a directory for the module, in case people manually deleted the module
  didNotChange <- names(needsReinstallation[!needsReinstallation])
  didNotChange2 <- if (length(jaspModulesToBeInstalled) == 0) {
    # until jaspModulesToBeInstalled is actually passed from cmake
    setdiff(didNotChange, c("jaspBase", "jaspGraphs"))
  } else {
    # intersect between packages in build folder and modules to be installed as specified in cmake
    intersect(didNotChange, jaspModulesToBeInstalled)
  }
  existsInBuildFolder <- dir.exists(file.path(moduleBinaryPath, didNotChange2, didNotChange2))
  names(existsInBuildFolder) <- didNotChange2
  nmsDidNotExistInBuildFolder <- didNotChange2[!existsInBuildFolder]

  if (!any(sourcesChanged) && all(existsInBuildFolder)) {
    cat(sprintf(paste0("None of the installed modules changed relative to the previous run.\n")))
    return(setNames(rep(TRUE, length(newNames)), newNames))
  }

  # could also store reverse dependencies in the object directly, instead of computing them on the fly
  for (pkg in nmsSourcesChanged) {
    revDeps <- vapply(objNew[["dependencies"]], function(x) pkg %in% names(x), logical(1L))
    matches <- intersect(names(revDeps[revDeps]), jaspModulesToBeInstalled)
    needsReinstallation[matches] <- TRUE
  }

  needsReinstallAsDependency <- needsReinstallation & !sourcesChanged
  nmsNeedsReinstallAsDependency <- names(needsReinstallAsDependency[needsReinstallAsDependency])


  cat(sprintf(paste0(
      "The following %d modules had source code changes relative to the previous run:\n\n%s\n\n",
      "The following %d modules depend on some of the modules listed above and also need to be reinstalled:\n\n%s\n\n",
      "The following %d modules were missing from the build folder and may need to be reinstalled:\n\n%s\n\n"
    ), length(nmsSourcesChanged), paste(nmsSourcesChanged, collapse = ", "),
       length(nmsNeedsReinstallAsDependency), paste(nmsNeedsReinstallAsDependency, collapse = ", "),
       length(nmsDidNotExistInBuildFolder), paste(nmsDidNotExistInBuildFolder, collapse = ", ")))

  return(needsReinstallation)
}

checkIfModuleNeedsToBeInstalled <- function(moduleName) {

  loadModuleStatusObject()
  moduleStatusObject <- getOption("jaspModuleInstallerModuleStatusObject")

  if (is.null(moduleStatusObject))
    return(TRUE)

  if (!(moduleName %in% names(moduleStatusObject[["needsReinstallation"]]))) {
    warning("Module ", moduleName, " not found in the module status object, assuming it needs to be installed.", domain = NA)
    return(TRUE)
  }

  return(moduleStatusObject[["needsReinstallation"]][[moduleName]])

}

loadModuleStatusObject <- function(returnObject = FALSE, warnIfNotExists = FALSE) {

  obj <- getOption("jaspModuleInstallerModuleStatusObject")
  if (!is.null(obj)) {
    if (returnObject)
      return(obj)
    else
      return()
  }

  moduleStatusObjectPath <- getModuleStatusObjectPath()

  if (file.exists(moduleStatusObjectPath)) {

    moduleStatusObject <- readRDS(file = moduleStatusObjectPath)
    options("jaspModuleInstallerModuleStatusObject" = moduleStatusObject)

    if (returnObject)
      return(moduleStatusObject)

  } else if (warnIfNotExists) {

    warning(sprintf("Attempting to load the moduleStatusObject at %s but it does not exists.", moduleStatusObjectPath), domain = NA)

  }

}

getModuleStatusObjectPath <- function() {

  installedPath <- find.package(package = "jaspModuleInstaller", quiet = FALSE)
  moduleStatusObjectPath <- file.path(installedPath, "moduleStatusObject.rds")
  moduleStatusObjectPath
}

getJaspDependencies <- function(jaspModules) {

  # direct dependencies listed in DESCRIPTION files
  jaspModuleDeps <- lapply(jaspModules, getJaspDependenciesFromDescription)
  moduleNames <- basename(jaspModules)
  names(jaspModuleDeps) <- moduleNames

  # loop over the modules and add jasp dependencies
  safety <- 50
  for (i in 1:safety) {

    old <- jaspModuleDeps
    for (j in seq_along(jaspModuleDeps)) {
      if (!is.null(jaspModuleDeps[[j]])) {
        idx <- match(jaspModuleDeps[[j]], moduleNames, nomatch = 0L)
        jaspModuleDeps[[j]] <- union(jaspModuleDeps[[j]], Reduce(union, jaspModuleDeps[idx]))
      }
    }

    if (identical(old, jaspModuleDeps))
      break

  }

  if (i == safety)
    warning(sprintf(
      "Failed to discover all jasp dependencies after %d recursive iterations. getJaspDepsRecursively needs to be revisited!",
      safety
      ), domain = NA)

  return(jaspModuleDeps)

}

getJaspDependenciesFromDescription <- function(modulePath) {

  descriptionPath <- file.path(modulePath, "DESCRIPTION")

  if (!file.exists(descriptionPath)) {
    warning(sprintf("Trying to find dependencies from DESCRIPTION at path '%s' but it does not exist! Double check if the module exists, perhaps you forgot to run `git submodule update --init`?", descriptionPath))
    return(NULL)
  }

  Filter(function(x) startsWith(prefix = "jasp", x = x), renv::dependencies(descriptionPath, quiet = TRUE)[, "Package"])

}

validateJaspRoot <- function(jaspRoot) {

  if (!dir.exists(jaspRoot))
    stop("jaspRoot (", jaspRoot, ") contains does not exist!")

  if (!dir.exists(file.path(jaspRoot, "Modules")))
    stop("jaspRoot/Modules (", jaspRoot, "/Modules) does not exist!")

  if (!dir.exists(file.path(jaspRoot, "Engine")))
    stop("jaspRoot/Engine (", jaspRoot, "/Engine) does not exist!")

}
