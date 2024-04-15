#' Write Status Object of JASP Modules
#'
#' @param jaspRoot the root of the jasp-desktop github repository
#'
#' @export
writeModuleStatusObject <- function(jaspRoot) {

  # TODO: validate jaspRoot
  moduleStatusObject     <- computeModuleStatusObject(jaspRoot)
  moduleStatusObjectPath <- getModuleStatusObjectPath()
  devcat(sprintf("writing module status to: %s\n", moduleStatusObjectPath))

  saveRDS(moduleStatusObject, file = moduleStatusObjectPath)
}

computeModuleStatusObject <- function(jaspRoot) {

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

  # TODO: would needs to know the module build directory, could speed up building
  needsReinstallation <- character()

  # this object is not space efficient and uses strings for everything, but hopefully nobody cares.
  return(list(
    jaspPackageNames    = basename(allPkgs),
    dependencies        = dependenciesAndPaths,
    # these two are unused for now
    md5sums             = md5sums,
    needsReinstallation = needsReinstallation
  ))

}

loadModuleStatusObject <- function() {

  moduleStatusObjectPath <- getModuleStatusObjectPath()

  if (file.exists(moduleStatusObjectPath)) {

    moduleStatusObject <- readRDS(file = moduleStatusObjectPath)
    options("jaspModuleInstallerModuleStatusObject" = moduleStatusObject)

  } else {

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
