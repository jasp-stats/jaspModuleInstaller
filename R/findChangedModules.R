computeModuleStatus <- function(jaspRoot) {

  jaspModules <- Filter(
    function(p) isJaspSourcePackage(p),
    list.dirs(file.path(jaspRoot, "Modules"),, recursive = FALSE)
  )

  jaspPackages <- Filter(
    function(p) isJaspSourcePackage(p),
    list.dirs(file.path(jaspRoot, "Engine"), recursive = FALSE)
  )

  allPkgs <- c(jaspModules, jaspPackages)
  md5sums <- lapply(c(jaspModules, jaspPackages), createMd5Sums)
  names(md5sums) <- basename(allPkgs)

  dependencies <- getJaspDependencies(jaspModules)

  installedPath <- path.package("jaspModuleInstaller", quiet = FALSE)
  objectPath <- file.path(installedPath, "moduleMd5sums.rds")

  # TODO: needs to know the module build directory!
  needsReinstallation <- character()

  cat(sprintf("writing the module dependency graph, md5sums, and which modules need reinstallation to: %s\n", objectPath))

  saveRDS(list(dependencies = dependencies, md5sums = md5sums, needsReinstallation = needsReinstallation), file = objectPath)

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
  if (!file.exists(descriptionPath))
    return(NULL)
  Filter(function(x) startsWith(prefix = "jasp", x = x), renv::dependencies(descriptionPath, quiet = TRUE)[, "Package"])
}
