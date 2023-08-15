assignFunctionInPackage <- function(fun, name, package) {
  ns <- getNamespace(package)
  unlockBinding(name, ns)
  assign(name, fun, ns)
  lockBinding(name, ns)
}

addLocalJaspToVersion <- function(version) {
  suffix <- "_Local_JASP"
  if (!endsWith(x = version, suffix = suffix))
    return(paste0(version, suffix))
  return(version)
}

createLocalRecord <- function(modulePkg, moduleInfo, cacheAble = TRUE, addJaspToVersion = TRUE) {
  record <- list(list(
    Package    = moduleInfo[["Package"]],
    Version    = addLocalJaspToVersion(moduleInfo[["Version"]]),
    Source     = "Local",
    RemoteType = "local",
    RemoteUrl  = modulePkg,
    Cacheable  = cacheAble
  ))
  names(record) <- moduleInfo[["Package"]]
  record
}

updateLockFileWithLocalJaspModules <- function(pathToLockfile, pathToModule) {

  print(pathToLockfile)
  print(pathToModule)
  lockfile <- renv::lockfile_read(pathToLockfile)

  engineRoot <- normalizePath(file.path(pathToModule, "..", "..", "Engine"))
  modulePaths <- c(
    file.path(engineRoot, "jaspGraphs"),
    file.path(engineRoot, "jaspBase"),
    pathToModule
  )
  records <- vector("list", length(modulePaths))
  for (i in seq_along(modulePaths)) {

    modulePath <- modulePaths[i]
    moduleInfo <- getModuleInfo(modulePath)
    record <- createLocalRecord(modulePath, moduleInfo)
    records[i] <- record
    names(records)[i] <- names(record)

  }

  updatedLockfile <- renv::record(records = records, lockfile = lockfile)
  return(updatedLockfile)
}

cleanModuleLibrary <- function() {
  identical(toupper(Sys.getenv("JASP_CLEAN_MODULE_LIBRARY", unset = "TRUE")), "TRUE")
}

isJaspModule <- function(path) {
  path <- normalizePath(path)
  startsWith(prefix = "jasp", x = basename(path)) &&
    grepl("jasp-desktop/[Engine|Modules]", path)
}

getInstallMode <- function() {
  return("localJaspPackages")
}

#' @export
installJaspModule <- function(modulePkg, moduleLibrary, repos, onlyModPkg, force = FALSE, frameworkLibrary = NULL) {

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
    getFromNamespace("with_build_tools", "pkgbuild")
  } else {
    identity
  }

  tryCatch({
    maybeBuildTools({
      installJaspModuleFromRenv(modulePkg, moduleLibrary, repos, onlyModPkg)
      # if (hasRenvLockFile(modulePkg)) installJaspModuleFromRenv(       modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg)
      # else                            installJaspModuleFromDescription(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, frameworkLibrary = frameworkLibrary)
    },
    required = FALSE)
  }, error = function(e) {
    if (is.null(e[["output"]])) {
      stop(e, domain = NA)
    } else {
      return(stop(e[["output"]], domain = NA))
    }
  })

  return(invisible(TRUE))
}


installJaspModuleFromRenv <- function(modulePkg, moduleLibrary, repos, onlyModPkg) {

  print(sprintf("Installing module with renv. installJaspModuleFromRenv('%s', '%s', '%s', %s)",
                modulePkg, moduleLibrary, repos, onlyModPkg))

  setupRenv(moduleLibrary, modulePkg)
  installMode <- getInstallMode()

  clean <- cleanModuleLibrary()
  moduleName <- getModuleName(modulePkg)

  lockfilePath <- getRenvLockFile(modulePkg)
  jaspLockfilePath <- tempfile(fileext = "jasp_renv.lock")

  file.copy(from = lockfilePath, to = jaspLockfilePath, overwrite = TRUE)

  updatedLockfile <- updateLockFileWithLocalJaspModules(jaspLockfilePath, modulePkg)
  renv::lockfile_write(updatedLockfile, file = jaspLockfilePath)

  if (installMode == "identicalToLockfile") {

    # install remote versions of jasp module dependencies but local version of the module
    renv::restore(lockfile = lockfilePath,      exclude = moduleName, clean = clean, library = moduleLibrary)
    renv::restore(lockfile = jaspLockfilePath, packages = moduleName, clean = clean, library = moduleLibrary)

  } else if (installMode == "localJaspPackages") {

    # install local versions of jasp module and jasp module dependencies
    renv::restore(lockfile = jaspLockfilePath, clean = clean, library = moduleLibrary)

  }

}

