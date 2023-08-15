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

updateLockFileWithLocalJaspModules <- function(pathToLockfile) {

  lockfile <- renv::lockfile_read(pathToLockfile)
  pathToThisModule <- dirname(normalizePath(pathToLockfile))
  modulePaths <- c("../../Engine/jaspGraphs", pathToThisModule)
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
installJaspModule <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, force = FALSE, frameworkLibrary = NULL) {

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

  result <- tryCatch({
    maybeBuildTools({
      installJaspModuleFromRenv(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg)
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

  return(result)
}


installJaspModuleFromRenv <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg) {

  print(sprintf("Installing module with renv. installJaspModuleFromRenv('%s', c(%s), '%s', '%s', %s)",
                modulePkg, paste0("'", libPathsToUse, "'", collapse = ", "), moduleLibrary, repos, onlyModPkg))

  setupRenv(moduleLibrary, modulePkg)
  installMode <- getInstallMode()

  clean <- cleanModuleLibrary()
  moduleName <- getModuleName()

  lockfilePath <- "renv.lock"
  jaspLockfilePath <- tempfile(fileext = "jasp_renv.lock")

  file.copy(lockfilePath, jaspLockfilePath, overwrite = TRUE)

  updatedLockfile <- updateLockFileWithLocalJaspModules(jaspLockfilePath)
  renv::lockfile_write(updatedLockfile, file = jaspLockfilePath)
  renv::restore(lockfile = "jasp_updated_renv.lock", clean = clean)

  if (installMode == "identicalToLockfile") {

    # install remote versions of jasp module dependencies but local version of the module
    renv::restore(lockfile = "renv.lock",               exclude = moduleName, clean = clean, library = moduleLibrary)
    renv::restore(lockfile = "jasp_updated_renv.lock", packages = moduleName, clean = clean, library = moduleLibrary)

  } else if ("localJaspPackages") {

    # install local versions of jasp module and jasp module dependencies
    renv::restore(lockfile = "jasp_updated_renv.lock", clean = clean, library = moduleLibrary)

  }

}

