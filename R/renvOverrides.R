# NOTE: this file must be self contained!
# It is (once) sourced independently so it cannot refer to any functions outside of this file.
# as a consequence, this file is a tad long

#' Determine the operating system
#'
#' @return Either, "windows", "osx", or "linux".
#' @export
getOS <- function() {

  os <- NULL

  if (!is.null(Sys.info())) {

    os <- Sys.info()["sysname"]

    if (os == "Darwin")
      os <- "osx"

  } else {

    if (grepl("^darwin", R.version$os))
      os <- "osx"

    if (grepl("linux-gnu", R.version$os))
      os <- "linux"

  }

  if (is.null(os))
    stop("Unable to determine the operating system because `Sys.info()` returned NULL.", domain = NA)

  return(tolower(os))

}

assignFunctionInPackage <- function(fun, name, package) {
  ns <- getNamespace(package)
  unlockBinding(name, ns)
  assign(name, fun, ns)
  lockBinding(name, ns)
}

assignFunctionInRenv <- function(fun, name) {
  assignFunctionInPackage(fun, name, "renv")
}

isJaspModule <- function(path) {
  path <- normalizePath(path)
  nm <- basename(path)
  !identical(nm, "jaspModuleInstaller") &&
    startsWith(prefix = "jasp", x = nm) &&
    any(
      # installed package
      dir.exists(file.path(path, "qml")),
      # source package
      dir.exists(file.path(path, "inst", "qml"))
    )
  # original check that fails for installed locations
  # grepl("jasp-desktop/[Engine|Modules]", path)
}

isJaspSourcePackage <- function(path) {
  path <- normalizePath(path, winslash = "/")
  nm    <- basename(path)
  # dirnm1 <- basename(dirname(path))
  # dirnm2 <- basename(dirname(dirname(path)))
  !identical(nm, "jaspModuleInstaller") &&
    startsWith(prefix = "jasp", x = nm) &&
    grepl("jasp-desktop/[Engine|Modules]", path)
    # also works but a bit wonky
    # dirnm2 == "jasp-desktop"            &&
    # (dirnm1 == "Modules" || dirnm1 == "Engine")
}

addLocalJaspToVersion <- function(version) {
  suffix <- "_Local_JASP"
  if (!endsWith(x = version, suffix = suffix))
    return(paste0(version, suffix))
  return(version)
}

renv_remotes_resolve_path_impl_override <- function(path) {
  desc <- renv:::renv_description_read(path)

  # start of changes
  # print(path)
  if (isJaspModule(path)) {
    # cat(sprintf("renv_remotes_resolve_path_impl_override, path = %s\n", path))
    Cacheable <- TRUE
    Version <- addLocalJaspToVersion(desc$Version)
    cat(sprintf("computing hash for jasp module at path %s\n", path))
    Hash    <- computeModuleHash(path)

  } else {
    Cacheable <- FALSE
    Version <- desc$Version
    Hash    <- NULL
  }
  list(Package = desc$Package, Version = Version, Source = "Local",
       RemoteType = "local", RemoteUrl = path, Cacheable = Cacheable, Hash = Hash)
  # end of changes
}

isCalledFromRenvSnapshot <- function() {
  calls <- sys.calls()
  # print(calls)
  calledFromSnapShot <- FALSE
  modify <- TRUE
  for (call in calls)
    if (identical(call[[1]], as.symbol("snapshot"))) {
      modify <- TRUE
      break
    }
  return(calledFromSnapShot)
}

renv_description_read_override <- function(path = NULL, package = NULL, subdir = NULL, field = NULL,
                                            ...) {
  `%||%` <- renv:::`%||%`
  path <- path %||% find.package(package)
  if (!renv:::renv_path_absolute(path))
    path <- renv:::renv_path_normalize(path)
  if (dir.exists(path)) {
    components <- c(path, if (nzchar(subdir %||% "")) subdir,
                    "DESCRIPTION")
    path <- paste(components, collapse = "/")
  }
  if (!file.exists(path))
    renv:::stopf("DESCRIPTION file %s does not exist", renv:::renv_path_pretty(path))
  description <- renv:::filebacked(context = "renv_description_read",
                                   path = path, callback = renv:::renv_description_read_impl, subdir = subdir,
                                   ...)
  if (isJaspModule(dirname(path))) {
    # cat(sprintf("renv_description_read_override, path = %s\n", path))

    # version should only be adjusted when this is NOT called from snapshot
    # why though?
    modify <- TRUE#!isCalledFromRenvSnapshot()
    # cat(sprintf("renv_description_read_override, modify = %s\n", modify))
    if (modify)
      description[["Version"]] <- addLocalJaspToVersion(description[["Version"]])
  }
  if (!is.null(field))
    return(description[[field]])
  description
}

renv_lockfile_diff_record_override <- function(before, after) {

  # renv:::renv_record_normalize removes hash information
  before0 <- before
  after0  <- after

  before <- renv:::renv_record_normalize(before)
  after  <- renv:::renv_record_normalize(after)

  # this is not 100% necessary, but I'm not sure if there is a better and easier way to determine
  # whether a package is a JASP module (other than startsWith(x, "JASP"), which is a bit unsafe)
  jaspPackages <- getOption("jaspModuleInstallerModuleStatusObject")[["jaspPackageNames"]]

  # first, compare on version / record existence
  type <- renv:::case(
    is.null(before) ~ "install",
    is.null(after)  ~ "remove",
    before$Version < after$Version ~ "upgrade",
    before$Version > after$Version ~ "downgrade",
    before$Package %in% jaspPackages && before0$Hash != after0$Hash ~ "install (JASP: source code changed)"
  )

  if (!is.null(type))
    return(type)

  # check for a crossgrade -- where the package version is the same,
  # but details about the package's remotes have changed
  if (!setequal(renv:::renv_record_names(before), renv:::renv_record_names(after)))
    return("crossgrade")

  nm <- union(renv:::renv_record_names(before), renv:::renv_record_names(after))
  if (!identical(before[nm], after[nm]))
    return("crossgrade")

  NULL

}

isPathInRenvCache <- function(path) {
  # TODO: normalizePath(path) is used to follow symlinks, needs to be verified across OSes
  startsWith(x = normalizePath(path), prefix = normalizePath(renv::paths$cache()))
}

extractHashFromRenvCachePath <- function(path) {
  # TODO: normalizePath(path) is used to follow symlinks, needs to be verified across OSes
  path |> normalizePath() |> dirname() |> basename()
}

renv_hash_description_override <- function(path) {

  if (isJaspModule(path) && isPathInRenvCache(path)) {

    return(extractHashFromRenvCachePath(path))

  } else {

    renv:::filebacked(
      context  = "renv_hash_description",
      path     = path,
      callback = renv:::renv_hash_description_impl
    )

  }
}

hackRenv <- function(onlyBootstrap_platform_prefix = FALSE) {

  # renv adds e.g,. "R-3.6/x86_64-pc-linux-gnu" to all paths (R-version/os) and we don't need that
  assignFunctionInRenv(function() return(""),                   "renv_bootstrap_platform_prefix")

  # when installing Rcpp/ RInside we can stop here, when installing modules we need the overrides below as well
  if (onlyBootstrap_platform_prefix)
    return()

  assignFunctionInRenv(renv_remotes_resolve_path_impl_override, "renv_remotes_resolve_path_impl")
  assignFunctionInRenv(renv_description_read_override,          "renv_description_read")
  assignFunctionInRenv(renv_lockfile_diff_record_override,      "renv_lockfile_diff_record")
  assignFunctionInRenv(renv_hash_description_override,          "renv_hash_description")

}

postInstallFixes <- function(folderToFix) {
  if (length(ls(all.names = TRUE, pattern = "\\.postProcessLibraryModule")) > 0 ) {
    # We seem to be running in JASP (but this won't be used because renv starts a separate R process)
    # print("we are *in* jasp, so we use .postProcessLibraryModule!")
    .postProcessLibraryModule(folderToFix)
  } else {
    # We do not have that function available so we will need to start JASPEngine ourselves, but where is it?
    old_PATH <- Sys.getenv("PATH")

    #sometimes R.dll is not in the path on windows, despite this being called from R...
    if (getOS() == "windows")
      Sys.setenv("PATH" = paste0(R.home(component = 'bin'), ';', old_PATH, collapse = ""))

    jaspEngineLocation <- Sys.getenv("JASPENGINE_LOCATION", unset = file.path(getwd(), "..", "JASPEngine"))
    jaspEngineCall     <- paste0(jaspEngineLocation, ' "', folderToFix ,'"')
    #print(paste0("Not *in* JASP so calling JASPEngine as: '", jaspEngineCall ,"'"))

    if (getOS() == "osx")
      system(jaspEngineCall)

    if (getOS() == "windows")
      Sys.setenv("PATH" = old_PATH);
  }
}

addRenvBeforeAfterDispatch <- function() {

  renBeforeAfterInstallStruct <- structure(list(
    before.install = function(x) {
      #print("BEFORE INSTALLING")
      #print(sprintf("Path = %s", mget("path", envir = parent.frame(1),
      #                                ifnotfound = "unknown path")))
      0 #do nothing
    },

    after.install = function(x) {
      installPath <- mget("installpath", envir = parent.frame(1), ifnotfound = "unknown")

      if (installPath != "unknown") {
        print(sprintf("Installed %s to '%s', now running post install fixes.", x, installPath))
        postInstallFixes(installPath)
      } else {
        print(sprintf("Installing %s did not work immediately, but renv might still look at remotes for this.", x))
      }
    }),
    class = "renvInstallHookOverride"
  )

  options(renv.install.package.options = renBeforeAfterInstallStruct)
}

#' @export
`[[.renvInstallHookOverride` <- function(x, ...) {
  return(unclass(x))
}

setupRenv <- function(moduleLibrary, modulePkg = NULL) {

  hackRenv()

  options(
    "install.opts"                = "--no-multiarch --no-docs --no-test-load", # no test-load because on mac the paths need to be fixed
    "renv.config.install.verbose" = TRUE,  # not necessary but saves headaches
    "renv.config.cache.enabled"   = TRUE,  # enable using a cache
    "renv.cache.linkable"         = TRUE,  # undocumented, see renv:::renv_cache_linkable. required for linking to a custom folder
    "renv.config.auto.snapshot"   = FALSE, # new option, we don't want this
    "renv.snapshot.ignore.self"   = FALSE  # currently unused, but can be useful.
  )

  if (!is.null(modulePkg)) {
    # renv_package_find crashes when package is base.
    # for some terrible reason, people explicitly do base:: even though this isn't necessary.
    renv::settings$ignored.packages(project = modulePkg, value = "base", persist = FALSE)
  }

  cachePaths <- strsplit(Sys.getenv("RENV_PATHS_CACHE"), .Platform$path.sep)

  for (cachePath in cachePaths[[1]]) #strsplit is vectorized but we only give it a single string, so index to that single first one
    if (!dir.exists(cachePath))
      stop(sprintf("A cache is supposed to be at '%s' but it does not exist!", cachePath))

  Sys.setenv("RENV_PATHS_LIBRARY" = moduleLibrary)

  cat("Using the following paths:\n")
  for (name in names(renv::paths))
    cat(sprintf("%s:%s%s\n", name, paste0(rep(" ", 12 - nchar(name)), collapse = ""), renv::paths[[name]]()))

  # Try to nudge renv towards installing binaries when possible
  # NOTE: this needs to be reconsidered!
  if (getOS() == "windows" || getOS() == "osx")
    options(install.packages.compile.from.source = "never")

  addRenvBeforeAfterDispatch()

  # renv::install("Matrix", type="source")

}
