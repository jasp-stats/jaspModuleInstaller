
renv_remotes_resolve_path_impl_override <- function(path) {
  desc <- renv:::renv_description_read(path)

  # start of changes
  print(path)
  if (isJaspModule(path)) {
    cat(sprintf("renv_remotes_resolve_path_impl_override, path = %s\n", path))
    Cacheable <- TRUE
    # print("Version")
    # print(desc$Version)

    Version <- addLocalJaspToVersion(desc$Version)
    # Version <- desc$Version
    # print(Version)
  } else {
    Cacheable <- FALSE
    Version <- desc$Version
  }
  list(Package = desc$Package, Version = Version, Source = "Local",
       RemoteType = "local", RemoteUrl = path, Cacheable = Cacheable)
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
    cat(sprintf("renv_description_read_override, path = %s\n", path))

    # version should only be adjusted when this is NOT called from snapshot
    modify <- !isCalledFromRenvSnapshot()
    cat(sprintf("renv_description_read_override, modify = %s\n", modify))
    if (modify)
      description[["Version"]] <- addLocalJaspToVersion(description[["Version"]])
  }
  if (!is.null(field))
    return(description[[field]])
  description
}

hackRenv <- function() {

  # renv adds e.g,. "R-3.6/x86_64-pc-linux-gnu" to all paths (R-version/os) and we don't need that
  assignFunctionInPackage(
    fun     = function() return(""),
    name    = "renv_bootstrap_platform_prefix",
    package = "renv"
  )

  assignFunctionInPackage(renv_remotes_resolve_path_impl_override, "renv_remotes_resolve_path_impl", "renv")
  assignFunctionInPackage(renv_description_read_override, "renv_description_read", "renv")
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

setupRenv <- function(moduleLibrary, modulePkg) {

  hackRenv()

  options(
    "renv.config.install.verbose" = TRUE,  # not necessary but saves headaches
    "renv.config.cache.enabled"   = TRUE,  # enable using a cache
    "renv.cache.linkable"         = TRUE,  # undocumented, see renv:::renv_cache_linkable. required for linking to a custom folder
    "renv.config.auto.snapshot"   = FALSE, # new option, we don't want this
    "renv.snapshot.ignore.self"   = FALSE  # currently unused, but can be useful.
  )

  # renv_package_find crashes when package is base.
  # for some terrible reason, people explicitly do base:: even though this isn't necessary.
  renv::settings$ignored.packages(project = modulePkg, value = "base", persist = FALSE)

  cachePaths <- strsplit(Sys.getenv("RENV_PATHS_CACHE"), .Platform$path.sep)

  for (cachePath in cachePaths[[1]]) #strsplit is vectorized but we only give it a single string, so index to that single first one
    if (!dir.exists(cachePath))
      stop(sprintf("A cache is supposed to be at '%s' but it does not exist!", cachePath))

  Sys.setenv("RENV_PATHS_LIBRARY" = moduleLibrary)

  cat("Using the following paths:\n")
  for (name in names(renv::paths))
    cat(sprintf("%s:%s%s\n", name, paste0(rep(" ", 12 - nchar(name)), collapse = ""), renv::paths[[name]]()))

  options(install.opts = "--no-multiarch --no-docs --no-test-load"); #make sure we do not do a test-load, because this will not work on mac. the rpaths still need to be fixed

  # Try to nudge renv towards installing binaries when possible
  # NOTE: this needs to be reconsidered!
  if (getOS() == "windows" || getOS() == "osx")
    options(install.packages.compile.from.source = "never")

  addRenvBeforeAfterDispatch()

  # renv::install("Matrix", type="source")

}
