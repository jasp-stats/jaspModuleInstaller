args <- commandArgs(TRUE)

# meant to be called via system or system2
#
# args should have the form
#
# library path
# repository
# tempfile
# paths to jasp modules

# example
# args <- c(
#   build/R/pkgdepends_library,
#   "https://cloud.r-project.org",
#   tempfile(),
#   "local::jasp-desktop/Modules/jaspTTests",
#   "local::jasp-desktop/Engine/jaspBase",
#   "local::jasp-desktop/Engine/jaspGraphs"
# )

stopifnot(length(args) >= 4L)

.libPaths(args[1])
print(.libPaths())

repos <- getOption("repos")
repos["CRAN"] <- args[2]
options(repos = repos)

tempPath    <- args[3]
jaspModules <- args[4:length(args)]

pd <- pkgdepends::new_pkg_installation_proposal(jaspModules, config = list(library=tempdir()))
pd$solve()
pd$draw()
sol <- pd$get_solution()
dat <- sol$data

# this could be expanded but we currently do not support other repos anyway...
fromRepository <- which(dat$type == "standard")
fromGitHub     <- which(dat$type == "github")
fromInstalled  <- which(dat$type == "installed")

recordsFromRepository <- setNames(lapply(fromRepository, function(i) {
  list(Package = dat$package[i], Version = dat$version[i], Source = "Repository")
}), dat$package[fromRepository])

recordsFromInstalled <- setNames(lapply(fromInstalled, function(i) {
  list(Package = dat$package[i], Version = dat$version[i], Source = "Repository")
}), dat$package[fromInstalled])

recordsFromGithub <- setNames(lapply(fromGitHub, function(i) {
  list(
    Package        = dat$package[i],
    Version        = dat$version[i],
    Source         = "GitHub",
    RemoteType     = dat$metadata[[i]][["RemoteType"]],
    RemoteHost     = dat$metadata[[i]][["RemoteHost"]],
    RemoteUsername = dat$metadata[[i]][["RemoteUsername"]],
    RemoteRepo     = dat$metadata[[i]][["RemoteRepo"]],
    RemoteSha      = dat$metadata[[i]][["RemoteSha"]]
  )
}), dat$package[fromGitHub])

combinedRecords <- c(recordsFromInstalled, recordsFromGithub, recordsFromRepository)

saveRDS(combinedRecords, file = tempPath)
