#' Utilities for Checksums
#'
#' @param modulePkg the path to a JASP module.
#' @param moduleLibrary the build folder where the JASP module is installed.
#'
#' @rdname checksums
#' @return `createMd5Sums` returns a named vector with md5sum for each file name. The names of the vector are the file names.
#' `md5SumsChanged` returns TRUE if the checksums are different from the local files and FALSE otherwise.
#' @export
#'
createMd5Sums <- function(modulePkg) {

  srcFiles <- c(
    list.files(modulePkg,                         recursive = TRUE, full.names = TRUE, pattern = "(NAMESPACE|DESCRIPTION)$"),
    list.files(file.path(modulePkg, "src"),       recursive = TRUE, full.names = TRUE, pattern = "(\\.(cpp|c|hpp|h)|(Makevars|Makevars\\.win))$"),
    list.files(file.path(modulePkg, "R"),         recursive = TRUE, full.names = TRUE, pattern = "\\.R$"),
    list.files(file.path(modulePkg, "inst"),      recursive = TRUE, full.names = TRUE, pattern = "\\.(qml|po|svg|png|jpg|md)$"),
    list.files(file.path(modulePkg, "inst"),      recursive = TRUE, full.names = TRUE, pattern = "\\qmldir$"),
    list.files(modulePkg,                         recursive = TRUE, full.names = TRUE, pattern = "renv\\.lock")
  )
  newMd5Sums <- tools::md5sum(srcFiles)
  newMd5Sums
}

makeMd5SumsFilename <- function(modulePkg, moduleLibrary) {
  file.path(moduleLibrary, "..", paste(basename(modulePkg), "md5sums.rds", sep = "_"))
}

writeMd5Sums <- function(modulePkg, moduleLibrary) {
  saveRDS(createMd5Sums(modulePkg), file = makeMd5SumsFilename(modulePkg, moduleLibrary))
}

readMd5Sums <- function(modulePkg, moduleLibrary) {
  readRDS(file = makeMd5SumsFilename(modulePkg, moduleLibrary))
}


#' @rdname checksums
md5SumsChanged <- function(modulePkg, moduleLibrary) {

  file <- makeMd5SumsFilename(modulePkg, moduleLibrary)
  if (!file.exists(file))
    return(TRUE)

  oldMd5Sums <- readRDS(file)
  newMd5Sums <- createMd5Sums(modulePkg)

  return(!identical(oldMd5Sums, newMd5Sums))

}
