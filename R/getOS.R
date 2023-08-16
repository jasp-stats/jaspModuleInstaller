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
