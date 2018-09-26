
#' View a reader (HTML) in the rstudio viewer
#'
#' @param url An URL, created with *_reader
#'
#' @export
view_reader <- function(url) {
  viewer <- getOption("viewer")
  viewer(url)
}

