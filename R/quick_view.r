#' View a browser (HTML) in the R viewer
#'
#' @param url An URL, created with *_browser
#'
#' @export
view_browser <- function(url) {
  viewer <- getOption("viewer")
  viewer(url)
}

