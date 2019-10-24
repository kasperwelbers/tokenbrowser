#' View a browser (HTML) in the R viewer
#'
#' @param url An URL, created with *_browser
#'
#' @export
#' @examples
#' url = create_browser(sotu_data$tokens, sotu_data$meta, token_col = 'token', header = 'Speeches')
#'
#' \donttest{
#' view_browser(url)   ## view browser in the Viewer
#' }
view_browser <- function(url) {
  viewer <- getOption("viewer")
  viewer(url)
}

