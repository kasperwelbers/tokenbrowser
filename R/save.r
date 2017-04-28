#' Wrap html body in the template and save
#'
#' @param data     The html body data
#' @param template The html header/footer template
#' @param filename The name of the file to save the html. Default is a temp file
#'
#' @return The (local) url to the html file
#' @export
save_html <- function(data, template, filename=NULL) {
  if (is.null(filename)) {
    filename = tempfile("tokenbrowser_", fileext = ".html")
    message("Writing html to ", filename)
  }
  sink(filename)
  cat(template$header)
  cat(data)
  cat(template$footer)
  sink()
  filename
}

#' create html file
#'
#' For creating the html in batches, the create_html(), fill_html() and close_html() functions can be used.
#' create_html() and close_html() require the template for creating the header and footer.
#' fill_html() can be used sequentally to fill the html.
#'
#' The connection closes between each fill, to prevent incidental writing to the file.
#'
#' @param template The html template file, as created with \link{html_template}
#' @param filename
#'
#' @return the filename
#' @export
create_html <- function(template, filename=NULL) {
  if (is.null(filename)) {
    filename = tempfile("tokenbrowser_", fileext = ".html")
    message("Writing html to ", filename)
  }
  sink(filename)
  cat(template$header)
  sink()
  filename
}

#' Fill html file
#'
#' see documentation for \link{create_html}
#'
#' @param data     The html body data
#' @param filename The name of the file to save the html. Default is a temp file
#'
#' @export
fill_html <- function(data, filename) {
  sink(filename, append = T)
  cat(data)
  sink()
}

#' close html file
#'
#' see documentation for \link{create_html}
#'
#' @param template The html header/footer template
#' @param filename The name of the file to save the html. Default is a temp file
#'
#' @export
close_html <- function(template, filename) {
  sink(filename, append = T)
  cat(template$footer)
  sink()
  invisible(filename)
}
