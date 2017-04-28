#' create the html template
#'
#' @param template The name of the template to be used
#' @param doc_width The width of the document text field
#' @param css_str A character string, to be directly added to the css style header
#'
#' @return A list with the html header and footer
#' @export
html_template <- function(template, doc_width=750, css_str=NULL) {
  TEMPLATE = system.file(sprintf("template/%s.html", template), package="tokenbrowser", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  html = stringi::stri_split(str = html, fixed = '$CONTENT$')[[1]]
  html = list(header = html[1], footer = html[2])

  css = create_css(template, doc_width=doc_width, css_str=css_str)
  html$header = gsub("$CSS$", css, html$header, fixed = T)
  html
}

create_css <- function(template, doc_width='750px', css_str=NULL) {
  CSS_TEMPLATE = system.file(sprintf("template/%s.css", template), package="tokenbrowser", mustWork=T)
  css = readLines(CSS_TEMPLATE, warn=F)

  ## add custom settings
  if (!is.null(css_str)) {
    css = c(css, css_str)
  }

  ## replace custom settings
  css = stringi::stri_paste(css, collapse='\n')
  css = gsub('$doc_width$', doc_width, css, fixed = T)

  css
}

