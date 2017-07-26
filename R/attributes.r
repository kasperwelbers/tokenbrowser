#' create attribute string for html tags
#'
#' @param ... named arguments are used as attributes, with the name being the name of the attribute (e.g., class, style). All argument must be vectors of the same length, or lenght 1 (used as a constant). NA values can be used to skip an attribute. If all attributes are NA, an NA is returned
#'
#' @return a character vector with attribute strings. Designed to be usable as the attr_str in add_tag()
#' @export
tag_attr <- function(...) {
  attr = list(...)
  for (name in names(attr)) {
    attr[[name]] = as.character(attr[[name]])
    not_na = !is.na(attr[[name]])
    attr[[name]][!is.na(attr[[name]])] = stringi::stri_paste(name, '="', attr[[name]][!is.na(attr[[name]])], '"', sep='')
  }
  do.call(paste_na_omit, args = c(attr, sep=' '))
}

#' Create the content of the html style attribute
#'
#' Designed to be used together with the html_attributes function.
#'
#' @param ... named arguments are used as settings in the html style attribute, with the name being the name of the setting (e.g., background-color). All arguments must be vectors of the same length. NA values can be used to ignore a setting, and if all settings are NA then NA is returned (instead of an empty string for style settings).
#'
#' @export
#'
#' @return a character vector with the content of the html style attribute
attr_style <- function(...){
  style = list(...)
  for (name in names(style)) {
    style[[name]] = as.character(style[[name]])
    not_na = !is.na(style[[name]])
    style[[name]][!is.na(style[[name]])] = stringi::stri_paste(name, style[[name]][!is.na(style[[name]])], sep=': ')
  }
  do.call(paste_na_omit, args = c(style, sep='; '))
}


