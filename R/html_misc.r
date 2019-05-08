#' Wrap values in an HTML tag
#'
#' @param x a vector of values to be wrapped in a tag
#' @param tag A character vector of length 1, specifying the html tag (e.g., "div", "h1", "span")
#' @param attr_str A character string of the same length as x (or of length 1).
#' @param ignore_na If TRUE, do not add tag if value is NA
#' @param span_adjacent If TRUE, include adjacent tokens with identical attr_str within the same tag
#'
#' @export
#'
#' @return a character vector
add_tag <- function(x, tag, attr_str=NULL, ignore_na=F, span_adjacent=F) {
  if (!is.null(attr_str)) {
    attr_str = ifelse(is.na(attr_str), yes = if (ignore_na) NA else '',
                                       no = stringi::stri_paste(' ', attr_str, sep=''))
  } else attr_str = if (ignore_na) NA else ''

  if (span_adjacent) {
    same_as_next = attr_str == c(attr_str[-1], '#####')
    same_as_prev = attr_str == c('#####', attr_str[-length(attr_str)])

    x = ifelse(!same_as_prev, stringi::stri_paste('<',tag, attr_str,'>',  x, sep=''), x)
    x = ifelse(!same_as_next, stringi::stri_paste(x, '</',tag,'>', sep=''), x)
  } else x = stringi::stri_paste('<',tag, attr_str,'>',  x,  '</',tag,'>',  sep='')
  x
}


#' HTML tables for meta data per document
#'
#' Each row of the data.frame is transformed into a html table with two columns: name and value.
#' The columnnames of meta are used as names.
#'
#' @param meta a data.frame where each row represents the meta data for a document
#' @param ignore_col optionally, a character vector with names of metadata columns to ignore
#'
#' @return a character vector where each value contains a string for an html table.
#' @export
create_meta_tables <- function(meta, ignore_col=NULL) {
  if (ncol(meta) > 0) {
    html_table = ''
    for (col in colnames(meta)) {
      if (col == 'NAVIGATION_STRING') next
      if (col %in% ignore_col) next
      html_table = stringi::stri_paste('\n', html_table, '<tr><th>', col, '</th><td>', as.character(meta[[col]]), '</td></tr>')
    }
    add_tag(html_table, 'table', attr_str = tag_attr(class='meta_table'))
  } else ''
}

