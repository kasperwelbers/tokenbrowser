#' Convert tokens into full texts in an HTML file
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the reader as document meta
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param filename  Name of the output file. Default is temp file
#' @param doc_width The width of the document text field
#' @param css_str   A character string, to be directly added to the css style header
#'
#' @return The name of the file where the reader is saved. Can be opened conveniently from within R using browseUrl()
#' @export
create_reader <- function(tokens, meta=NULL, doc_col='doc_id', token_col='token', filename=NULL, doc_width=750, css_str=NULL){
  docs = wrap_documents(tokens, meta, doc_col, token_col)
  docstring = stringi::stri_paste(docs, collapse='\n\n')

  doc_ids = unique(tokens[[doc_col]])
  n_doc = length(doc_ids)
  nav = anchor_ref_list(doc_ids)

  template = html_template('reader', doc_width=doc_width, css_str=css_str)
  template$header = gsub('$NAVIGATION$', nav, template$header, fixed = T)
  template$header = gsub('$HEADER$', paste(n_doc, 'documents'), template$header, fixed = T)

  save_html(docstring, template, filename)
}

#' Convert tokens into full texts in an HTML file with highlighted tokens
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param value     Either a logical vector or a numeric vector with
#'                  values between 0 and 1. If a logical vector is used, then tokens
#'                  with TRUE will be highlighted (with the color specified in pos_col).
#'                  If a numeric vector is used, the value determines the alpha (transparency),
#'                  with 0 being fully transparent and 1 being fully colored.
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the reader as document meta
#' @param col       The color used to highlight
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param filename  Name of the output file. Default is temp file
#' @param ...       Additional formatting arguments passed to create_reader()
#'
#' @return The name of the file where the reader is saved. Can be opened conveniently from within R using browseUrl()
#' @export
highlighted_reader <- function(tokens, value, meta=NULL, col='yellow', doc_col='doc_id', token_col='token', filename=NULL, ...){
  tokens[[token_col]] = highlight_tokens(tokens[[token_col]], value=value, col = col)
  create_reader(tokens, meta, doc_col, token_col, filename, ...)
}

#' Convert tokens into full texts in an HTML file with color ramp highlighting
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param value     A numeric vector with values between -1 and 1. Determines the color
#'                  mixture of the scale colors specified in col_range
#' @param alpha     Optionally, the alpha (transparency) can be specified, with 0 being fully
#'                  transparent and 1 being fully colored. This can be a vector to specify a
#'                  different alpha for each value.
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the reader as document meta
#' @param col_range The color used to highlight
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param filename  Name of the output file. Default is temp file
#' @param ...       Additional formatting arguments passed to create_reader()
#'
#' @return The name of the file where the reader is saved. Can be opened conveniently from within R using browseUrl()
#' @export
colorscaled_reader <- function(tokens, value, alpha=0.4, meta=NULL, col_range=c('red','blue'), doc_col='doc_id', token_col='token', filename=NULL, ...){
  tokens[[token_col]] = colorscale_tokens(tokens[[token_col]], value=value, col_range = col_range, alpha=alpha)
  create_reader(tokens, meta, doc_col, token_col, filename, ...)
}

