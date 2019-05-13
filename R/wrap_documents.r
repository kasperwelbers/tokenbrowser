create_doc_headers <- function(meta, doc_col='doc_id', nav=doc_col) {
  title = add_tag(meta[[doc_col]], 'doc_id')


  if (!is.null(nav)) {
    navtags = add_tag(nav, 'div', tag_attr(style=attr_style(display="none")))
    meta = create_meta_tables(meta, ignore_col = doc_col)
    stringi::stri_paste(title, navtags, meta, sep='\n')
  } else {
    meta = create_meta_tables(meta, ignore_col = doc_col)
    stringi::stri_paste(title, meta, sep='\n')
  }

}

wrap_tokens <- function(tokens, doc_col='doc_id', token_col='token'){
  if (any(is.na(tokens[[token_col]]))) {
    if (is.factor(tokens[[token_col]])) {
      levels(tokens[[token_col]]) = union(levels(tokens[[token_col]]), '')
    }
    tokens[[token_col]][is.na(tokens[[token_col]])] = ''
  }
  text = split(tokens[[token_col]], f = tokens[[doc_col]])
  text = stringi::stri_paste_list(text, sep=' ')
  text = gsub('\\n', '<br>', text)
  sprintf('<p>%s</p>', pretty_text_wrap(text))
}

pretty_text_wrap <- function(x){
  x = gsub('_| ', ' ', x)
  x = gsub(" ([.,?!:;>)])", '\\1', x)
  x = gsub('([(<]) ', '\\1', x)
  x
}

#top_category <- function(meta, tokens, category, doc_col){
#  agg = stats::aggregate(category, by=list(tokens[[doc_col]], category), FUN=length)
#  agg = agg[order(-agg$x),]
#  agg = agg[!duplicated(agg[[1]]),]
#  agg[[2]][match(meta[[doc_col]], agg[[1]])]
#}

#' Wrap tokens into document html strings
#'
#' @param tokens     A data.frame with a column for document ids (doc_col)
#'                   and a column for tokens (token_col)
#' @param meta       A data.frame with a column for document_ids (doc_col). All other columns are added
#'                   to the browser as document meta
#' @param doc_col    The name of the document id column
#' @param token_col  The name of the token column
#' @param nav        The column in meta used for nav. Defaults to 'doc_id'
#' @param token_nav  Alternative to nav (which uses meta), a column in tokens used for navigation
#' @param top_nav    If token_nav is used, navigation filters will only apply to the top x values with highest token occurence in a document
#' @param thres_nav  Like top_nav, but specifying a threshold for the minimum number of tokens.
#'
#' @return A named vector, with document ids as names and the document html strings as values
#' @export
wrap_documents <- function(tokens, meta, doc_col='doc_id', token_col='token', nav=doc_col, token_nav=NULL, top_nav=NULL, thres_nav=NULL) {
  if (!methods::is(tokens, 'data.frame')) tokens = as.data.frame(tokens)

  doc_id = unique(tokens[[doc_col]])
  if (!is.null(meta)) {
    meta = as.data.frame(meta)
    meta = meta[match(doc_id, meta[[doc_col]]),]
  } else {
    meta = data.frame(doc_id = doc_id)
    colnames(meta) = doc_col
  }

  if (!is.null(token_nav)) {
    nav = token_nav_string(tokens, meta, doc_col, token_nav, top_nav, thres_nav)
    header = create_doc_headers(meta, doc_col = doc_col, nav=nav)
  } else {
    nav = if (is.null(nav)) NULL else sprintf('<tag>%s</tag>', meta[[nav]])
    header = create_doc_headers(meta, doc_col = doc_col, nav= nav)
  }

  texts = wrap_tokens(tokens, doc_col=doc_col, token_col=token_col)
  docs = stringi::stri_paste(header, texts, sep='\n')

  docs = add_tag(docs, 'article', tag_attr(insearch="1",infilter="1"))
  names(docs) = doc_id

  docs
}

