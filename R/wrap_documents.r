create_doc_headers <- function(meta, doc_col='doc_id', add_anchor=T, nav=doc_col) {
  title = add_tag(meta[[doc_col]], 'doc_id')

  if (add_anchor) {
    anchor = sprintf('<a name="nav%s" nav="nav%s"></a>', meta[[nav]], meta[[nav]])
    meta = create_meta_tables(meta, ignore_col = doc_col)
    stringi::stri_paste(anchor, title, meta, sep='\n')
  } else {
    meta = create_meta_tables(meta, ignore_col = doc_col)
    stringi::stri_paste(title, meta, sep='\n')
  }

}

wrap_tokens <- function(tokens, doc_col='doc_id', token_col='token'){
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

top_category <- function(meta, tokens, category, doc_col){
  agg = aggregate(category, by=list(tokens[[doc_col]], category), FUN=length)
  agg = agg[order(-agg$x),]
  agg = agg[!duplicated(agg[[1]]),]
  agg[[2]][match(meta[[doc_col]], agg[[1]])]
}

#' Wrap tokens into document html strings
#'
#' @param tokens     A data.frame with a column for document ids (doc_col)
#'                   and a column for tokens (token_col)
#' @param meta       A data.frame with a column for document_ids (doc_col). All other columns are added
#'                   to the reader as document meta
#' @param doc_col    The name of the document id column
#' @param token_col  The name of the token column
#' @param add_anchor If True, each document will contain an a tag with name 'nav_%s' (see nav argument), that can
#'                   be used for navigation.
#' @param nav        The column in meta used for nav. Defaults to 'doc_id'
#'
#' @return A named vector, with document ids as names and the document html strings as values
#' @export
wrap_documents <- function(tokens, meta, doc_col='doc_id', token_col='token', add_anchor=T, nav=doc_col) {
  if (!is(tokens, 'data.frame')) tokens = as.data.frame(tokens)

  doc_id = unique(tokens[[doc_col]])
  if (!is.null(meta)) {
    meta = as.data.frame(meta)
    meta = meta[match(doc_id, meta[[doc_col]]),]
  } else {
    meta = data.frame(doc_id = doc_id)
    colnames(meta) = doc_col
  }

  header = create_doc_headers(meta, doc_col = doc_col, add_anchor=add_anchor, nav=nav)
  texts = wrap_tokens(tokens, doc_col=doc_col, token_col=token_col)
  backref = '<p align="right"><a href=\"#top\">(back to top)</a></p>'

  docs = stringi::stri_paste(header, texts, backref, sep='\n')
  docs = add_tag(docs, 'article', tag_attr(nav  = meta[[nav]]))
  names(docs) = doc_id
  docs = docs[order(meta[,nav,drop=F])]
  docs
}

