token_nav_string <- function(tokens, meta, doc_col, token_nav) {
  utok = stats::na.omit(unique(tokens[,c(doc_col,token_nav)]))
  utok = split(utok[[token_nav]], utok[[doc_col]])
  ids = names(utok)

  navstring = stringi::stri_paste_list(utok, sep = '|||')
  navstring = stringi::stri_paste('|||',navstring,'|||',sep='')

  out = rep(NA, nrow(meta))
  out[match(ids, meta[[doc_col]])] = navstring
  out
}
