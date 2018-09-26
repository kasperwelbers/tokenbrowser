id_nav <- function(x) {
  x = as.character(x)
  x = x[!is.na(x)]
  refstr = stringi::stri_paste('#nav', 1:length(x), sep='')
  anchor_refs = add_tag(x, 'a', tag_attr(href = refstr))
  anchor_refs = add_tag(anchor_refs, 'li')
  stringi::stri_paste(anchor_refs, collapse='\n')
}
