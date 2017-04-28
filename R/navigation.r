anchor_ref_list <- function(x) {
  refstr = stringi::stri_paste('#nav', x, sep='_')
  anchor_refs = add_tag(x, 'a', tag_attr(href = refstr))
  anchor_refs = add_tag(anchor_refs, 'li')
  stringi::stri_paste(anchor_refs, collapse='\n')
}
