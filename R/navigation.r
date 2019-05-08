id_nav <- function(x) {
  x = as.character(x)
  x = x[!is.na(x)]
  refstr = stringi::stri_paste('|||', x, '|||', sep='')
  refs = add_tag(x, 'input',
                 tag_attr(type='checkbox',
                          class='navselect',
                          name=refstr,
                          onchange='navSelect()',
                          value=refstr))
  refs = add_tag(refs, 'li')
  stringi::stri_paste(refs, collapse='\n')
}

