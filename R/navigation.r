id_nav <- function(x, cols=NULL, navfilter=T) {
  x = as.character(x)
  x = x[!is.na(x)]
  refstr = stringi::stri_paste('|||', x, '|||', sep='')
  if (navfilter) {
    refs = add_tag(x, 'input',
                   tag_attr(type='checkbox',
                            class='navselect',
                            name=refstr,
                            onchange='navSelect()',
                            value=refstr))
  } else {
    refs = add_tag(x, 'span')
  }

  if (!is.null(cols)) {
    if (length(cols) != length(x)) stop('Number of colors does not match number of (non-na) values')
    cols = highlight_col(rep(0.4,length(cols)),cols)
    span = add_tag('', 'span', tag_attr(class='dot', style=attr_style(`background-color`= cols)))
    refs = stringi::stri_paste(span,refs)
  }
  refs = add_tag(refs, 'li')
  stringi::stri_paste(refs, collapse='\n')
}

