test_that("tokenbrowser", {
  d = sotu_data

  ## simple
  url = create_browser(d$tokens, d$meta, token_col = 'token', header = 'Speeches')
  #view_browser(url)

  ## highlight
  highlight = nchar(as.character(sotu_data$tokens$token))
  highlight = highlight / max(highlight)
  highlight[highlight < 0.3] = NA
  url = highlighted_browser(sotu_data$tokens, value = highlight, sotu_data$meta)
  #view_browser(url)

  ## scales
  scale = nchar(as.character(sotu_data$tokens$token))
  scale[scale>6] = scale[scale>6] +20
  scale = rescale_var(sqrt(scale), -1, 1)
  scale[abs(scale) < 0.5] = NA
  url = colorscaled_browser(sotu_data$tokens, value = scale, meta=sotu_data$meta)
  view_browser(url)

  ## categories
  category = match(sotu_data$tokens$pos, c('N','M','V'))
  category[sotu_data$tokens$doc_id == unique(sotu_data$tokens$doc_id)[2]] = NA ## add empty document for test
  url = categorical_browser(sotu_data$tokens, category=category, labels=c('N','M','V'), meta=sotu_data$meta, multi_cat = T)
  view_browser(url)
})

