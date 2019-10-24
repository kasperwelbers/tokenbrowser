test_that("tokenbrowser", {
  d = sotu_data

  ## simple
  d$tokens
  url = create_browser(d$tokens, d$meta, token_col = 'token', header = 'Speeches')
  #view_browser(url)

  ## simple with navigation
  url = create_browser(d$tokens, d$meta, token_col = 'token', header = 'Speeches', doc_nav = 'headline')
  #view_browser(url)


  ## highlight
  highlight = nchar(as.character(sotu_data$tokens$token))
  highlight = round(highlight / max(highlight), 1)
  highlight[highlight < 0.3] = NA
  url = highlighted_browser(sotu_data$tokens, value = highlight, sotu_data$meta)
  #view_browser(url)

  ## scales
  scale = nchar(as.character(sotu_data$tokens$token))
  scale[scale>6] = scale[scale>6] +20
  scale = rescale_var(sqrt(scale), -1, 1)
  scale[abs(scale) < 0.5] = NA
  url = colorscaled_browser(sotu_data$tokens, value = scale, meta=sotu_data$meta)
  #view_browser(url)

  ## search results
  code = rep(NA, nrow(sotu_data$tokens))
  code[grep('war', sotu_data$tokens$token)] = 'War'
  code[grep('mother|father|child', sotu_data$tokens$token)] = 'Family'
  code = as.factor(code)
  url = categorical_browser(sotu_data$tokens, category=(code), meta=sotu_data$meta)
  view_browser(url)

  ## categories (using top_nav)
  category = match(sotu_data$tokens$pos, c('N','M','V'))
  category[sotu_data$tokens$doc_id == unique(sotu_data$tokens$doc_id)[2]] = NA ## add empty document for test
  url = categorical_browser(sotu_data$tokens, category=category, labels=c('N','M','V'), meta=sotu_data$meta, top_nav=2)
  view_browser(url)


})

