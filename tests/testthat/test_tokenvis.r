test_that("tokenbrowser", {
  d = sotu_data

  url = create_reader(d$tokens, d$meta)
  view_reader(url)
  browseURL(url)

  highlight = nchar(as.character(sotu_data$tokens$token))
  highlight = highlight / max(highlight)
  highlight[highlight < 0.3] = NA
  url = highlighted_reader(sotu_data$tokens, value = highlight, sotu_data$meta)
  view_reader(url)
  #browseURL(url)

  scale = nchar(as.character(sotu_data$tokens$token))
  scale[scale>6] = scale[scale>6] +20
  scale = rescale_var(sqrt(scale), -1, 1)
  scale[abs(scale) < 0.5] = NA
  url = colorscaled_reader(sotu_data$tokens, value = scale, meta=sotu_data$meta)
  view_reader(url)
  #browseURL(url)

  ## topics
  category = match(sotu_data$tokens$pos, c('N','M','V'))
  url = categorical_reader(sotu_data$tokens, category=category, labels=c('N','M','V'), meta=sotu_data$meta)
  view_reader(url)

  library(rsyntax)
  tokens = read.csv('~/projects/bron_extractie_demo/tokens.csv')
  tokens = annotate(tokens, alpino_quote_queries(), column='quotes')
  url = categorical_reader(tokens, tokens$quotes)
  view_reader(url)

  browseURL(url)
})

