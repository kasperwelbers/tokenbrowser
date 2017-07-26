test_that("tokenbrowser", {
  library(tokenbrowser)

  ## for debugging, source everything
  ## tokenbrowser:::sourceall()

  d = sotu_data
  url = create_reader(d$tokens, d$meta)
  #browseURL(url)

  highlight = nchar(as.character(d$tokens$token))
  highlight = highlight / max(highlight)
  highlight[highlight < 0.4] = NA
  url = highlighted_reader(d$tokens, value = highlight, d$meta)
  #browseURL(url)

  scale = nchar(as.character(d$tokens$token))
  scale = scale / median(scale)
  scale = rescale_var(scale, -1, 1)
  scale[abs(scale) < 0.6] = NA
  url = colorscaled_reader(d$tokens, value = scale, meta=d$meta)
  #browseURL(url)

  ## topics
  category = match(d$tokens$pos, c('N','M','V'))
  url = categorical_reader(d$tokens, category=category, meta=d$meta)
  #browseURL(url)
})

