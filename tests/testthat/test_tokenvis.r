test_that("tokenbrowser", {
  library(tokenbrowser)

  ## for debugging, source everything
  ## tokenbrowser:::sourceall()

  d = sotu_data
  url = create_reader(d$tokens, d$meta)
  #browseURL(url)

  highlight = nchar(as.character(sotu_data$tokens$token))
  highlight = highlight / max(highlight)
  highlight[highlight < 0.3] = NA
  url = highlighted_reader(sotu_data$tokens, value = highlight, sotu_data$meta)
  #browseURL(url)

  scale = nchar(as.character(sotu_data$tokens$token))
  scale = rescale_var(sqrt(scale), -1, 1)
  url = colorscaled_reader(sotu_data$tokens, value = scale, meta=sotu_data$meta)
  #browseURL(url)

  ## topics
  category = match(sotu_data$tokens$pos, c('N','M','V'))
  url = categorical_reader(sotu_data$tokens, category=category, meta=sotu_data$meta)
  #browseURL(url)
})

