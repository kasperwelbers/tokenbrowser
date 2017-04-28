test_that("tokenbrowser", {
  library(tokenbrowser)

  ## for debugging, source everything
  ## tokenbrowser:::sourceall()

  data("sotu")
  url = create_reader(tokens, meta)
  #browseURL(url)

  highlight = nchar(as.character(tokens$token))
  highlight = highlight / max(highlight)
  highlight[highlight < 0.4] = NA
  url = highlighted_reader(tokens, value = highlight, meta)
  #browseURL(url)

  scale = highlight*2 - 1
  scale[abs(scale) < 0.4] = NA
  url = colorscaled_reader(tokens, value = scale, meta=meta)
  #browseURL(url)
})

