paste_na_omit <- function(..., sep='') {
  ## hack to skip over NA's with paste (only sep). Elegant solution pending
  d = list(...)
  if (length(d) == 1) return(as.character(d[[1]]))
  for (name in names(d)) {
    d[[name]] = as.character(d[[name]])
    d[[name]][is.na(d[[name]])] = '#=NA=#'
  }
  d = do.call(stringi::stri_paste, args = c(d, sep=sep))
  d = gsub(sprintf('%s#=NA=#', sep, sep), '', d)
  d = gsub(sprintf('#=NA=#%s', sep, sep), '', d)
  d
}
