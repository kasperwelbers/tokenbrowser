#' Convert tokens into full texts in an HTML file
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the browser as document meta
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param doc_nav   The name of a column (factor or character) in meta, used to create a navigation bar for selecting document groups.
#' @param token_nav Alternative to doc_nav, a column in the tokens.
#' @param filename  Name of the output file. Default is temp file
#' @param doc_width The width of the document text field
#' @param css_str   A character string, to be directly added to the css style header
#' @param header    Optionally, specify the header
#' @param n         If TRUE, report N in header
#' @param navfilter If TRUE (default) enable filtering with nav(igation) bar.
#' @param colors    Optionally, a vector with color names for the navigation bar. Length has to be identical to
#'                  unique non-NA items in the navigation.
#'
#' @return The name of the file where the browser is saved. Can be opened conveniently from within R using browseUrl()
#' @export
#'
#' @examples
#' \dontrun{
#' url = create_browser(d$tokens, d$meta, token_col = 'token', header = 'Speeches')
#' view_browser(url)   ## view browser in the Viewer
#' browseURL(url)     ## view browser in default webbrowser
#' }
create_browser <- function(tokens, meta=NULL, doc_col='doc_id', token_col='token', doc_nav=NULL, token_nav=NULL, filename=NULL, doc_width=750, css_str=NULL, header=NULL, n=TRUE, navfilter=TRUE, colors=NULL){
  docs = wrap_documents(tokens, meta, doc_col, token_col, nav=doc_nav, token_nav = token_nav)
  docstring = stringi::stri_paste(docs, collapse='\n\n')

  doc_ids = unique(tokens[[doc_col]])
  n_doc = length(doc_ids)

  nav = NULL
  if (!is.null(doc_nav)) {
    nav = if (methods::is(meta[[doc_nav]], 'factor')) levels(meta[[doc_nav]]) else unique(meta[[doc_nav]])
  }
  if (!is.null(token_nav)) {
    nav = if (methods::is(tokens[[token_nav]], 'factor')) levels(tokens[[token_nav]]) else unique(tokens[[token_nav]])
  }
  if (!is.null(nav)) nav = id_nav(nav, colors, navfilter)

  template = html_template('browser', doc_width=doc_width, css_str=css_str)
  template$header = gsub('$NAVIGATION$', if (is.null(nav)) '' else nav, template$header, fixed = T)

  if (n)
    template$header = gsub('$N$', sprintf('N = <ndoc>%s</ndoc>', n_doc), template$header, fixed = T)
  else
    template$header = gsub('$N$', '', template$header, fixed = T)

  if (!is.null(header))
    template$header = gsub('$HEADER$', header, template$header, fixed = T)
  else
    template$header = gsub('$HEADER$', '', template$header, fixed = T)

  save_html(docstring, template, filename)
}

#' Convert tokens into full texts in an HTML file with highlighted tokens
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param value     Either a logical vector or a numeric vector with
#'                  values between 0 and 1. If a logical vector is used, then tokens
#'                  with TRUE will be highlighted (with the color specified in pos_col).
#'                  If a numeric vector is used, the value determines the alpha (transparency),
#'                  with 0 being fully transparent and 1 being fully colored.
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the browser as document meta
#' @param col       The color used to highlight
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param doc_nav   The name of a column in meta, used to set a navigation tag
#' @param token_nav Alternative to doc_nav, a column in the tokens, used to set a navigation tag
#' @param filename  Name of the output file. Default is temp file
#' @param ...       Additional formatting arguments passed to create_browser()
#'
#' @return The name of the file where the browser is saved. Can be opened conveniently from within R using browseUrl()
#' @export
#'
#' @examples
#' \dontrun{
#' ## as an example, highlight words based on word length
#' highlight = nchar(as.character(sotu_data$tokens$token))
#' highlight = highlight / max(highlight)
#' highlight[highlight < 0.3] = NA
#' url = highlighted_browser(sotu_data$tokens, value = highlight, sotu_data$meta)
#' view_browser(url)   ## view browser in the Viewer
#' browseURL(url)     ## view browser in default webbrowser
#' }
highlighted_browser <- function(tokens, value, meta=NULL, col='yellow', doc_col='doc_id', token_col='token', doc_nav=NULL, token_nav=NULL, filename=NULL, ...){
  tokens[[token_col]] = highlight_tokens(tokens[[token_col]], value=value, col = col)
  create_browser(tokens, meta, doc_col, token_col, doc_nav=doc_nav, token_nav=token_nav, filename=filename, ...)
}

#' Convert tokens into full texts in an HTML file with color ramp highlighting
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param value     A numeric vector with values between -1 and 1. Determines the color
#'                  mixture of the scale colors specified in col_range
#' @param alpha     Optionally, the alpha (transparency) can be specified, with 0 being fully
#'                  transparent and 1 being fully colored. This can be a vector to specify a
#'                  different alpha for each value.
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the browser as document meta
#' @param col_range The color used to highlight
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param doc_nav   The name of a column in meta, used to set a navigation tag
#' @param token_nav Alternative to doc_nav, a column in the tokens, used to set a navigation tag
#' @param filename  Name of the output file. Default is temp file
#' @param ...       Additional formatting arguments passed to create_browser()
#'
#' @return The name of the file where the browser is saved. Can be opened conveniently from within R using browseUrl()
#' @export
#'
#' @examples
#' \dontrun{
#' ## as an example, scale word colors based on number of characters
#' scale = nchar(as.character(sotu_data$tokens$token))
#' scale[scale>6] = scale[scale>6] +20
#' scale = rescale_var(sqrt(scale), -1, 1)
#' scale[abs(scale) < 0.5] = NA
#' url = colorscaled_browser(sotu_data$tokens, value = scale, meta=sotu_data$meta)
#' view_browser(url)   ## view browser in the Viewer
#' browseURL(url)     ## view browser in default webbrowser
#' }
colorscaled_browser <- function(tokens, value, alpha=0.4, meta=NULL, col_range=c('red','blue'), doc_col='doc_id', token_col='token', doc_nav=NULL, token_nav=NULL, filename=NULL, ...){
  tokens[[token_col]] = colorscale_tokens(tokens[[token_col]], value=value, col_range = col_range, alpha=alpha)
  create_browser(tokens, meta, doc_col, token_col, doc_nav=doc_nav, token_nav=token_nav, filename=filename, ...)
}

#' Convert tokens into full texts in an HTML file with category highlighting
#'
#' @param tokens    A data.frame with a column for document ids (doc_col)
#'                  and a column for tokens (token_col)
#' @param category  Either a numeric vector with values representing categories, or a factor vector, in which case
#'                  the values are used as labels. If a numeric vector is used, the labels can also be specified in the labels argument
#' @param alpha     Optionally, the alpha (transparency) can be specified, with 0 being fully
#'                  transparent and 1 being fully colored. This can be a vector to specify a
#'                  different alpha for each value.
#' @param labels    A character vector giving names to the unique category values. If category is a factor vector, the factor levels are
#'                  used.
#' @param meta      A data.frame with a column for document_ids (doc_col). All other columns are added
#'                  to the browser as document meta.
#' @param meta_cat_col Optionally, the name of a (numeric) column in the meta data with the category id of the document.
#'                     The values need to correspond to the category values, and will be used to organize the document according
#'                     to the most strongly present category. If no column is specified, the most frequent category is used.
#' @param multi_cat Alternatively to meta_cat_col, do not select one category per document, but allow multiple categories.
#'                  Navigation filter will then look whether a category occurs at least once in a document
#' @param colors    A character vector with color names for unique values of the category argument. Has to be the same length
#'                  as unique(na.omit(category))
#' @param doc_col   The name of the document id column
#' @param token_col The name of the token column
#' @param filename  Name of the output file. Default is temp file
#' @param ...       Additional formatting arguments passed to create_browser()
#'
#' @return The name of the file where the browser is saved. Can be opened conveniently from within R using browseUrl()
#' @export
#'
#' @examples
#' \dontrun{
#' ## as an example, use part of speech tags as categories
#' category = match(sotu_data$tokens$pos, c('N','M','V'))
#'
#' ## this approach organizes documents by the most frequent category
#' url = highlighted_browser(sotu_data$tokens, value = highlight, sotu_data$meta)
#'
#' view_browser(url)   ## view browser in the Viewer
#' browseURL(url)     ## view browser in default webbrowser
#' }
categorical_browser <- function(tokens, category, alpha=0.3, labels=NULL, meta=NULL, meta_cat_col=NULL, multi_cat=F, colors=NULL, doc_col='doc_id', token_col='token', filename=NULL, ...){
  if (methods::is(category, 'character')) category = as.factor(category)
  if (methods::is(category, 'numeric') && is.null(labels)) labels = unique(category)
  if (methods::is(category, 'factor')) {
    if (is.null(labels)) labels = levels(category)
    category = as.numeric(category)
  }

  if (is.null(meta)) {
    meta = data.frame(doc_id = tokens[[doc_col]])
    colnames(meta) = doc_col
  }

  if (is.null(colors)) colors = grDevices::rainbow(length(unique(stats::na.omit(category))))

  if (multi_cat) {
    tokens[[token_col]] = category_highlight_tokens(tokens[[token_col]], category=category, labels=labels, alpha=alpha, colors = colors)
    tokens[['multi_cat']] = factor(category, labels=labels)
    create_browser(tokens, meta, doc_col, token_col, token_nav='multi_cat', filename= filename, colors=colors, ...)

  } else {
    if (is.null(meta_cat_col)) {
      meta$category = top_category(meta, tokens, category, doc_col)
      if (any(is.na(meta$category))) {
        meta$category[is.na(meta$category)] = max(meta$category, na.rm = T) + 1
        labels = c(labels, '...')
      }

      meta$category = factor(meta$category, labels=labels)
      meta_cat_col = 'category'
    } else {
      if (!meta_cat_col %in% colnames(meta)) stop(sprintf('The meta_cat_col ("%s") is not a column in meta', meta_cat_col))
    }

    tokens[[token_col]] = category_highlight_tokens(tokens[[token_col]], category=category, labels=labels, alpha=alpha, colors = colors)
    create_browser(tokens, meta, doc_col, token_col, doc_nav=meta_cat_col, filename= filename, colors=colors, ...)
  }
}



