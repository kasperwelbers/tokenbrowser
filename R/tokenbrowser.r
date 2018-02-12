
create_topicbrowser <- function(tokens, word_assignments, term_topic, doc_topic, feature_col, meta=NULL, docs_per_topic=100, doc_select=c('top','random','first'), doc_col='doc_id', token_col='token') {
  doc_select = match.call(doc_select)
  topic_docs = choose_topic_docs(doc_topic, docs_per_topic, doc_select)

  tokens = match_word_assignments(tokens, word_assignments, topic_docs, doc_col, feature_col)

  create_browser(tokens, tokens$topic, meta, doc_col=doc_col, token_col=token_col)
  tokens = tokens[order(tokens$doc_id, tokens$token_index),]

  #for (ut in unique(topic)) {
  #  tokens
  #}
}

create_browser <- function(tokens, topic, meta, doc_col, token_col){
  NULL
}

match_word_assignments <- function(tokens, word_assignments, topic_docs, doc_col, feature_col) {
  doc_filter = unlist(topic_docs)
  tokens = tokens[tokens[[doc_col]] %in% doc_filter, c(doc_col, feature_col),]
  word_assignments = word_assignments[word_assignments[[doc_col]] %in% doc_filter,]
  tokens$topic = word_assignments$topic[match(paste(tokens[[doc_col]], tokens[[feature_col]]),
                                              paste(word_assignments[[doc_col]], word_assignments[[feature_col]]))]
}

choose_topic_docs <- function(doc_topic, docs_per_topic, doc_select) {
  best_doc_topic = apply(doc_topic, MARGIN = 1, which.max)
  best_doc_topic = data.frame(doc_id = rownames(doc_topic),
                              topic = best_doc_topic,
                              score = doc_topic[cbind(1:nrow(doc_topic), best_doc_topic)])

  if (doc_select == 'top') best_doc_topic = best_doc_topic[order(-best_doc_topic$score),]
  if (doc_select == 'random') best_doc_topic = best_doc_topic[sample(1:nrow(best_doc_topic)),]

  best_doc_topic = split(stats::setNames(best_doc_topic$score, nm = as.character(best_doc_topic$doc_id)),
                         best_doc_topic$topic)

  if (!is.na(docs_per_topic)) best_doc_topic = sapply(best_doc_topic, utils::head, n = docs_per_topic, simplify = F)
  best_doc_topic
}
