#' Find similar words
#' 
#' @param scimeetr_data a scimeetr object
#' @param synonymous_to an character string that is an expression for which 
#' synonymous and similar expression are wanted. synonyms for each elements in
#' the vectors will be proposed.
#' @return a list of character string. In other words, a vector of suggestions.
#' @importFrom dplyr %>% 
get_synonyms <- function(scimeetr_data, synonymous_to, n_gram = c(1L,3L)) {
  tokens = scimeetr_data$com1$dfsci$AB %>%
    tolower %>%
    stringr::str_replace_all('[[:punct:] ]+', " ") %>%
    stringr::str_replace_all('[0-9]+', "") %>%
    stringr::str_split(pattern = ' ')
  it = text2vec::itoken(tokens)
  meaningless_word <- tm::stopwords("english")
  v = text2vec::create_vocabulary(it,
                        ngram = n_gram,
                        stopwords = meaningless_word) %>% 
    text2vec::prune_vocabulary(term_count_min = 5,
                     doc_count_min = 5)
  model2 = text2vec::Collocations$new(vocabulary = v, collocation_count_min = 40, pmi_min = 0)
  model2$partial_fit(it)
  it_phrases = model2$transform(it)
  v = text2vec::create_vocabulary(it_phrases,
                        stopwords = meaningless_word,
                        ngram = n_gram)
  model2 = text2vec::Collocations$new(vocabulary = v, collocation_count_min = 40, pmi_min = 0)
  model2$partial_fit(it_phrases)
  it_phrases = model2$transform(it_phrases)
  v = text2vec::create_vocabulary(it_phrases,
                        stopwords = meaningless_word,
                        ngram = n_gram)
  model2 = text2vec::Collocations$new(vocabulary = v, collocation_count_min = 40, pmi_min = 0)
  model2$partial_fit(it_phrases)
  it_phrases = model2$transform(it_phrases)
  v = text2vec::prune_vocabulary(v, term_count_min = 10)
  # create co-occurrence vectorizer
  vectorizer = text2vec::vocab_vectorizer(v)
  tcm = text2vec::create_tcm(it_phrases, vectorizer, skip_grams_window = 5L)
  glove = text2vec::GlobalVectors$new(word_vectors_size = 100, vocabulary = v, x_max = 10)
  wv_main = glove$fit_transform(tcm, n_iter = 100, convergence_tol = 0.0001)
  wv_context = glove$components
  word_vectors = wv_main + t(wv_context)
  suggestions <- NULL
  synonymous_to <- stringr::str_replace_all(synonymous_to, ' ', '_')
  for(i in 1:length(synonymous_to)){
    if(any(synonymous_to[i] == row.names(word_vectors))){
      cos_sim = text2vec::sim2(x = word_vectors, y = word_vectors[synonymous_to[i], , drop = FALSE], method = "cosine", norm = "l2")
      suggestions <- c(suggestions,
                       list(names(head(sort(rowSums(cos_sim), decreasing = TRUE), 25))))
      names(suggestions)[i] <- synonymous_to[i]
    } else {
      suggestions <- c(suggestions,
                       list("the expression was not in any of the abstracts"))
      names(suggestions)[i] <- synonymous_to[i]
    }

  }
  return(suggestions)
}