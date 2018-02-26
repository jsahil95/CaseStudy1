library(stringr)

contains_word <- function(i, word) {
  word_count <- sum(str_count(training.emails[[i]], word))
  return (word_count > 0)
}