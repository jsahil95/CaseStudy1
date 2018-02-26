# File containing all functions
# Include source(func.R) in other files to use these functions

library(stringr)

contains_word <- function(i, word) {
  word_count <- sum(str_count(training.emails[[i]], word))
  return (word_count > 0)
}