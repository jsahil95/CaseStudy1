#First, set the directory to match the local directory for the TRAINING folder
#on your computer. Below, I give an example on my own personal computer.

training.directory <- "/Users/chunheisiu/Dropbox/Documents/USF/2018_Spring/MATH_373/CaseStudy/SPAMData/TRAINING"

#Now load training and test files from the SPAM data set
training.names <- list.files(training.directory)

#next, load each email using the above file.names
training.emails <- list()
setwd(training.directory)

for (i in 1:length(training.names)){
  training.emails[[i]] <- readLines(training.names[i])
}

#install and load package for easy text analysis
library(stringr)

x <- data.frame()

# Function to check if an email contains spam word
contains_word <- function(i, word) {
  word_count <- sum(str_count(training.emails[[i]], word))
  return (word_count > 0)
}

for (i in 1:length(training.names)){
  x[i, 1] <- contains_word(i, "explosive")
}


#You will use the above type of search to create a matrix of 20 or more binary
#variables to build into your classifier

#Training labels
labels <- read.table("/Users/jdwilson4/Desktop/CSDMC2010_SPAM/SPAMTrain.label")

#Now you can run with your Naive Bayes classifier!