# Set working directory
wd <- "/Users/chunheisiu/Dropbox/Documents/USF/2018_Spring/MATH_373/CaseStudy"

# Set directory to folder containing training data
training.dir <- paste(wd, "/CaseStudy1/SPAMData/TRAINING", sep = "")
setwd(training.dir)

# Load training and test files from the SPAM data set
training.names <- list.files(training.dir)

# Load each email using the above file.names
training.emails <- list()
for (i in 1:length(training.names)){
  training.emails[[i]] <- readLines(training.names[i])
}

# Load stringr package for easy text analysis
library(stringr)

# Initialize binary matrix
x <- data.frame()

# Function to check if an email contains spam word
contains_word <- function(i, word) {
  word_count <- sum(str_count(training.emails[[i]], word))
  return (word_count > 0)
}

# Check conditions for each email in the sample
for (i in 1:length(training.names)){
  x[i, 1] <- contains_word(i, "explosive")
}

#You will use the above type of search to create a matrix of 20 or more binary
#variables to build into your classifier

# Load training labels
labels <- read.table("/Users/jdwilson4/Desktop/CSDMC2010_SPAM/SPAMTrain.label")