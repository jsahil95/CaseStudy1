# Set working directory
wd <- "/Users/chunheisiu/Dropbox/Documents/USF/2018_Spring/MATH_373/CaseStudy/CaseStudy1"

# Set directory to folder containing training data
training.dir <- paste(wd, "/SPAMData/TRAINING", sep = "")
setwd(training.dir)

# Load training and test files from the SPAM data set
training.names <- list.files(training.dir)

# Load each email using the above file.names
training.emails <- list()
for (i in 1:length(training.names)){
  training.emails[[i]] <- readLines(training.names[i], encoding = "latin1")
}

# Load stringr package for easy text analysis
library(stringr)

# Initialize binary matrix
x <- data.frame()
spam_word <- t(read.csv(paste(wd, "/Spammy_wordslist.csv", sep = ""), header = F))

# Function to check if a string contains spam word
contains_word <- function(string, word) {
  word_count <- sum(str_count(string, word))
  return (word_count > 0)
}

# Function to check if an email contains spammy word in a known word list
# Return true if there less than two spammy words appear in the email, false otherwise
check_spam_word <- function(i) {
  spam_count <- double()
  for (j in 1:length(spam_word)) {
    spam_count[j] <- sum(str_count(training.emails[[i]], spam_word[j]))
  }
  return (sum(spam_count >= 1) < 2)
}

# Function to check if an email contains reply from previous email
# Return true if there is at least one reply, false otherwise
check_reply <- function(i) {
  for (j in 1:length(training.emails[[i]])) {
    if (startsWith(training.emails[[i]][j], ">")) {
      return (T)
    }
  }
  return (F)
}

# Function to count the number of images in an email
count_image <- function(i) {
  image_count <- sum(str_count(training.emails[[i]], "<IMG|<img"))
  return (image_count)
}

# Function to count the number of images in an email
# Return true if there are less than 5 images, false otherwise
check_image <- function(i) {
  return (count_image(i) < 5)
}

# Function to extract the subject from an email
extract_subject <- function(i) {
  condition <- !is.na(str_extract(training.emails[[i]], "^Subject:.+$"))
  index <- which(condition == T)
  if (length(index) == 0L) {
    return ("")
  }
  subject_line <- training.emails[[i]][(min(index))]
  return (substring(subject_line, 10))
}

# Function to count the number of punctuation in an email 
count_punc <- function(i) {
  subject <- extract_subject(i)
  return (sum(str_count(subject, "[:punct:]")))
}

# Function to count the number of punctuation in an email
# Return true if there are less than 2 punctuation, false otherwise
check_punc <- function(i) {
  return (count_punc(i) < 2)
}

# Function to check whether the subject of an email contains the word "spam"
# Return true if the word exists in the subject, false otherwise
check_spamword <- function(i) {
  return (contains_word(extract_subject(i), "spam") | contains_word(extract_subject(i), "SPAM"))
}

# Function to extract the reply-to from an email
extract_replyto <- function(i) {
  condition <- !is.na(str_extract(training.emails[[i]], "^Reply-To:.+$"))
  index <- which(condition == T)
  if (length(index) == 0L) {
    return ("")
  }
  subject_line <- training.emails[[i]][(min(index))]
  return (substring(subject_line, 11))
}

# Function to check whether there is a reply-to address
# Return true if a reply-to address does not exist, false otherwise
check_replyto <- function(i) {
  return (nchar(extract_replyto(i)) == 0)
}

# Check conditions for each email in the sample
for (i in 1:length(training.names)){
  x[i, 1] <- check_spam_word(i) # Check spammy words
  x[i, 2] <- check_reply(i) # Check replies
  x[i, 3] <- check_image(i) # Check number of images
  x[i, 4] <- check_punc(i) # Check subject number of punctuations
  x[i, 5] <- check_spamword(i) # Check whether subject contains "spam"
  x[i, 6] <- check_replyto(i) # Check reply-to address
}

#You will use the above type of search to create a matrix of 20 or more binary
#variables to build into your classifier

# Load training labels
labels <- read.table(paste(wd, "/SPAMData/SPAMTrain.label", sep = ""))

