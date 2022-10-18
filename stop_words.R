library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)
library(qdap)

setwd("Desktop/P&S/Module1/LAB")
list.files(getwd())

test_path <- "test.csv"
train_path <- "train.csv"

stop_words <- read_file("stop_words.txt")
splitted_stop_words <- strsplit(stop_words, split='\n')
splitted_stop_words <- splitted_stop_words[[1]]

test <-  read.csv(file = test_path, stringsAsFactors = FALSE)

test_fake <- test[!(test$Label=="credible"),]
test_credible <- test[!(test$Label=="fake"),]

naiveBayes <- setRefClass("naiveBayes",
                          
# here it would be wise to have some vars to store intermediate result
# frequency dict etc. Though pay attention to bag of wards! 
fields = list(
  fake_data = "data.frame", credible_data = "data.frame", all_data = "data.frame",
  all_counter = "numeric", fake_counter = "numeric", credible_counter = "numeric"
),


methods = list(
  
  
  
  # prepare your training data as X - bag of words for each of your
  # messages and corresponding label for the message encoded as 0 or 1 
  # (binary classification task)
  fit = function(data_path)
  {
    # Stop words
    stop_words <- read_file("stop_words.txt")
    splitted_stop_words <- strsplit(stop_words, split='\n')
    splitted_stop_words <- splitted_stop_words[[1]]
    
    # Read file for training
    data_file <- read.csv(file = data_path, stringsAsFactors = FALSE)
    
    # Number of all words
    all_counter <<- sum((unnest_tokens(data_file, 'splitted', 'Body', token="words", to_lower = TRUE) %>% filter(!splitted %in% splitted_stop_words) %>% count(splitted,sort=TRUE))[2])
    
    # Create two data frames that have credible and fake news
    data_fake <- data_file[!(data_file$Label=="credible"),]
    data_credible <- data_file[!(data_file$Label=="fake"),]
    
    # Tokenise fake news and find probabilities of each word given that news are fake
    fake_tokens <- unnest_tokens(data_fake, 'splitted', 'Body', token="words", to_lower = TRUE) %>% filter(!splitted %in% splitted_stop_words)
    fake_counters <- fake_tokens %>% count(splitted,sort=TRUE)
    
    summa <- sum(fake_counters[2])
    fake_counter <<- summa
    
    unique_values <- nrow(fake_counters)
    
    fake_probability <- fake_counters
    
    for (x in 1:unique_values){
      fake_probability[x, 2] <- (fake_probability[x, 2] + 1) / (summa + unique_values)
    }
    
    #fake_data <<- fake_counters
    fake_data <<- fake_probability
    
    # Tokenise credible news and find probabilities of each word given that news are fake
    credible_tokens <- unnest_tokens(data_credible, 'splitted', 'Body', token="words", to_lower = TRUE) %>% filter(!splitted %in% splitted_stop_words)
    credible_counters <- credible_tokens %>% count(splitted,sort=TRUE)
    
    summa <- sum(credible_counters[2])
    credible_counter <<- summa
    
    unique_values <- nrow(credible_counters)
    
    credible_probability <- credible_counters
    
    for (x in 1:unique_values){
      credible_probability[x, 2] <- (credible_probability[x, 2] + 1) / (summa + unique_values)
    }
    
    #credible_data <<- credible_counters
    credible_data <<- credible_probability
  },
  
  # return prediction for a single message 
  predict = function(message)
  {
    fake_probability_overall = fake_counter / all_counter
    credible_probability_overall = credible_counter / all_counter
    
    # Take message and split it while deleting stop words from it
    message <- tolower(message)
    words <- strsplit(message , split = " ")[[1]] 
    words <- words[!words %in% splitted_stop_words]
    
    # Checks conditional probability for fake news
    fake_probability = 1
    
    for (word in words){
      if (word %in% fake_data$splitted & word %in% credible_data$splitted){
        fake_probability = fake_probability * fake_data[which(fake_data[1] == word), 2] / credible_data[which(credible_data[1] == word), 2] * 100
      }
      else {
        fake_probability = fake_probability * (1 / all_counter) * 100
      }
    }
    
    # Checks conditional probability for credible news
    credible_probability = 1
    
    for (word in words){
      if (word %in% credible_data$splitted & word %in% fake_data$splitted){
        credible_probability = credible_probability / fake_data[which(fake_data[1] == word), 2] * credible_data[which(credible_data[1] == word), 2] * 100
      }
      else {
        credible_probability = credible_probability * (1/all_counter) * 100
      }
    }
    
    # Returns if that message is fake (FALSE) or credible (TRUE)
    if (fake_probability >= credible_probability){
      return (FALSE)
    }
    else{
      return (TRUE)
    }
  },
  
  # score you test set so to get the understanding how well you model
  # works.
  # look at f1 score or precision and recall
  # visualize them 
  # try how well your model generalizes to real world data! 
  score = function(data_path)
  {
    # Read file with real world data
    data_file <- read.csv(file = data_path, stringsAsFactors = FALSE)
    
    # Initialize counters for guesses
    correct_guesses = 0
    wrong_guesses = 0
    
    # For loop through all rows in data frame of real world data
    for (row in 1:nrow(data_file)){
      print(row / nrow(data_file) * 100)
      print(" % of file parsed")
      result <- model$predict(data_file[row, 3])
      if ((result == TRUE & data_file[row, 4] == "credible") || (result == FALSE & data_file[row, 4] == "fake")){
        correct_guesses = correct_guesses + 1
      }
      else{
        wrong_guesses = wrong_guesses + 1
      }
    }
    print("Precision of model is:")
    print(correct_guesses/(correct_guesses + wrong_guesses))
  }
))

model = naiveBayes()
model$fit(train_path)
model$predict("Red Flag Warning: These California Wildfires Are ‘Among The Most Destructive Fire Events In US History’ And They Are About To Get Even Worse Wildfires Are ‘Among The Most Destructive Fire Events In US History’ And They Are About To Get Even Worse Wildfires Are ‘Among The Most Destructive Fire Events In US History’ And They Are About To Get Even Worse Wildfires Are ‘Among The Most Destructive Fire Events In US History’ And They Are About To Get Even Worse ")

model$score(test_path)
