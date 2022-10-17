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

# Text, that is classified as fake, without stop words
tidy_text_fake <- unnest_tokens(test_fake, 'splitted', 'Body', token="words", to_lower = TRUE) %>% filter(!splitted %in% splitted_stop_words)

tidy_text_fake %>% count(splitted,sort=TRUE)

# Plot of words in fake news
frequent_terms <- freq_terms(tidy_text_fake[4], 20)
plot(frequent_terms, xlab=c(0,20), ylab=c(0,20))


# Text, that is classified as credible, without stop words
tidy_text_credible <- unnest_tokens(test_credible, 'splitted', 'Body', token="words", to_lower = TRUE) %>%
  filter(!splitted %in% splitted_stop_words)

tidy_text_credible %>% count(splitted,sort=TRUE)
counters <- tidy_text_credible %>% count(splitted,sort=TRUE)
print(counters[1])
print(which(counters[1] == "can")[1])
print(counters[3,2])

summa <- sum(counters[2])
print(summa)

unique_values <- nrow(counters)

credible_prob <- counters

for (x in 1:unique_values){
  credible_prob[x, 2] <- (credible_prob[x, 2] + 1) / (summa + unique_values)
}



credible_prob <= replace(counters, counters$n, cou)

line <- "Today we have seen miracle in the daylight. They said 'It was beautiful'."
line_counter <- unnest_tokens(line, token="words") %>% filter(!splitted %in% splitted_stop_words)


# Plot of words in credible news
frequent_terms <- freq_terms(tidy_text_credible[4], 20)
plot(frequent_terms, main="NAME", xlim=c(0,20), ylim=c(0,20))

naiveBayes <- setRefClass("naiveBayes",
                          
  # here it would be wise to have some vars to store intermediate result
  # frequency dict etc. Though pay attention to bag of wards! 
  fields = list(
    counter_fake <- naiveBayes %>% 
  ),
  methods = list(
    # prepare your training data as X - bag of words for each of your
    # messages and corresponding label for the message encoded as 0 or 1 
    # (binary classification task)
    fit = function(X, y)
    {
      # TODO
    },
    
    # return prediction for a single message 
    predict = function(message)
    {
      # TODO
    },
    
    # score you test set so to get the understanding how well you model
    # works.
    # look at f1 score or precision and recall
    # visualize them 
    # try how well your model generalizes to real world data! 
    score = function(X_test, y_test)
    {
      # TODO
    }
))

model = naiveBayes()
model$fit()

