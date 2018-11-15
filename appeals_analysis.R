library(tidytext)
library(tidyverse)
library(NLP)
library(tm)
library(stringr)
library(readr)
library(dplyr)
library(ROCR)

##### A
  ## create appeals.data tibble
  appeals.data <- read_tsv(file = "recent_opinions.tsv")
  appeals.data <- as_tibble(appeals.data) %>% mutate(opinion_ID = row_number())
  appeals.data$opinion_ID <- as.character(appeals.data$opinion_ID)

  ## create a custom dictionary of stop words 
  data(stop_words)
  custom_stopwords <- read.csv(file = "custom_words.txt", header=F) %>% rename(word=V1) %>% mutate(lexicon="custom") 
  all_stopwords <- rbind(stop_words, custom_stopwords)

  rm(custom_stopwords,stop_words)

  
#### B 

#### a)
  ##unnest tokens in appeals.data and remove custom stop_words
  unnested.data <- appeals.data %>% unnest_tokens(word,text) %>% anti_join(all_stopwords)

  ## top 10 common words in the entire corpus 
  unnested.data%>% count(word) %>% 
  arrange(desc(n)) %>% slice(1:10)
  
  ## top 10 common words in the 5th circuit court
  unnested.data%>% filter(circuit == "fifth") %>% 
    count(word) %>% arrange(desc(n)) %>% slice(1:10)
  
  ## top 10 common words in the 9th circuit court
  unnested.data %>% filter(circuit == "ninth") %>% 
    count(word) %>% arrange(desc(n)) %>% slice(1:10)
  
#### b) Build a document-term tibble, where each row represents an opinion (there should be 16389 rows). 
  #There should be 102 columns: the circuit (code “fifth” as 1, and “ninth” as 0), the opinion_id, and 
  #the number of occurrences of the 100 most common words in the corpus (that are not stop words). 
  
  ## get top 100 words
  top100words <- unnested.data %>% count(word, sort=T) %>% 
    arrange(desc(n)) %>% slice(1:100) %>% select(word)
  
  ## count word for each document
  pre.dtm <- unnested.data %>%
    count(year,circuit,opinion_ID,word)
  
  ## document term matrix
  dtm <- pre.dtm %>%
    cast_dtm(opinion_ID,word,n)
   
  ## subet for top 100 words 
  dtm<-dtm[,c(top100words$word)]
  dim(dtm) #confirm that number of rows is 16389 and the number of columns is top 100 words 
  
  ## convert dtm to matrix then to tibble 
  dtm <- as.matrix(dtm)
  dtm <- as_tibble(dtm, rownames = "opinion_ID")
  
  circuit_id <- data.frame(opinion_ID = appeals.data$opinion_ID, circuit = appeals.data$circuit)
  circuit_id$opinion_ID <- as.character(circuit_id$opinion_ID)
  
  dtm <- left_join(dtm, circuit_id) #bring in circuit id 

  dim(dtm) #confirms 16389 rows and 102 columns 
  
  ## code “fifth” as 1, and “ninth” as 0
  levels(dtm$circuit) <- c(1,0)
  
  rm(circuit_id, pre.dtm)
  
  ## shuffle the raws
  dtm <- dtm[sample(nrow(dtm)),]
  
  ## split train and test set 
  set.seed(1234)
  dtm <- dtm %>% slice(sample(1:n())) #shuffle data
  split_size = floor(nrow(dtm)/2) #splits size 
  train <- dtm %>% slice(1:split_size) #first half 
  test <- dtm %>% slice(split_size+1:n())

  ## very important or else it wont work!
  train$opinion_ID <- as.numeric(train$opinion_ID)
  test$opinion_ID <- as.numeric(test$opinion_ID)
  train$circuit <- as.numeric(as.character(train$circuit))
  test$circuit <- as.numeric(as.character(test$circuit))
  
#### c)
  ##Fit a logistic regression model on the training set that predicts the circuit as a function of all
  ##other predictors. If you got warning messages, what do they say? 
  
  word_model <- glm(circuit ~., data=train, family="binomial")

  ##compute auc
  test$predicted.probability <- predict(word_model, newdata = test, type='response') 
  test.pred <- prediction(test$predicted.probability, test$circuit)
  test.perf <- performance(test.pred, "auc")
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 

  ##### the auc score is  99.78825 

#### d) Drop the predictor referred to in part c) above, and refit your logistic regression model on the training set.
  ##What is your new AUC on the test set? 
  ##What are the five smallest and five largest coefficients in this model? 
  ##Give a precise interpretation of the largest model coefficient
        
  set.seed(1234)
  dtm <- dtm %>% slice(sample(1:n())) #shuffle data
  split_size = floor(nrow(dtm)/2) #splits size 
  train <- dtm %>% slice(1:split_size) #first half 
  test <- dtm %>% slice(split_size+1:n())
  
  #very important or else it wont work!
  train$opinion_ID <- as.numeric(train$opinion_ID)
  test$opinion_ID <- as.numeric(test$opinion_ID)
  train$circuit <- as.numeric(as.character(train$circuit))
  test$circuit <- as.numeric(as.character(test$circuit))
  
  train <- train[-1]
  test  <- test[-1]
        
  word_model <- glm(circuit ~.,data=train, family="binomial")
  summary(word_model)
        
  ##compute auc
  test$predicted.probability <- predict(word_model,newdata=test,type='response')
  test.pred <- prediction(test$predicted.probability, test$circuit)
  test.perf <- performance(test.pred, "auc")
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
 
  ##### the auc score is  95.8521

  ##calculate the five smallest and five largest coefficients 
  word_coef <- summary(word_model)[["coefficients"]]
  word_coef[order(word_coef[ , 1]), ]   
  
##### C
#### a)
  ##unnest tokens and remove custom stop words
  unnest.bigram <- appeals.data %>% unnest_tokens(bigram, text, token = 'ngrams', n=2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% all_stopwords$word) %>%
    filter(!word2 %in% all_stopwords$word) %>% 
    unite(bigram, word1, word2, sep = " ")
  
#### b) build a document-term tibble with each row of an opinion and 102 columns
  ## get top 100 bigrams
  top100bigram <- unnest.bigram %>% count(bigram, sort=T) %>% 
    arrange(desc(n)) %>% slice(1:100) %>% select(bigram)
  
  ## count bigram for each document
  pre.dtm.bi <- unnest.bigram %>% count(year,circuit,opinion_ID,bigram)
  
  ## document term matrix
  dtm.bi <- pre.dtm.bi %>%
    cast_dtm(opinion_ID,bigram,n)
  
  ## subet for top 100 words 
  dtm.bi <- dtm.bi[,c(top100bigram$bigram)]
  dim(dtm.bi) #number of rows is 16371 and the number of columns is top 100 bigrams 
  
  ## convert dtm to matrix then to tibble 
  dtm.bi <- as.matrix(dtm.bi)
  dtm.bi <- as_tibble(dtm.bi, rownames = "opinion_ID")
  
  circuit_id <- data.frame(opinion_ID = appeals.data$opinion_ID, circuit = appeals.data$circuit)
  circuit_id$opinion_ID <- as.character(circuit_id$opinion_ID)
  
  dtm.bi <- right_join(dtm.bi, circuit_id, by = "opinion_ID") #bring in opinion id and circuit id
  dtm.bi[is.na(dtm.bi)] <- 0
  dim(dtm.bi) #confirms 16389 rows and 102 columns

  ## code “fifth” as 1, and “ninth” as 0
  levels(dtm.bi$circuit) <- c(1,0)
  rm(circuit_id, pre.dtm.bi)
  
  ## shuffle the raws
  dtm.bi <- dtm.bi[sample(nrow(dtm.bi)),]
  
  ## split train and test set 
  set.seed(1234)
  dtm.bi <- dtm.bi %>% slice(sample(1:n())) #shuffle data
  split_size = floor(nrow(dtm.bi)/2) #splits size 
  train <- dtm.bi %>% slice(1:split_size) #first half 
  test <- dtm.bi %>% slice(split_size+1:n())
  
  ## very important or else it wont work!
  train$opinion_ID <- as.numeric(train$opinion_ID)
  test$opinion_ID <- as.numeric(test$opinion_ID)
  train$circuit <- as.numeric(as.character(train$circuit))
  test$circuit <- as.numeric(as.character(test$circuit))
  
#### d) Drop the predictor referred to in part c) above, and refit your logistic regression model on the training set.
  ##What is your new AUC on the test set? 
  ##What are the five smallest and five largest coefficients in this model? 
  ## Give a precise interpretation of the largest model coefficient
  
  train <- train[-1]
  test  <- test[-1]
  
  bigram_model <- glm(circuit ~.,data=train, family="binomial")
  summary(bigram_model)
  
  ##compute auc
  test$predicted.probability <- predict(bigram_model,newdata=test,type='response')
  test.pred <- prediction(test$predicted.probability, test$circuit)
  test.perf <- performance(test.pred, "auc")
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
  ##### the auc score is  94.36046 
  
  ##calculate the five smallest and five largest coefficients 
  bigram_coef <- summary(bigram_model)[["coefficients"]]
  bigram_coef[order(bigram_coef[ , 1]), ]   
  
##### D
  #### b)  considering the top 100 bigrams and using the tf-idf value for each of the top 100 bigrams. 
  ##get top 100 bigrams
  top100bigram <- unnest.bigram %>% count(bigram, sort=T) %>% 
    arrange(desc(n)) %>% slice(1:100) %>% select(bigram)
  
  ##count bigram for each document using the tf_idf for each of the top 100 bigrams
  pre.dtm.bi.tf <- unnest.bigram %>% count(year,circuit,opinion_ID,bigram) %>% 
    bind_tf_idf(bigram, opinion_ID, n) %>% arrange(desc(n))
  
  ## compute the tf-idf value for each bigram using the entire corpus of data
  dtm.bi.tf <- pre.dtm.bi.tf %>%
    cast_dtm(opinion_ID,bigram,tf_idf)
  
  ## subet for top 100 words 
  dtm.bi.tf <- dtm.bi.tf[,c(top100bigram$bigram)]
  dim(dtm.bi.tf) #number of rows is 16371 and the number of columns is top 100 bigrams 
  
  ## convert dtm to matrix then to tibble 
  dtm.bi.tf <- as.matrix(dtm.bi.tf)
  dtm.bi.tf <- as_tibble(dtm.bi.tf, rownames = "opinion_ID")
  
  circuit_id <- data.frame(opinion_ID = appeals.data$opinion_ID, circuit = appeals.data$circuit)
  circuit_id$opinion_ID <- as.character(circuit_id$opinion_ID)
  
  dtm.bi.tf <- right_join(dtm.bi.tf, circuit_id, by = "opinion_ID") #bring in opinion id and circuit id 
  dtm.bi.tf[is.na(dtm.bi.tf)] <- 0
  dim(dtm.bi.tf) #confirms 16389 rows and 102 columns
  
  ## code “fifth” as 1, and “ninth” as 0
  levels(dtm.bi.tf$circuit) <- c(1,0)
  rm(circuit_id, pre.dtm.bi.tf)
  
  ## shuffle the raws
  dtm.bi.tf <- dtm.bi.tf[sample(nrow(dtm.bi.tf)),]
  
  ## split train and test set 
  set.seed(1234)
  dtm.bi.tf <- dtm.bi.tf %>% slice(sample(1:n())) #shuffle data
  split_size = floor(nrow(dtm.bi)/2) #splits size 
  train <- dtm.bi.tf %>% slice(1:split_size) #first half 
  test <- dtm.bi.tf %>% slice(split_size+1:n())
  
  ## very important or else it wont work!
  train$opinion_ID <- as.numeric(train$opinion_ID)
  test$opinion_ID <- as.numeric(test$opinion_ID)
  train$circuit <- as.numeric(as.character(train$circuit))
  test$circuit <- as.numeric(as.character(test$circuit))
  
#### d) Drop the predictor referred to in part c) above, and refit your logistic regression model on the training set.
  ##What is your new AUC on the test set? 
  ##What are the five smallest and five largest coefficients in this model? 
  ## Give a precise interpretation of the largest model coefficient
  
  train <- train[-1]
  test  <- test[-1]
  
  bigram_model_tf <- glm(circuit ~.,data=train, family="binomial")
  summary(bigram_model_tf)
  
  ##compute auc
  test$predicted.probability <- predict(bigram_model,newdata=test,type='response')
  test.pred <- prediction(test$predicted.probability, test$circuit)
  test.perf <- performance(test.pred, "auc")
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
  ##### the auc score is  94.1268  
  
  ##calculate the five smallest and five largest coefficients 
  bigramtf_coef <- summary(bigram_model_tf)[["coefficients"]]
  bigramtf_coef[order(bigramtf_coef[ , 1]), ]   
  
##### F
  ## Generate all trigrams, making sure you remove trigrams that contain a stopword as either of the three words in the trigram. 
  unnest.trigram <- appeals.data %>% 
    unnest_tokens(trigram, text, token = 'ngrams', n=3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
    filter(!word1 %in% all_stopwords$word) %>%
    filter(!word2 %in% all_stopwords$word) %>% 
    filter(!word3 %in% all_stopwords$word) %>% 
    unite(trigram, word1, word2, word3, sep = " ")
  
  ## Examine, for each circuit, the top 10 trigrams (by frequency in the corpus) that contain the word “supreme.” 
  
  ## top 10 common words in the entire corpus 
  unnest.trigram %>% 
    filter(str_detect(trigram, "supreme")) %>% count(trigram) %>% 
    arrange(desc(n)) %>% slice(1:10)
  
  ## top 10 common words in the 5th circuit court
  unnest.trigram %>% filter(circuit == "fifth") %>% 
    filter(str_detect(trigram, "supreme")) %>% count(trigram) %>% 
    arrange(desc(n)) %>% slice(1:10)
  
  ## top 10 common words in the 9th circuit court
  unnest.trigram %>% filter(circuit == "ninth") %>% 
    filter(str_detect(trigram, "supreme")) %>%
    count(trigram) %>% arrange(desc(n)) %>% slice(1:10)
  