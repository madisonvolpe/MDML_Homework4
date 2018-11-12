
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
    all_stopwords

##### B
##### a)  
    ## unnest tokens and remove custom stop words
    unnest.appeals.data <- appeals.data %>% unnest_tokens(word, text)
    stop.unnest.appeals.data <- unnest.appeals.data %>% anti_join(all_stopwords)

    ## top 10 common words in the entire corpus 
    top10entire <- stop.unnest.appeals.data%>% count(word) %>% 
      arrange(desc(n)) %>% slice(1:10)
    
    ## top 10 common words in the 5th circuit court
    top10fifth <- stop.unnest.appeals.data%>% filter(circuit == "fifth") %>% 
      count(word) %>% arrange(desc(n)) %>% slice(1:10)

    ## top 10 common words in the 9th circuit court
    top10ninth <- stop.unnest.appeals.data %>% filter(circuit == "ninth") %>% 
      count(word) %>% arrange(desc(n)) %>% slice(1:10)

##### b) build a document-term tibble with each row of an opinion and 102 columns
    ## rearrange the dataframe with a new column as the count of word frequency per opinion_ID
    data_word_dtm <- stop.unnest.appeals.data %>% 
      count(word, opinion_ID, circuit, sort=T) %>% arrange(opinion_ID)
      
    ## make a dataframe including the column of top 100 common words 
    top100words <- stop.unnest.appeals.data %>% count(word, sort=T) %>% 
      arrange(desc(n)) %>% slice(1:100) %>% select(word)
    
    ## make a document-term tibble
    word_dtm <- data_word_dtm %>% cast_dtm(opinion_ID, word, n)
    word_dtm_matrix <- as.matrix(word_dtm)
    word_dtm_tibble <- as.tibble(word_dtm_matrix)

    ## remove columns not relatd to the top 100 words
    word_dtm_100 <- word_dtm_tibble[, colnames(word_dtm_tibble) %in% top100words$word]
      
    ## add columns of circuit and opinion_ID, and replace the values in circuit column.
    word_dtm_100 <- word_dtm_100 %>% 
        mutate(opinion_ID = row_number(), circuit = appeals.data$circuit) %>%
        select(circuit, opinion_ID, everything())
    word_dtm_100$circuit[word_dtm_100$circuit=="fifth"] <- "1"
    word_dtm_100$circuit[word_dtm_100$circuit=="ninth"] <- "0"
    word_dtm_100$circuit <- as.numeric(word_dtm_100$circuit) # for fitting the glm model 

    ## randomly shffle the rows
    n <- nrow(word_dtm_100)
    shuffled_word_dtm_100 <- word_dtm_100[sample(n), ]
    
    ## split into a 50% training and 50% test set
    train_indices <- 1:round(0.5 * n)
    train <- shuffled_word_dtm_100[train_indices, ]
    test_indices <- (round(0.5 * n) + 1):n
    test <- shuffled_word_dtm_100[test_indices, ]
    
##### c) fit a logistic regression model & compute the AUC of this model 
    ## fit a logistic regression model on the training set 
    word_model <- glm(circuit ~ ., data=train, family=binomial())
    
    ##### Warning messages:
    ##### 1: glm.fit: algorithm did not converge 
    ##### 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
    
    ## compute the AUC of this model on the test set
    test$predicted.probability <- predict(word_model, newdata = test, type='response') 
    test.pred <- prediction(test$predicted.probability, test$circuit)
    test.perf <- performance(test.pred, "auc")
    cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
    
    ##### the auc score is  99.57218 
    
    ## calculate coefficients to decide which predictor may cause the strange result 
    summary_word_model <- data.frame(summary(word_model)$coefficients)
    summary_word_model[with(summary_word_model, order(-Estimate)), ]
    ##### intercept is too high as 1868.80708997. 
    ##### "opinion_ID" may increase the predicted probability of circuit: we decided to remove it.
    
##### d) 
    ## drop the predictor "opinion_ID" and refit the logistic regression model on training set
    word_model_new <- glm(circuit ~ . - opinion_ID, data=train, family=binomial())
    
    ## calculate the new AUC on the test set
    test$predicted.probability <- predict(word_model_new, newdata = test, type='response') 
    test.pred <- prediction(test$predicted.probability, test$circuit)
    test.perf <- performance(test.pred, "auc")
    cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
    
    ##### the auc score is  70.04131 
    
    ## the five smallest and five largest coefficients in the model
    summary_word_model_new <- data.frame(summary(word_model_new)$coefficients)
    summary_for_coefficients <- summary_word_model_new[with(summary_word_model_new, order(-Estimate)), ]
    head(summary_for_coefficients)
    tail(summary_for_coefficients)

##### C
##### a)
    ## unnest tokens and remove custom stop words
    unnest.bigram <- appeals.data %>% 
      unnest_tokens(bigram, text, token = 'ngrams', n=2)
    
    stop.unnest.bigram <- unnest.bigram %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% all_stopwords$word) %>%
      filter(!word2 %in% all_stopwords$word) %>% 
      unite(bigram, word1, word2, sep = " ")
    
    ## top 100 common bigram in the entire corpus 
    top100bigram <- stop.unnest.bigram %>% count(bigram, sort=T) %>% 
      arrange(desc(n)) %>% slice(1:100) %>% select(word) 
    
##### b) build a document-term tibble with each row of an opinion and 102 columns
    ## rearrange the dataframe with a new column as the count of word frequency per opinion_ID
    data_bigram_dtm <- stop.unnest.bigram %>% 
      count(bigram, opinion_ID, circuit, sort=T) %>% arrange(opinion_ID)
    
    ## make a document-term tibble
    bigram_dtm <- data_bigram_dtm %>% cast_dtm(opinion_ID, bigram, n)
    bigram_dtm_matrix <- as.matrix(bigram_dtm)
    bigram_dtm_tibble <- as.tibble(bigram_dtm_matrix)
    
    ## remove columns not relatd to the top 100 words
    bigram_dtm_100 <- bigram_dtm_tibble[, colnames(bigram_dtm_tibble) %in% top100bigram$bigram]
    
    ## add columns of circuit and opinion_ID, and replace the values in circuit column.
    bigram_dtm_100 <- bigram_dtm_100 %>% 
      mutate(opinion_ID = row_number(), circuit = appeals.data$circuit) %>%
      select(circuit, opinion_ID, everything())
    bigram_dtm_100$circuit[bigram_dtm_100$circuit=="fifth"] <- "1"
    bigram_dtm_100$circuit[bigram_dtm_100$circuit=="ninth"] <- "0"
    bigram_dtm_100$circuit <- as.numeric(bigram_dtm_100$circuit) # for fitting the glm model 
    
    ## randomly shffle the rows
    n2 <- nrow(bigram_dtm_100)
    shuffled_bigram_dtm_100 <- bigram_dtm_100[sample(n2), ]
    
    ## split into a 50% training and 50% test set
    train_bigram_indices <- 1:round(0.5 * n2)
    train_bigram <- shuffled_bigram_dtm_100[train_bigram_indices, ]
    test_bigram_indices <- (round(0.5 * n2) + 1):n2
    test_bigram <- shuffled_bigram_dtm_100[test_bigram_indices, ]

##### d) 
    ## drop the predictor "opinion_ID" and refit the logistic regression model on training set
    bigram_model <- glm(circuit ~ . - opinion_ID, data=train_bigram, family=binomial())
    
    ## calculate the new AUC on the test set
    test_bigram$predicted.probability <- predict(bigram_model, newdata = test_bigram, type='response') 
    test_bigram.pred <- prediction(test_bigram$predicted.probability, test_bigram$circuit)
    test_bigram.perf <- performance(test_bigram.pred, "auc")
    cat('the auc score is ', 100*test_bigram.perf@y.values[[1]], "\n") 
    
    ##### the auc score is  **** 
    
    ## the five smallest and five largest coefficients in the model
    summary_bigram_model <- data.frame(summary(bigram_model)$coefficients)
    summary_bigram_coefficients <- summary_bigram_model[with(summary_bigram_model, order(-Estimate)), ]
    head(summary_bigram_coefficients)
    tail(summary_bigram_coefficients)
    
##### D
##### b)    
    ## top 100 common bigram and using the tf_idf for each of the top 100 bigrams
    #bigramdata <- stop.unnest.bigram %>% count(opinion_ID, bigram) %>% arrange(desc(n))
    #top100bigram <- bigramdata %>% slice(1:100)    
    data_bigram_tfidf <- stop.unnest.bigram %>% 
      count(bigram, opinion_ID, circuit, sort=T) %>% 
      bind_tf_idf(bigram, opinion_ID, n) %>% arrange(desc(n))
    
    ## make a document-term tibble by using tf_idf
    bigram_dtm_tfidf <- data_bigram_tfidf %>% cast_dtm(opinion_ID, bigram, tf_idf)
    bigram_dtm_tfidf_matrix <- as.matrix(bigram_dtm_tfidf)
    bigram_dtm_tfidf_tibble <- as.tibble(bigram_dtm_tfidf_matrix)
    
    ## remove columns not relatd to the top 100 words
    bigram_dtm_tfidf_100 <- bigram_dtm_tfidf_tibble[, colnames(bigram_dtm_tfidf_tibble) %in% top100bigram$bigram]
    
    ## add columns of circuit and opinion_ID, and replace the values in circuit column.
    bigram_dtm_tfidf_100 <- bigram_dtm_tfidf_100 %>% 
      mutate(opinion_ID = row_number(), circuit = appeals.data$circuit) %>%
      select(circuit, opinion_ID, everything())
    bigram_dtm_tfidf_100$circuit[bigram_dtm_tfidf_100$circuit=="fifth"] <- "1"
    bigram_dtm_tfidf_100$circuit[bigram_dtm_tfidf_100$circuit=="ninth"] <- "0"
    bigram_dtm_tfidf_100$circuit <- as.numeric(bigram_dtm_tfidf_100$circuit) # for fitting the glm model 
    
    ## randomly shffle the rows
    n3 <- nrow(bigram_dtm_tfidf_100)
    shuffled_bigram_tfidf_100 <- bigram_dtm_tfidf_100[sample(n3), ]
    
    ## split into a 50% training and 50% test set
    train_indices_tfidf <- 1:round(0.5 * n3)
    train_bigram_tfidf <- shuffled_bigram_tfidf_100 [train_indices_tfidf, ]
    test_indices_tfidf <- (round(0.5 * n3) + 1):n3
    test_bigram_tfidf <- shuffled_bigram_tfidf_100 [test_indices_tfidf, ]

##### d) 
    ## drop the predictor "opinion_ID" and refit the logistic regression model on training set
    bigram_tfidf_model <- glm(circuit ~ . - opinion_ID, data=train_bigram_tfidf, family=binomial())
    
    ## calculate the new AUC on the test set
    test_bigram_tfidf$predicted.probability <- predict(bigram_model_tfidf, newdata = test_bigram_tfidf, type='response') 
    test_bigram_tfidf.pred <- prediction(test_bigram_tfidf$predicted.probability, test_bigram_tfidf$circuit)
    test_bigram_tfidf.perf <- performance(test_bigram_tfidf.pred, "auc")
    cat('the auc score is ', 100*test_bigram_tfidf.perf@y.values[[1]], "\n") 
    
    ##### the auc score is  **** 
    
    ## the five smallest and five largest coefficients in the model
    summary_bigram_tfidf_model <- data.frame(summary(bigram_tfidf_model)$coefficients)
    summary_bigram_tfidf_coefficients <- summary_bigram_tfidf_model[with(summary_bigram_tfidf_model, order(-Estimate)), ]
    head(summary_bigram_tfidf_coefficients)
    tail(summary_bigram_tfidf_coefficients)
    
    
##### F
    ## Generate all trigrams, making sure you remove trigrams that contain a stopword as either of the three words in the trigram. 
    unnest.trigram <- appeals.data %>% 
      unnest_tokens(trigram, text, token = 'ngrams', n=3)
    
    stop.unnest.trigram <- unnest.trigram %>%
      separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
      filter(!word1 %in% all_stopwords$word) %>%
      filter(!word2 %in% all_stopwords$word) %>% 
      filter(!word3 %in% all_stopwords$word) %>% unite(trigram, word1, word2, word3, sep = " ")
    stop.unnest.trigram

    ## Examine, for each circuit, the top 10 trigrams (by frequency in the corpus) that contain the word “supreme.” 
    top10trigram_fifth <- stop.unnest.trigram %>% filter(circuit == "fifth") %>% 
      filter(str_detect(trigram, "supreme")) %>% count(trigram) %>% arrange(desc(n)) %>% slice(1:10)
    
    top10trigram_ninth <- stop.unnest.trigram %>% filter(circuit == "ninth") %>% 
      filter(str_detect(trigram, "supreme")) %>% count(trigram) %>% arrange(desc(n)) %>% slice(1:10)
  
    