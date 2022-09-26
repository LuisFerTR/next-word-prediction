library(hunspell)
library(sentimentr)
library(tidyverse)
library(tidytext)


ngram_tokenizer <- function(text_lines, n = 1) {
  text_lines_tibble <- tibble(line = 1:length(text_lines), text = text_lines)
  
  # Using sentimentr
  sentences <- get_sentences(text_lines)
  # names: neutral, profanity, sentence
  profanity_terms <- extract_profanity_terms(sentences) 
  profane_words <- unique(unlist(profanity_terms$profanity))
  
  if (n == 1) {
    ngram <- text_lines_tibble %>%
      unnest_tokens(token, text, strip_punct = TRUE, strip_numeric = TRUE) %>%
      filter(!is.na(token)) %>%
      mutate(word = token) %>%
      anti_join(stop_words, by=c("token"="word")) # Remove stop words
    
    # Filter profanity
    ngram <- ngram %>%
      filter(!token %in% profane_words)
    
    # Remove misspelled words
    ngram <- ngram %>% filter(hunspell_check(word))
    ngram <- ngram$token
  }
  
  else if (n == 2) {
    ngram <- text_lines_tibble %>%
      unnest_tokens(bigram, text, 
                    token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>%
      separate(bigram, c("word1", "word2"), # Split bigram into two words
               extra="drop", # Drop any extra values without a warning.
               fill="right") %>% # fill with missing values on the right 
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word1)) %>% # remove numbers
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word2)) %>%
      filter(!word1 %in% profane_words) %>% # remove profane words
      filter(!word2 %in% profane_words) %>%
      filter(!is.na(word1)) %>%
      filter(!is.na(word2)) %>%
      filter(hunspell_check(word1)) %>% # remove misspelled words
      filter(hunspell_check(word2)) %>%
      unite(bigram, word1, word2, sep = " ") %>% # Merge two words into a bigram
      filter(!grepl('^ ', bigram))
      
    ngram <- ngram$bigram
  }
  
  else if (n == 3) {
    ngram <- text_lines_tibble %>%
      unnest_tokens(trigram, text, 
                    token = "ngrams", n = 3) %>%
      filter(!is.na(trigram)) %>%
      separate(trigram, c("word1", "word2", "word3"), # Split trigram
               extra="drop", # Drop any extra values without a warning.
               fill="right") %>% # fill with missing values on the right 
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word3 %in% stop_words$word) %>%
      filter(!word1 %in% profane_words) %>% # remove profane words
      filter(!word2 %in% profane_words) %>%
      filter(!word3 %in% profane_words) %>%
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word1)) %>% # remove numbers
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word2)) %>%
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word3)) %>%
      filter(!is.na(word1)) %>%
      filter(!is.na(word2)) %>%
      filter(!is.na(word3)) %>%
      filter(hunspell_check(word1)) %>% # remove misspelled words
      filter(hunspell_check(word2)) %>%
      filter(hunspell_check(word3)) %>%
      unite(trigram, word1, word2, word3, sep = " ") %>% # Merge three words into a trigram
      filter(!grepl('^ ', trigram))
  
    ngram <- ngram$trigram  
  }
  
  ngram
}

ngram_predict <- function(text) {
  tokens <- tibble(line = 1:length(text), text = text) %>%
    unnest_tokens(token, text, strip_punct = TRUE, strip_numeric = TRUE) %>%
    filter(!is.na(token))
    
  tokens <- unlist(tokens$token) 
  tokens[!(tokens %in% dict)] <- "UNK" # mark the ones not in dictionary as UNK
  L <- length(tokens)
  
  if(L==0) { 
    w1 <- "UNK"
    w2 <- "UNK"
  }
  
  if(L==1) { 
    w1 <- "UNK"
    w2 <- tokens[L] 
  }
  
  if(L>1) { 
    w1 <- tokens[L-1]
    w2 <- tokens[L] 
  }
  
  w3 <- dict
  
  # Recursion case by case
  words123 <- str_c(w1, w2, w3, sep = " ")
  id1 <- (words123 %in% model$trigram$trigram) # Trigram known
  words12 <- str_c(w1, w2, sep = " ")
  words23 <- str_c(w2, w3, sep = " ")
  
  # Trigram unknown backoff to bigram
  bigram_known <- model$bigram$bigram
  # Words 1 and 2 are known
  id2 <- (!id1) & (words12 %in% bigram_known) & (words23 %in% bigram_known)
  # Word 1 is known but word 2 is not
  id3 <- (!id1) & (words12 %in% bigram_known) & (!(words23 %in% bigram_known))
  # Word 1 is not known but word 2 is it
  id4 <- (!id1) & (!(words12 %in% bigram_known)) & (words23 %in% bigram_known)
  # Neither word1 nor word2 are known
  id5 <- (!id1) & (!(words12 %in% bigram_known)) & (!(words23 %in% bigram_known))
  
  a <- matrix(NA,length(words123),1)
  if(sum(id1)>0) { a[id1] <- as.numeric(unlist(model$trigram[words123[id1], "MLE"])) }
  
  if(sum(id2)>0) { 
    a[id2] <- as.numeric(unlist(model$bigram[model$bigram$bigram == words12, "MLE"])) + 
    as.numeric(unlist(model$bigram[model$bigram$bigram %in% words23[id2], "MLE"])) + 
    log(2*0.4) 
  }
  
  if(sum(id3)>0) { 
    a[id3] <- as.numeric(unlist(model$bigram[model$bigram$bigram == words12, "MLE"])) + 
    as.numeric(unlist(model$unigram[model$unigram$word == w2, "MLE"])) + 
    as.numeric(unlist(model$unigram[model$unigram$word %in% w3[id3], "MLE"])) + 
    log(3*0.4) 
  }
  
  if(sum(id4)>0) { 
    a[id4] <- as.numeric(unlist(model$unigram[model$unigram$word == w1, "MLE"])) + 
      as.numeric(unlist(model$unigram[model$unigram$word == w2, "MLE"])) + 
      as.numeric(unlist(model$bigram[model$bigram$bigram %in% words23[id4], "MLE"])) + 
      log(3*0.4) 
  }
  
  if(sum(id5)>0) { 
    a[id5] <- as.numeric(unlist(model$unigram[model$unigram$word == w1, "MLE"])) + 
      as.numeric(unlist(model$unigram[model$unigram$word == w2, "MLE"])) + 
      as.numeric(unlist(model$unigram[model$unigram$word %in% w3[id5], "MLE"])) + 
      log(4*0.4) 
  }
  
  pred_word <- w3[which.max(a)]
}