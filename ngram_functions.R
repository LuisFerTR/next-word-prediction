library(hunspell)
library(lexicon)
library(sentimentr)
library(tidyverse)
library(tidytext)


ngram_tokenizer <- function(text_lines, n = 1) {
  text_lines_tibble <- tibble(line = 1:length(text_lines), text = text_lines)
  
  profane_words <- profanity_alvarez # Lexicon package
  
  if (n == 1) {
    ngram <- text_lines_tibble %>%
      unnest_tokens(token, text, strip_punct = TRUE, strip_numeric = TRUE) %>%
      filter(!is.na(token)) %>%
      mutate(word = token) # %>%
      # filter(!word %in% stop_words$word) # Remove stop words
    
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
      separate(bigram, c("word1", "word2"), sep = " ", # Split bigram into two words
               extra="drop", # Drop any extra values without a warning.
               fill="right") %>% # fill with missing values on the right 
      # filter(!word1 %in% stop_words$word) %>%
      # filter(!word2 %in% stop_words$word) %>%
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word1)) %>% # remove numbers
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word2)) %>%
      filter(!word1 %in% profane_words) %>% # remove profane words
      filter(!word2 %in% profane_words) %>%
      filter(!is.na(word1)) %>%
      filter(!is.na(word2)) %>%
      filter(hunspell_check(word1)) %>% # remove misspelled words
      filter(hunspell_check(word2)) %>%
      unite(bigram, word1, word2, sep = " ") %>% # Merge two words into a bigram
      filter(!str_detect(bigram, "^ "))
      
    ngram <- ngram$bigram
  }
  
  else if (n == 3) {
    ngram <- text_lines_tibble %>%
      unnest_tokens(trigram, text, 
                    token = "ngrams", n = 3) %>%
      filter(!is.na(trigram)) %>%
      separate(trigram, c("word1", "word2", "word3"), sep = " ", # Split trigram
               extra="drop", # Drop any extra values without a warning.
               fill="right") %>% # fill with missing values on the right 
      # filter(!word1 %in% stop_words$word) %>%
      # filter(!word2 %in% stop_words$word) %>%
      # filter(!word3 %in% stop_words$word) %>%
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
      filter(!str_detect(trigram, "^ "))
  
    ngram <- ngram$trigram  
  }
  
  ngram
}

ngram_predict <- function(text) {
  # Tokenize text
  tokens <- tibble(line = 1:length(text), text = text) %>%
    unnest_tokens(token, text, strip_punct = TRUE, strip_numeric = TRUE) %>%
    filter(!is.na(token))
    
  # Extract tokens
  tokens <- unlist(tokens$token) 
  tokens[!(tokens %in% dict)] <- "UNK" # Mark the ones not in dictionary as UNK
  L <- length(tokens)
  
  # If text is empty then both words are unknown
  if(L==0) { 
    w1 <- "UNK"
    w2 <- "UNK"
  }
  
  # If text is only one word then the first trigram's word is unknown
  if(L==1) { 
    w1 <- "UNK"
    w2 <- tokens[L] 
  }
  
  # Take the last text's two words
  if(L>1) { 
    w1 <- tokens[L-1]
    w2 <- tokens[L] 
  }
  
  w3 <- dict
  
  # Recursion case by case
  words123 <- str_c(w1, w2, w3, sep = " ")
  words12 <- str_c(w1, w2, sep = " ")
  words23 <- str_c(w2, w3, sep = " ")
  all_words <- c(w3, "UNK")
  
  # Trigram known
  tri_known <- model$trigram %>% filter(trigram %in% words123)
  
  # Bigram words12 is known
  bi12 <- model$bigram %>% filter(bigram %in% words12)
  # Store all bigrams words23 known
  bi23 <- model$bigram %>% filter(bigram %in% words23)
  
  # Find w1, w2, w3 in unigram model
  u1 <- model$unigram %>% filter(word == w1)
  u2 <- model$unigram %>% filter(word == w2)
  u3 <- model$unigram %>% filter(word %in% all_words)
  
  predictions <- tibble(possible_word = character(), MLE = double())
  
  
  if (nrow(tri_known) > 0) { # Trigrams known
    w3tri_known <- str_split(tri_known$trigram, " ", simplify = TRUE)[,3]
    predictions = predictions %>% add_row(possible_word = w3tri_known,
                                          MLE = tri_known[["MLE"]])
    
  } 
  
  if (nrow(bi12) > 0) { # Words 1 and 2 are known
    # Second word of found bigram
    w3bi23 <- str_split(bi23$bigram, " ", simplify = TRUE)[,2]
    predictions = predictions %>% add_row(possible_word = w3bi23,
                            MLE = bi12[["MLE"]] + bi23[["MLE"]] + log(2*0.4))
    
  } 
  
  if (w1 %in% dict && !(w2 %in% dict)) { # Word 1 is known but word 2 is not
    predictions = predictions %>% add_row(possible_word = all_words,
                                          MLE = bi12[["MLE"]] + u2[["MLE"]] + 
                                            u3[["MLE"]] + log(3*0.4))
  
  } 
  
  if (!(w1 %in% dict) && w2 %in% dict) { # Word 1 is not known but word 2 is known
    # Second word of found bigram
    w3bi23 <- str_split(bi23$bigram, " ", simplify = TRUE)[,2]
    predictions = predictions %>% add_row(possible_word = w3bi23,
                            MLE = u1[["MLE"]] + u2[["MLE"]] + 
                              bi23[["MLE"]] + log(3*0.4))
  
  } 
  
  if (!(w1 %in% dict) && w2 %in% dict){ # Neither word1 nor word2 are known
    predictions = predictions %>% add_row(possible_word = all_words,
                            MLE = u1[["MLE"]] + u2[["MLE"]] + 
                              u3[["MLE"]] + log(4*0.4))
  }
  
  best_result <- predictions %>% slice(which.max(MLE))
  predict_word <- as.character(best_result[, "possible_word"])
  predict_word
  
  # TODO los if producen un error pero hay que revisar si hay opciones de bi23
}