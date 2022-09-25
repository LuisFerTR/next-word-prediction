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
      mutate(word = token) #%>%
      #anti_join(stop_words, by=c("token"="word")) # Remove stop words
    
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
      #filter(!word1 %in% stop_words$word) %>%
      #filter(!word2 %in% stop_words$word) %>%
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word1)) %>% # remove numbers
      filter(!grepl("^(\\d+(,\\d+)*(.\\d+))|(\\d+)$", word2)) %>%
      filter(!word1 %in% profane_words) %>% # remove profane words
      filter(!word2 %in% profane_words) %>%
      filter(!is.na(word1)) %>%
      filter(!is.na(word2)) %>%
      filter(hunspell_check(word1)) %>% # remove misspelled words
      filter(hunspell_check(word2)) %>%
      unite(bigram, word1, word2, sep = " ") # Merge two words into a bigram
      
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
      #filter(!word1 %in% stop_words$word) %>%
      #filter(!word2 %in% stop_words$word) %>%
      #filter(!word3 %in% stop_words$word) %>%
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
      unite(trigram, word1, word2, word3, sep = " ") # Merge three words into a trigram
  
    ngram <- ngram$trigram  
  }
  
  ngram
}