library("lexicon")
library("stringr")

clean_data <- function(input) {
  profane_words <- profanity_alvarez # Lexicon package
  special_characters <- c(".", "+", "*", "?", "^", "$", "(", ")", 
                          "[", "]", "{", "}", "|", "\\")
  patterns <- paste0("\\", special_characters)
  replacements <- paste0("\\\\", special_characters)
  
  # Pattern - Replacement maps
  pat_rep_map <- replacements
  names(pat_rep_map) <- patterns
  
  remove_pattern <- str_replace_all(profane_words, pat_rep_map) %>%
    paste(collapse = "|")
  
  str_to_lower(input) %>%
    str_remove_all(remove_pattern) # remove profane words
}