library(readr) # read_lines, write_lines
library(R.utils) # countLines

text_sampling <- function(input_file_name, output_file_name) {
  set.seed(1234)
  
  lines <- read_lines(input_file_name)
  
  n_lines <- as.numeric(countLines(input_file_name))
  
  size <- ifelse(n_lines/100 > 10000, n_lines/100, 10000)
  
  selected_lines <- sample(n_lines, size = size, replace = FALSE)
  subset_text <- lines[selected_lines]
  
  write_lines(subset_text, output_file_name)
}