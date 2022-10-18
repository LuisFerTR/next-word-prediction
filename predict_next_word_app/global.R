library(shiny)
library(tidyverse)
library("sbo")
library("devtools")
source_url("https://raw.githubusercontent.com/LuisFerTR/next-word-prediction/main/predict_next_word_app/clean_data.R")

# Load prediction table
data_url <- "https://github.com/LuisFerTR/next-word-prediction/raw/main/predict_next_word_app/t4.RData"
load(url(data_url))

# Create prediction model
p4 <- sbo_predictor(t4) 