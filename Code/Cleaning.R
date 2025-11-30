
          # Data Cleaning #

# packages #
install.packages (c(tidyverse, tokenizers, stringer, textstem, wordcloud, Mass, class, rpart, raondomForest, gbm, caret, broom))
library(tidyverse)
library(tokenizers)   # sentence tokenization
library(stringr)      # regex & str_squish
library(textstem)     # lemmatization (lemmatize_words)

# data sets # 
st <- read.csv("data/Fed_Scrape-2015-2023.csv", stringsAsFactors = FALSE)
sk <- read.csv("data/SP500.csv", stringsAsFactors = FALSE)

# FOMC data #

tokenize_and_clean <- function(text) {
  # split into sentences
  sentences <- tokenize_sentences(text)[[1]]
  
  cleaned_sentences <- lapply(sentences, function(sent) {
    cleaned <- as.character(sent)
    cleaned <- gsub("[^a-zA-Z\\s]", " ", cleaned)  # keep letters and spaces
    cleaned <- str_squish(cleaned)                 # remove extra spaces
    cleaned
  })
  
  paste(unlist(cleaned_sentences), collapse = " ")
}

# Apply to Text column
st_clean$Text <- vapply(
  st_clean$Text,
  tokenize_and_clean,
  FUN.VALUE = character(1)
)

# S&P 500 data #

# Drop unwanted columns (2, 3, 5, 7) as in Python
coltodelete <- c(2, 3, 5, 7)
sk <- sk[, -coltodelete]

# Rename columns to match Python version
sk <- sk %>% rename(Date = date, Open = open, Close = close,`Change%` = change_percent)

# merge data #

st_clean$Date <- as.Date(as.character(st_clean$Date), format = "%Y%m%d")
sk$Date       <- as.Date(sk$Date, format = "%Y-%m-%d")

# Inner join on Date
data_clean <- inner_join(st_clean, sk, by = "Date")





