# Hawish/Dovish sentiment Scoring #

install.packages (c(tidyverse, tokenizers, stringer, textstem, wordcloud, Mass, class, rpart, raondomForest, gbm, caret, broom))
library(tidyverse)
library(tokenizers)   
library(stringr)     
library(textstem)


hawkish <- c("business", "businesses", "demand", "economic",
             "economy", "employment", "energy", "equities",
             "equity", "expansion", "financial", "growth",
             "housing", "income", "indicators", "inflation",
             "inflationary", "investment", "investments", "labor",
             "manufacturing", "outlook", "output", "price",
             "prices", "production", "recovery", "resource",
             "securities", "slack", "spending", "target",
             "toll", "wage", "wages")

dovish <- c("accommodation", "devastation", "downturn", "recession",
            "unemployment")

calculate_sentiment <- function(text) {
  w <- unlist(str_split(text, "\\s+"))
  num_hawkish <- sum(w %in% hawkish)
  num_dovish <- sum(w %in% dovish)
  
  
  if (num_hawkish > num_dovish) {"hawkish"} else if (num_dovish > num_hawkish) {"dovish" } else {"neutral"}
}

# add stock move and sentiment dummies #
data_clean <- data_clean %>% mutate(Stock = case_when(`Change%` < 0 ~ "down",`Change%` == 0 ~ "zero",TRUE ~ "up"))
sentiment_mapping <- c(dovish = 1, hawkish = -1, neutral = 0)
data_clean$Sentiment_Numerical <- unname(sentiment_mapping[data_clean$Sentiment])

# save data #
write.csv(final_clean, "data/final_clean.csv", row.names = FALSE)