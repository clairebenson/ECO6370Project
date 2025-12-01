
          # Sentiment Scoring #

# packages #
install.packages (c("tidyverse", "tokenizers", "stringer", "textstem", "wordcloud", "Mass", "class", "rpart", "raondomForest", "gbm", "caret", "broom"))

library(tidyverse)
library(tokenizers)   # sentence tokenization
library(stringr)      # regex & str_squish
library(textstem)     # lemmatization (lemmatize_words)

# +ve, -ve words #

positive_words <- c(
  "abating","accelerated","add","advance","advanced",
  "augmented","balanced","better","bolsters","boom",
  "booming","boost","boosted","eased","elevated",
  "elevating","expand","expanding","expansionary","extend",
  "extended","fast","faster","firm","gains",
  "growing","heightened","high","higher","improved",
  "improvement","improving","increase","increased","increases",
  "increasing","more","raise","rapid","rebounded",
  "recovering","rise","risen","rising","robust",
  "solid","significant","solid","strong","spike",
  "spikes","spiking","stabilize","strengthen","strengthened",
  "strengthens","strong","stronger","supportive",
  "up","upside","upswing","uptick"
)

negative_words <- c(
  "adverse","back","below","constrained","contract",
  "contracting","contraction","cooling","correction","dampen",
  "damping","decelerated","declined","declined","declines",
  "declining","decrease","decreases","decreasing","deepening",
  "depressed","deteriorated","deterioration","diminished","disappointing",
  "dislocation","disruptions","down","downbeat","downside",
  "drop","dropping","ebbed","erosion","fade",
  "faded","fading","fall","fallen","falling",
  "fell","insufficient","less","limit","low",
  "lower","moderated","moderating","moderation","reduce",
  "reduced","reduction","reluctant","removed","restrain",
  "restrained","restraining","restraint","resumption","reversed",
  "slack","slow","slowed","slower","slowing",
  "slowly","sluggish","sluggishness","slumped","soft",
  "softened","softening","stagnant","strained","strains",
  "stress","subdued","tragic","turmoil","underutilization",
  "volatile","vulnerable","wary","weak","weakened",
  "weaker","weakness"
)

calculate_sentiment <- function(text) {
  w <- unlist(str_split(text, "\\s+"))
  num_pos <- sum(w %in% positive)
  num_neg <- sum(w %in% negative)
  
if (num_pos > num_neg) {"positive"} else if (num_neg > num_pos) {"negative" } else {"neutral"}
  }

data_clean$Sentiment <- vapply(data_clean$Text,calculate_sentiment,FUN.VALUE = character(1))

# add stock move and sentiment dummies #
data_clean <- data_clean %>% mutate(Stock = case_when(`Change%` < 0 ~ "down",`Change%` == 0 ~ "zero",TRUE ~ "up"))
sentiment_mapping <- c(positive = 1, negative = -1, neutral = 0)
data_clean$Sentiment_Numerical <- unname(sentiment_mapping[data_clean$Sentiment])

# save data #
write.csv(final_clean, "data/final_clean.csv", row.names = FALSE)




