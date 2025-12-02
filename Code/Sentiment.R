# Sentiment Scoring 

# packages 
install.packages (c("tidyverse", "tokenizers", "stringer", "textstem", "wordcloud", "Mass", "class", "rpart", "raondomForest", "gbm", "caret", "broom"))
library(tidyverse)
library(tokenizers)   # sentence tokenization
library(stringr)      
library(textstem)     #lemmatize_words

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

sentiment_mapping1 <- c(positive = 1, negative = 0)

final_clean <- read.csv("final_clean.csv")

#sentiment1 is for negative/positive word classification
calculate_sentiment1 <- function(text) {
  w <- unlist(str_split(text, "\\s+"))
  num_pos <- sum(w %in% positive_words)
  num_neg <- sum(w %in% negative_words)
  
if (num_pos > num_neg) {"positive"} else if (num_neg > num_pos) {"negative" } else {"neutral"}
  }

final_clean$Sentiment1 <- vapply(final_clean$Text,calculate_sentiment1,FUN.VALUE = character(1))

# Remove neutral sentiments for Sentiment1
final_clean <- final_clean %>% filter(Sentiment1 != "neutral")

final_clean$Sentiment1_Numerical <- unname(sentiment_mapping1[final_clean$Sentiment1])

# add stock move and sentiment dummies 
final_clean$sp_changepercent = ((final_clean$sp_close - final_clean$sp_open) / final_clean$sp_open)*100

final_clean$dj_changepercent = ((final_clean$dj_close - final_clean$dj_open) / final_clean$dj_open)*100

final_clean <- final_clean %>% mutate(spchange = case_when(`sp_changepercent` < 0 ~ "down",`sp_changepercent` == 0 ~ "zero",TRUE ~ "up"))

final_clean <- final_clean %>% mutate(djchange = case_when(`dj_changepercent` < 0 ~ "down",`dj_changepercent` == 0 ~ "zero",TRUE ~ "up"))

final_clean$SPchange_Numerical <- ifelse(final_clean$spchange == "up", 1, 0)

final_clean$DJchange_Numerical <- ifelse(final_clean$djchange == "up", 1, 0)

# save data 
write.csv(final_clean, "final_clean.csv", row.names = FALSE)
