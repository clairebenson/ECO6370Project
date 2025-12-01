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


dovish_words <- c("accommodate", "accommodation", "borrowing",
                  "devastation", "downturn", "recession",
                  "slowdown", "stimulate", "stimulus", "unemployment")

hawkish_words <- c("business", "businesses", "demand", "economic",
             "economy", "employment", "energy", "equities",
             "equity", "expansion", "financial", "growth",
             "housing", "income", "indicators", "inflation",
             "inflationary", "investment", "investments", "labor",
             "manufacturing", "outlook", "output", "price",
             "prices", "production", "recovery", "resource",
             "securities", "slack", "spending", "target",
             "toll", "wage", "wages")


sentiment_mapping1 <- c(positive = 1, negative = 0)
sentiment_mapping2 <- c(hawkish = 1, dovish = 0)

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

#sentiment2 is for dovish/hawkish word classification
calculate_sentiment2 <- function(text) {
  w <- unlist(str_split(text, "\\s+"))
  num_dovish <- sum(w %in% dovish_words)
  num_hawkish <- sum(w %in% hawkish_words)
  
  if (num_dovish > num_hawkish) {"dovish"} else if (num_hawkish > num_dovish) {"hawkish" } else {"neutral"}
}

final_clean$Sentiment2 <- vapply(final_clean$Text,calculate_sentiment2,FUN.VALUE = character(1))

# Remove neutral sentiments for Sentiment2
final_clean <- final_clean %>% filter(Sentiment2 != "neutral")

final_clean$Sentiment2_Numerical <- unname(sentiment_mapping2[final_clean$Sentiment2])

# add stock move and sentiment dummies 
final_clean$changepercent = ((final_clean$close - final_clean$open) / final_clean$open)*100

final_clean <- final_clean %>% mutate(spchange = case_when(`changepercent` < 0 ~ "down",`changepercent` == 0 ~ "zero",TRUE ~ "up"))

final_clean$SPchange_Numerical <- ifelse(final_clean$spchange == "down", 1, 0)

# save data 
write.csv(final_clean, "final_clean.csv", row.names = FALSE)