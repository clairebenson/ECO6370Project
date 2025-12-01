#Visualizations of data 

library(ggplot2)

final_clean <- read.csv("final_clean.csv")

#For now, plotting both raw sentiment scores and sentiment numerical scores!! 
#This needs to be changed!! choose one! I believe it would be raw sentiment 

#Scatterplot by level of Sentiment1 scores

ggplot(final_clean, aes(x = Sentiment1, y = changepercent, color = Sentiment1)) +
  geom_point() +
  labs(title = "Sentiment1 Scores vs. Actual Stock Price Changes",
       x = "Sentiment1",
       y = "Price Change") +
  theme_minimal()

ggplot(final_clean, aes(x = Sentiment1_Numerical, y = changepercent, color = Sentiment1_Numerical)) +
  geom_point() +
  labs(title = "Sentiment1 Scores vs. Actual Stock Price Changes",
       x = "Sentiment1",
       y = "Price Change") +
  theme_minimal()

#Scatterplot by level of Sentiment2 scores 
ggplot(final_clean, aes(x = Sentiment2, y = changepercent, color = Sentiment2)) +
  geom_point() +
  labs(title = "Sentiment2 Scores vs. Actual Stock Price Changes",
       x = "Sentiment2",
       y = "Price Change") +
  theme_minimal()

ggplot(final_clean, aes(x = Sentiment2_Numerical, y = changepercent, color = Sentiment2_Numerical)) +
  geom_point() +
  labs(title = "Sentiment2 Scores vs. Actual Stock Price Changes",
       x = "Sentiment2",
       y = "Price Change") +
  theme_minimal()

#Table counts
all_sentiments1 <- c("positive", "negative")
all_sentiments2 <- c("hawkish", "dovish")

sentiment_table <- bind_rows(
  final_clean %>% 
    count(Sentiment1) %>% 
    mutate(Type = "Sentiment1") %>% 
    rename(Sentiment = Sentiment1) %>%
    right_join(data.frame(Sentiment = all_sentiments1, Type = "Sentiment1"), by = c("Sentiment", "Type")) %>%
    mutate(n = replace_na(n, 0)),
  final_clean %>% 
    count(Sentiment2) %>% 
    mutate(Type = "Sentiment2") %>% 
    rename(Sentiment = Sentiment2) %>%
    right_join(data.frame(Sentiment = all_sentiments2, Type = "Sentiment2"), by = c("Sentiment", "Type")) %>%
    mutate(n = replace_na(n, 0))
)

print(sentiment_table)

# Group by Stock and Sentiment 
sentiment_stock_counts <- bind_rows(
  final_clean %>% 
    group_by(SPchange_Numerical, Sentiment1) %>% 
    summarise(Count = n(), .groups = 'drop') %>% 
    rename(Sentiment = Sentiment1) %>%
    mutate(Type = "Sentiment1"),
  final_clean %>% 
    group_by(SPchange_Numerical, Sentiment2) %>% 
    summarise(Count = n(), .groups = 'drop') %>% 
    rename(Sentiment = Sentiment2) %>%
    mutate(Type = "Sentiment2")
)

print(sentiment_stock_counts)
