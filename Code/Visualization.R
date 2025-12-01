#Visualizations of data 

library(ggplot2)

data <- read.csv("final_clean.csv")

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
  labs(title = "Sentiment Scores1 vs. Actual Stock Price Changes",
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