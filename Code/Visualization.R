library(ggplot2)

ggplot(final_clean, aes(x = Sentiment, y = changepercent, color = Sentiment)) +
  geom_point() +
  labs(title = "Sentiment Scores vs. Actual Stock Price Changes",
       x = "Sentiment",
       y = "Price Change") +
  theme_minimal()
