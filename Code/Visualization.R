#Visualizations of data 


##### Load packages

library(tidyverse)   # dplyr, tibble, ggplot2, etc.
library(tidytext)    # tokenización y stopwords
library(textstem)    # lematización (lemmatize_words() / lemmatize_strings())
library(wordcloud)   # wordcloud tradicional
library(RColorBrewer)#colors
library(ggplot2)    
library (knitr)

### preliminary analysis


final_clean <- read.csv("final_clean.csv")

#combine all text as string

all_text <- final_clean %>%
  pull(Text) %>%             # extrae la columna Text como vector
  na.omit() %>%              # eliminar NA si los hay
  paste(collapse = " ")      # pegar todo con espacios

### create tibble 

text_tbl <- tibble(line = 1, text = all_text)

# tokenize
tokens <- text_tbl %>%
  unnest_tokens(word, text)  # por defecto lower-case y quita puntuación básica

# takeout stopwords 
data("stop_words")           # viene con tidytext
tokens_clean <- tokens %>%
  anti_join(stop_words, by = "word")

# eliminate numerical and 1 letter words
tokens_clean <- tokens_clean %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%  # eliminar solo números
  filter(nchar(word) > 1)                    # eliminar tokens de 1 carácter

# Lematizar (convert "running" -> "run", etc.)
# textstem::lemmatize_words acepta un vector de palabras
tokens_clean <- tokens_clean %>%
  mutate(lemma = lemmatize_words(word))

# count frecuencies
freq <- tokens_clean %>%
  count(lemma, sort = TRUE) %>%
  rename(word = lemma, freq = n)

# Generarate
set.seed(123)
wordcloud(
  words = freq$word,
  freq = freq$freq,
  max.words = 150,
  random.order = FALSE,
  scale = c(4, 0.5),
  colors = brewer.pal(8, "Dark2")
  
)



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

#Scatter plot of sentimet1 with spchange
ggplot(final_clean,
       aes(x = Sentiment1_Numerical,
           y = spchange,
           colour = Sentiment1)) +
  geom_point(size = 3)

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


#Table only for sentiment1
sentiment_table <- 
  final_clean %>% 
  count(Sentiment1) %>% 
  mutate(Type = "Sentiment1") %>% 
  rename(Sentiment = Sentiment1) %>%
  right_join(data.frame(Sentiment = all_sentiments1, Type = "Sentiment1"), 
             by = c("Sentiment", "Type")) %>%
  mutate(n = replace_na(n, 0))

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

# Joint distribution table for Sentiment1 and spchange

sentiment_table <- as.data.frame(table(final_clean$Sentiment1))
kable(sentiment_table, caption = "Table 1. Sentiment Frequency")

sentiment_stock_counts <- final_clean %>% 
  count(spchange, Sentiment1, name = "Count")

kable(sentiment_stock_counts, caption = "Table 2. Sentiment × SP Change")

