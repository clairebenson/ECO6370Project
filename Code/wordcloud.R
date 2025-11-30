######### Load packages

library(tidyverse)   # dplyr, tibble, ggplot2, etc.
library(tidytext)    # tokenización y stopwords
library(textstem)    # lematización (lemmatize_words() / lemmatize_strings())
library(wordcloud)   # wordcloud tradicional
library(RColorBrewer)#colors

#set directory

setwd("~/Angie/economia/masters/computacion/ECO6370Project/Data")

# library(wordcloud2) # alternativa interactiva

# combine all text as string

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

#eliminate numerical and 1 letter words
tokens_clean <- tokens_clean %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%  # eliminar solo números
  filter(nchar(word) > 1)                    # eliminar tokens de 1 carácter

# Lematizar (convert "running" -> "run", etc.)
# textstem::lemmatize_words acepta un vector de palabras
tokens_clean <- tokens_clean %>%
  mutate(lemma = lemmatize_words(word))

#count frecuencies
freq <- tokens_clean %>%
  count(lemma, sort = TRUE) %>%
  rename(word = lemma, freq = n)

#add colors
set.seed(123)
wordcloud(
  words = freq$word,
  freq = freq$freq,
  max.words = 150,
  random.order = FALSE,
  scale = c(4, 0.5),
  colors = brewer.pal(8, "Dark2")   # ← paleta de colores
)


ggsave("~/Angie/economia/masters/computacion/ECO6370Project/Results/wordcloud_cl.png", width = 10, height = 7, dpi = 300)
