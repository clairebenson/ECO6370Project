
### Load packages
library(tidyverse)
library(lubridate)
library(stringr)


### Importing datasets
st <- read_csv("communications.csv")
sk <- read_csv("sap500.csv")

### Remove unwanted columns 
cols_to_delete <- c(3, 4, 6, 8)
sk <- sk[ , -cols_to_delete]

### Text Cleaning Function
tokenize_and_clean <- function(text) {
  
  if (is.na(text)) return(NA)
  
  text %>%
    tolower() %>%                                 # lowercase
    str_replace_all("[^a-z\\s]", " ") %>%         # remove numbers & special chars
    str_squish()                                  # remove extra spaces
}

### Apply cleaning to text column in `st`
st <- st %>%
  mutate(Text = map_chr(Text, tokenize_and_clean))

### Rename columns 
st <- st %>%
  rename(
    date = Date
  )

sk <- sk %>%
  rename(
    date = Date,
    open = Open,
    close = Close,
    #change = change_percent
  )


### Merge datasets on "Date"
final_clean <- inner_join(st, sk, by = "date")

### Save merged dataset
write_csv(final_clean, "final_clean.csv")

#Defining positive and negative word lists
Tadle_positive <- c("abating", "accelerated", "add", "advance", "advanced",
              "augmented", "balanced", "better", "bolsters", "boom",
              "booming", "boost", "boosted", "eased", "elevated", "elevating",
              "expand", "expanding", "expansionary", "extend", "extended",
              "fast", "faster", "firmer", "high", "higher", "improved",
              "improvement", "improving", "increase", "increased", "increases", 
              "increasing", "more", "raise", "rapid", "rebounded", "recovering",
              "rise", "risen", "rising", "robust", "rose", "significant", 
              "solid", "sooner", "spike", "spikes", "spiking", "stable", "strength",
              "strengthen", "stengthened", "strengthens", "strong", "stronger",
              "supportive", "up", "upside", "upsing", "uptick")
