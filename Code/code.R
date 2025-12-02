
          # Cleaning #

### Load packages
library(tidyverse)
library(lubridate)
library(stringr)

#####determine folder for data
#setwd("~/Angie/economia/masters/computacion/ECO6370Project/Data")

### Importing datasets
st <- read_csv("Data/communications.csv")
sk <- read_csv("Data/sap500.csv")
dj <- read_csv("Data/DJ_data.csv")

statement <- subset(st, Type == "Statement")

### Remove unwanted columns 
cols_to_delete <- c(3, 4, 6)
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
statement <- statement %>%
  mutate(Text = map_chr(Text, tokenize_and_clean))

### Rename columns 
statement <- statement %>%
  rename(
    date = Date
  )

sk <- sk %>%
  rename(
    date = Date,
    sp_open = Open,
    sp_close = Close,
    #change = change_percent
  )

dj <- dj %>%
  rename(
    date = Date,
    dj_open = Open,
    dj_close = Close
  ) %>%
  select(date, dj_open, dj_close) %>%
  mutate(date = mdy(date)) %>%  # Use lubridate's mdy() for "Month/Day/Year" format
  filter(date %in% statement$date)


### Merge datasets on "Date"
final_clean <- inner_join(statement, sk, by = "date")

#Merge datasets on "date"
final_clean <- statement %>%
  inner_join(sk, by = "date") %>%
  left_join(dj, by = "date")

#Save merged dataset
write_csv(final_clean, "final_clean.csv")

