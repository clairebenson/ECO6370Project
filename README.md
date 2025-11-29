
# ECO6370Project #

This repository contains all code and data for our ECO 6370 project:“Examining 
the Sentiment of FOMC Minutes and Its Association with Stock Market Prices.”

Team members:

- Claire Benson
- Angie Nieto Riveros
- Dibekulu Mulu Birhan

## Project Overview ##

This project investigates whether the tone and sentiment of Federal Open Market 
Committee (FOMC) meeting minutes affect stock market behavior, focusing on the S&P 500.

We:
- Clean scrapped FOMC minutes
- Clean an merge the data with S&P 500 movements
- Compute sentiment scores using a lexicon-based method
- Explore the data with wordclouds and visualizations
- Run models to predict market movement using logistic regression and ML classifiers.

## Repository Structure ##

```
ECO6370Project/
│
├── Code/
│     ├── cleaning.R              # Data import, cleaning, merging
│     ├── sentiment_scoring.R     # Sentiment lexicon + scoring functions
│     ├── analysis.R              # EDA, wordclouds, regression & ML models
│
├── Data/
│     ├── Fed_Scrape-2015-2023.csv   # Raw FOMC scraped text
│     ├── SP500.csv                  # Raw S&P 500 daily data
│     ├── final_clean.csv            # Cleaned & merged dataset (produced by cleaning.R)
│
├── Results/
│     ├── figures/                   # Plots and wordclouds
│     └── tables/                    # Output tables
│
├── ECO6370Project.Rproj             # RStudio project file
└── README.md                        # Project documentation

```

## Required R packages ##

```r
install.packages(c("tidyverse", "tokenizers", "stringr", "textstem", "tm", "wordcloud",
  "MASS", "class", "rpart", "randomForest", "gbm", "caret", "broom"))

```

## Running the project ##
### **1. 

```
ECO6370Project.Rproj

```
### **1. Open the RStudio project**
```
ECO6370Project.Rproj
```

### **2. Run Cleaning**
```r
source("Code/Cleaning.R")
```

### **3.  Run Sentiment**
```r
source("Code/Sentiment.R")
```

### **4.  Run Analysis**
```r
source("Code/Analysis.R")
```

## Data Sources ##

FOMC Minutes (2015–2023)
- Scraped from Federal Reserve website by the team.

S&P 500 Stock Data
- Daily open/close/change percentage provided in SP500.csv.




