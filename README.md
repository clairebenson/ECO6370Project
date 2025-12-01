
# ECO6370Project #
**Examining the Sentiment of FOMC Statements and Its Association with Stock Market Prices**

This repository contains the data, code, and analysis for our ECO6370 final project.

Team members:

- Claire Benson
- Angie Nieto Riveros
- Dibekulu Mulu Birhan

## Project Overview ##

This project investigates whether and how the sentiment of statements from the 
Federal Open Market Committee (FOMC) meetings influences movements in the S&P 500 stock 
index price on the day of release.

The workflow includes:
- Cleaning scrapped FOMC statements
- Cleaning S&P 500 price movement data
- Compute sentiment scores using dictionary method 
- Explore the data with wordclouds and visualizations
- Run regression and classification models

## Repository Structure ##

```
ECO6370Project/
│
├── Code/
│     ├── cleaning.R              # Data cleaning 
│     ├── sentiment.R     # Sentiment scoring
│     ├── analysis.R              # Exploratory data analysis, regression & ML models
│
├── Data/
│     ├── communications.csv   # Raw FOMC scraped text data
│     ├── sap500.csv                  # Raw S&P 500 daily historical data
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

FOMC Minutes (2000–20225)

S&P 500 index historical Data

- Data source: kaggle for both.


