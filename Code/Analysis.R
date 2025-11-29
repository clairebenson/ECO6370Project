
            # Analysis #

# Packages #
library(tidyverse)
library(stringr)
library(textstem)
library(wordcloud)

# Models
library(MASS)         # LDA, QDA
library(class)        # kNN
library(rpart)        # decision tree
library(randomForest) # random forest
library(gbm)          # gradient boosting
library(caret)        # train/test split & CV
library(broom)        # tidy model output

# analysis data#

# Make sure cleaning.R has been run at least once so that data/final_clean.csv exists
data_analysis <- read.csv("data/data_analysis.csv", stringsAsFactors = FALSE)

#1. exploratory data analysis #
#1.1. wordcloud #

all_text <- paste(final_clean$Text, collapse = " ")
words    <- unlist(str_split(all_text, "\\s+"))
words    <- lemmatize_words(words)           # from textstem
words    <- words[nchar(words) > 0]          # drop empty strings

freq <- table(words)
set.seed(123)
wordcloud(names(freq), freq,max.words     = 200,scale         = c(4, 0.5),random.order  = FALSE)


#1.2. scatterplot #

ggplot(final_clean,
       aes(x = Sentiment, y = `Change%`, colour = Sentiment)) + geom_point() + labs(title = "Sentiment Scores vs. Actual Stock Price Changes",x = "Sentiment Scores", y = "Price Change"
  ) + theme_minimal()


#1.3. sentiment table #

sentiment_counts <- table(final_clean$Sentiment)
sentiment_table  <- as.data.frame(sentiment_counts)
colnames(sentiment_table) <- c("Sentiment", "Count")
print(sentiment_table)

sentiment_stock_counts <- final_clean %>% count(Stock, Sentiment, name = "Count")
print(sentiment_stock_counts)

#2. regression & classification models #

#2.1. logistic regression #
# Use only non-neutral observations

data_reg <- final_clean %>%
  filter(Sentiment_Numerical != 0) %>%
  mutate(Sentiment_Numerical = as.integer(Sentiment_Numerical == 1), # 1 = positive, 0 = negative
    Stock = as.integer(Stock == "down")                         # 1 = down, 0 = up
  )

nrow(data_reg)

set.seed(42)
train_idx <- sample(seq_len(nrow(data_reg)), size = 0.8 * nrow(data_reg))

train <- data_reg[train_idx, ]
test  <- data_reg[-train_idx, ]


logreg_model <- glm(Stock ~ Sentiment_Numerical,data   = train,family = binomial(link = "logit"))

summary(logreg_model)

coefficients <- tidy(logreg_model)
print(coefficients)

# Test accuracy
prob <- predict(logreg_model, newdata = test, type = "response")
pred <- ifelse(prob > 0.5, 1, 0)
logit_accuracy <- mean(pred == test$Stock)
cat("Logistic regression test accuracy:", round(logit_accuracy, 3), "\n")


#2.2. Classification models with caret #
# for caret, response must be factor

data_cls <- data_reg %>%
  mutate(Stock = factor(Stock, levels = c(0, 1), labels = c("up", "down"))
  )

set.seed(42)
train_idx <- createDataPartition(data_cls$Stock,p = 0.8,list = FALSE)

train_cls <- data_cls[train_idx, ]
test_cls  <- data_cls[-train_idx, ]

ctrl <- trainControl(
  method    = "cv",
  number    = 5,
  classProbs = FALSE
)

model_specs <- list(
  "Linear Discriminant Analysis"    = "lda",
  "Quadratic Discriminant Analysis" = "qda",
  "K-Nearest Neighbors"             = "knn",
  "Decision Tree"                   = "rpart",
  "Random Forest"                   = "rf",
  "Gradient Boosting"               = "gbm"
)

results <- list()

for (name in names(model_specs)) {
  method <- model_specs[[name]]
  
  set.seed(42)
  fit <- train(
    Stock ~ Sentiment_Numerical,
    data      = train_cls,
    method    = method,
    trControl = ctrl
  )
  
  # Find row with best tuning parameters
  best <- fit$bestTune
  best_row <- which(
    apply(
      fit$results[, names(best), drop = FALSE],
      1,
      function(z) all(z == best)
    )
  )
  cv_acc <- fit$results$Accuracy[best_row]
  
  # Test accuracy
  pred_test <- predict(fit, newdata = test_cls)
  test_acc  <- mean(pred_test == test_cls$Stock)
  
  cat(sprintf("%s – Test Accuracy: %.2f, CV Accuracy: %.2f\n",
              name, test_acc, cv_acc))
  
  results[[name]] <- list(
    model    = fit,
    test_acc = test_acc,
    cv_acc   = cv_acc
  )
}