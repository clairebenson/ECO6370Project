
            # Analysis #

# Packages #
install.packages (c("tidyverse", "tokenizers", "stringr", "textstem", "wordcloud", "MASS", "class", "rpart", "randomForest", "gbm", "caret", "broom"))

library(tidyverse)
library(stringr)
library(textstem)
library(wordcloud)

# Models
library(MASS)         
library(class)        
library(rpart)        
library(randomForest) 
library(gbm)         
library(caret)        
library(broom)        

# analysis data#

final_clean <- read.csv("final_clean.csv", stringsAsFactors = FALSE)

 
#1. exploratory data analysis #
#1.1. wordcloud #

all_text <- paste(final_clean$Text, collapse = " ")
words    <- unlist(str_split(all_text, "\\s+"))
words    <- lemmatize_words(words)
words    <- words[nchar(words) > 0]

freq <- table(words)
set.seed(123)
wordcloud(
  names(freq),
  freq,
  max.words    = 200,
  scale        = c(4, 0.5),
  random.order = FALSE
)

#1.2. scatterplot #

ggplot(final_clean,
       aes(x = Sentiment1_Numerical,
           y = spchange,
           colour = Sentiment1)) +
  geom_point(size = 3) +
  labs(
    title = "Sentiment vs Percentage Price Change",
    x = "Sentiment Score (-1,1)",
    y = "SP500 Price Change (%)"
  ) +
  theme_minimal()

#1.3. sentiment table #

sentiment_counts <- table(final_clean$Sentiment1)
print(as.data.frame(sentiment_counts))

sentiment_stock_counts <- final_clean %>%
  count(spchange, Sentiment1, name = "Count")
print(sentiment_stock_counts)


#2. regression & classification models #

#2.1. logistic regression #

# Train-test split
set.seed(42)

train_idx <- sample(seq_len(nrow(final_clean)), size = 0.8 * nrow(final_clean))

train <- final_clean[train_idx, ]
test  <- final_clean[-train_idx, ]

# Fit logistic regression
logreg_model <- glm(
  SPchange_Numerical~ Sentiment1_Numerical,
  data   = train,
  family = binomial(link = "logit")
)

summary(logreg_model)
tidy(logreg_model)

# Prediction accuracy
prob <- predict(logreg_model, newdata = test, type = "response")
pred <- ifelse(prob > 0.5, 1, 0)
logit_accuracy <- mean(pred == test$SPchange_Numerical)
cat("Logistic regression accuracy:", round(logit_accuracy, 3), "\n")


#2.2. classification models.

set.seed(42)
train_idx <- createDataPartition(final_clean$spchange, p = 0.8, list = FALSE)

train_cls <- final_clean[train_idx, ]
test_cls  <- final_clean[-train_idx, ]

ctrl <- trainControl(method="cv", number=5)

models <- c("lda","qda","knn","rpart","rf","gbm")

for (m in models) {
  set.seed(42)
  fit <- train(
    spchange ~ Sentiment1_Numerical,
    data = train_cls,
    method = m,
    trControl = ctrl
  )
  
  # Prediction
  pred_test <- predict(fit, newdata = test_cls)
  test_acc  <- mean(pred_test == test_cls$spchange)
  
  cat("Model:", m, 
      " | Test Accuracy:", round(test_acc,3),
      " | CV Accuracy:", round(max(fit$results$Accuracy),3), "\n")
}

