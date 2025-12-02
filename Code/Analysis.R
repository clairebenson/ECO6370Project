
            # Analysis #

# Packages #
install.packages (c("tidyverse", "pROC", "tokenizers", "stringr", "textstem", "wordcloud", "MASS", "class", "rpart", "randomForest", "gbm", "caret", "broom"))

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
library(pROC)

# analysis data#

final_clean <- read.csv("final_clean.csv", stringsAsFactors = FALSE)

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
reg_table <- tidy(logreg_model)

kable(reg_table, 
      caption = "Table: Logistic Regression of SP Change on Sentiment")

# ROC object
prob <- predict(logreg_model, newdata = test, type = "response")
roc_obj <- roc(response = test$SPchange_Numerical,
               predictor = prob)

plot(roc_obj,
     col = "#354CA1",
     main = "ROC Curve for SPchange Prediction",
     legacy.axes = TRUE)
auc(roc_obj)

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

set.seed(42)
train_idx <- createDataPartition(final_clean$spchange, p = 0.8, list = FALSE)

train_cls <- final_clean[train_idx, ]
test_cls  <- final_clean[-train_idx, ]

ctrl <- trainControl(method = "cv", number = 5)

models <- c("lda","qda","knn","rpart","rf","gbm")

# Create empty results storage
acc_results <- data.frame(
  Model = character(),
  Test_Accuracy = numeric(),
  CV_Accuracy = numeric(),
  stringsAsFactors = FALSE
)

for (m in models) {
  set.seed(42)
  fit <- train(
    spchange ~ Sentiment1_Numerical,
    data = train_cls,
    method = m,
    trControl = ctrl
  )
  
  # Test accuracy
  pred_test <- predict(fit, newdata = test_cls)
  test_acc  <- mean(pred_test == test_cls$spchange)
  
  # Cross-validation accuracy (best tuning parameter)
  cv_acc <- max(fit$results$Accuracy)
  
  # Store results in the table
  acc_results <- rbind(
    acc_results,
    data.frame(
      Model = m,
      Test_Accuracy = round(test_acc, 3),
      CV_Accuracy = round(cv_acc, 3)
    )
  )
}

# Display clean table
kable(acc_results, 
      caption = "Classification Model Accuracy (Test & CV)")
