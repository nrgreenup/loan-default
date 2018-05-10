### FILE DESCRIPTION: Analyze cleaned loan repayment data. Fit models predicting loan default.

### Data source: LendingClub public release data for year 2015. Accessed from https://www.lendingclub.com/info/download-data.action on 2018/04/09.

### Load libraries needed for analysis
library(dplyr)
library(stringr)
library(ggplot2)
library(pROC)
library(caret)
library(car)

### Import data 
loan_import <- read.csv("loan_data_cleaned.csv")

### Remove missing values 
loan_clean <- na.omit(loan_import)

### Create training and test sets
set.seed(65156)
n <- nrow(loan_clean)
train_size <- round(0.75 * n)
train_indices <- sample(1:n, train_size)
train <- loan_clean[train_indices,]
test <- loan_clean[- train_indices,]

### Fit series of logistic regression models on training data
# Fit model including all variables I first thought would be relevant for predicting loan default
# VIFs are extremely high for loan_amnt and installment and quite high for term. 
model1 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                         loan_amnt + installment + annual_inc +
                         revol_util + total_acc + dti +
                         delinq_2yrs + bankruptcy_bin + taxliens_bin,
                         data = train , family = "binomial")
summary(model1)
vif(model1)

# Remove "installment" from model1 because of concerns regarding multicollinearity
# VIFs are all within an acceptable range
model2 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
              loan_amnt + annual_inc + revol_util + total_acc + dti +
              delinq_2yrs + bankruptcy_bin + taxliens_bin,
              data = train , family = "binomial")
summary(model2)
vif(model2)


### Evaluate model performance for test data
model2_test_prob <- predict(model2, test, type = "response")
model2_test_pred <- as.factor(ifelse(model2_test_prob > 0.5 , 1 , 0))

confusionMatrix(data = model2_test_pred, factor(test$status_bin))