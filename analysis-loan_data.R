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
loan_clean <- read.csv("loan_data_cleaned.csv")

### Create training and test sets
set.seed(123)
n <- nrow(loan_clean)
train_size <- round(0.75 * n)
train_indices <- sample(1:n, train_size)
train <- loan_clean[train_indices,]
test <- loan_clean[- train_indices,]

### Fit series of logistic regression models on training data
# Model 1: Fit model including all variables I first thought would be relevant for predicting loan default
# VIFs are extremely high for loan_amnt and installment and quite high for term. 
model1 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                         loan_amnt + installment + annual_inc +
                         revol_util + total_acc + dti +
                         delinq_2yrs + bankruptcy_bin + taxliens_bin,
                         data = train , family = "binomial", na.action = na.exclude)
summary(model1)
vif(model1)


## Model 2: Remove "loan_amnt" from model1 because of concerns regarding multicollinearity
# VIFs are all within an acceptable range
model2 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
              installment + annual_inc + revol_util + total_acc + dti +
              delinq_2yrs + bankruptcy_bin + taxliens_bin,
              data = train , family = "binomial", na.action = na.exclude)
summary(model2)
vif(model2)

# Evaluate confusion matrix of Model 2
model2_test_prob <- predict(model2, test, type = "response")
model2_test_pred <- as.factor(ifelse(model2_test_prob > 0.5 , 1 , 0))
confusionMatrix(data = model2_test_pred, factor(test$status_bin))

# Examine ROC and AUC of Model 2
ROC2 <- roc(test$status_bin, model2_test_prob)
plot(ROC2, col = "red")
auc(ROC2)


## Model 3: Add in additional covariates of interest to Model 2.
# Lots of missing data, source is the "inq_last_12m" variable.
model3 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                installment + annual_inc + revol_util + total_acc + dti +
                delinq_2yrs + bankruptcy_bin + taxliens_bin + home_ownership +
                tot_cur_bal + addr_state + num_accts_ever_120_pd + 
                application_type + inq_last_12m,
                data = train , family = "binomial", na.action = na.exclude)
summary(model3)
vif(model3)

sapply(loan_data_vars_interested, function(x) sum(is.na(x)))

# Evaluate model performance for test data
model3_test_prob <- predict(model3, test, type = "response")
model3_test_pred <- as.factor(ifelse(model3_test_prob > 0.5 , 1 , 0))
confusionMatrix(data = model3_test_pred, factor(test$status_bin))

# Examine ROC of Model 3
ROC3 <- roc(test$status_bin, model3_test_prob)
plot(ROC3, col = "red")
auc(ROC3)


## Model 4: Remove credit inquiry variable (missing data) and other variables with low magnitude coefficients
model4 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                installment + revol_util + dti + delinq_2yrs + bankruptcy_bin +
                taxliens_bin + home_ownership + tot_cur_bal + 
                addr_state + num_accts_ever_120_pd + application_type,
                data = train , family = "binomial", na.action = na.exclude)
summary(model4)
vif(model4)


# Evaluate model performance for test data
model4_test_prob <- predict(model4, test, type = "response")
model4_test_pred <- as.factor(ifelse(model4_test_prob > 0.5 , 1 , 0))
confusionMatrix(data = model4_test_pred, factor(test$status_bin))

# Examine ROC of Model 4
ROC4 <- roc(test$status_bin, model4_test_prob)
plot(ROC4, col = "red")
auc(ROC4)


# ### Stepwise build model
# train <- na.omit(train)
# test <- na.omit(test)
# full_model <- glm(status_bin ~ . , data= train, family = "binomial")
# null_model <- glm(status_bin ~ 1 , data= train, family = "binomial")
# step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both")
# 
# # Evaluate model performance for test data
# step_test_prob <- predict(step_model, test, type = "response")
# step_test_pred <- as.factor(ifelse(step_test_prob > 0.5 , 1 , 0))
# 
# confusionMatrix(data = step_test_pred, factor(test$status_bin))
