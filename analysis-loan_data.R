### FILE DESCRIPTION: Analyze cleaned loan repayment data. Fit models predicting loan default.

### Data source: LendingClub public release data for year 2015. Accessed from https://www.lendingclub.com/info/download-data.action on 2018/04/09.

### Load libraries needed for analysis
library(dplyr)
library(stringr)
library(ggplot2)
library(pROC)
library(caret)
library(car)
library(randomForest)
library(margins)

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
##  Model 1: Fit model including all variables I first thought would be relevant for predicting loan default
#   VIFs are extremely high for loan_amnt and installment and quite high for term. 
model1 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                           loan_amnt + installment + annual_inc +
                           revol_util + total_acc + dti +
                           delinq_2yrs + bankruptcy_bin + taxliens_bin,
                           data = train , family = "binomial", na.action = na.exclude)
summary(model1)
vif(model1)

## Model 2: Remove "loan_amnt" from model1 because of concerns regarding multicollinearity
#  VIFs are all within an acceptable range
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
png("graph-ROC_model2.png")
plot(ROC2, col = "red", main = "Model 2 ROC Curve")
dev.off()
auc(ROC2)

## Model 3: Add in additional covariates of interest to Model 2.
#  Lots of missing data, source is the "inq_last_12m" variable.
model3 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                           installment + annual_inc + revol_util + total_acc + dti +
                           delinq_2yrs + bankruptcy_bin + taxliens_bin + home_ownership +
                           tot_cur_bal + addr_state + num_accts_ever_120_pd + 
                           application_type + inq_last_12m,
                           data = train , family = "binomial", na.action = na.exclude)
summary(model3)
vif(model3)

# Check where missingness originates
sapply(loan_data_vars_interested, function(x) sum(is.na(x)))

# Evaluate model performance for test data
model3_test_prob <- predict(model3, test, type = "response")
model3_test_pred <- as.factor(ifelse(model3_test_prob > 0.5 , 1 , 0))
confusionMatrix(data = model3_test_pred, factor(test$status_bin))

# Examine ROC of Model 3
ROC3 <- roc(test$status_bin, model3_test_prob)
png("graph-ROC_model3.png")
plot(ROC3, col = "red")
dev.off()
auc(ROC3)

## Model 4: Remove credit inquiry variable (missing data) and other variables with low magnitude coefficients
model4 <- glm(status_bin ~ term + sub_grade_num + emp_length_num + 
                           installment + revol_util + dti + delinq_2yrs +
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
png("graph-ROC_model4.png")
plot(ROC4, col = "red", main = " Model 4 ROC Curve")
dev.off()
auc(ROC4)

# Examine marginal effects of Model 4
margins4 <-margins(model4, type = "response")
summary(margins4)

### Random forest models
# Default parameters
vars_RF <- c("status_bin", "term", "sub_grade_num", "emp_length_num", 
             "installment", "revol_util", "dti", "delinq_2yrs", "taxliens_bin", 
             "home_ownership", "tot_cur_bal", "addr_state",
             "num_accts_ever_120_pd", "application_type", "revol_util")
train_RF <- select(train, one_of(vars_RF))
train_RF <- na.omit(train_RF)
test_RF <- select(test, one_of(vars_RF))
test_RF <- na.omit(test_RF)

set.seed(1)
RFdef <- randomForest(factor(status_bin) ~ . , data = train_RF)
RFdef

# Try various levels of mtry parameter
set.seed(28)
acc <- c()
for (i in 2:8) {
  RFmtry_i <- randomForest(factor(status_bin) ~ . , data = train_RF, mtry = i)
  RFmtry_i_pred <- predict(RFmtry_i, test_RF, type = "class")
  acc[i-1] = mean(RFmtry_i_pred == test_RF$status_bin)
}

acc <- as.data.frame(acc)
print(as.list(acc))
RF_acc <- ggplot(acc, aes(x= 2:8, y = acc)) + 
          geom_point(col = "red") +
          labs(title = "Accuracy of Random Forest Model by mtry Parameter Value",
               x = "mtry Parameter Value",
               y = "Model Accuracy") +
          theme(plot.title = element_text(hjust = 0.5))
RF_acc
ggsave("graph-RF_accuracy.png")

### Evaluate ROI based on few important predictors
# Distribution of ROI
png("graph-ROI_density.png")
plot(density(loan_data_vars_interested$ROI), main = "Kernel Density Plot of ROI")
dev.off()

# By grade
ROI_grade_mean <- as.data.frame(loan_data_vars_interested %>% 
                                 group_by(grade) %>%
                                 summarise(ROImean = mean(ROI), ROIsd = sd(ROI)))

ROI_gradeMEAN_graph <- ggplot(ROI_grade_mean, aes(x = grade, y = ROImean)) +
                                geom_bar(stat = "identity", fill = "#FF6666") +
                                geom_pointrange(aes(ymin = ROImean - ROIsd, ymax = ROImean + ROIsd)) +
                                ggtitle("Mean ROI by Borrower Grade") +
                                xlab("Borrower Grade") +
                                ylab("Mean ROI") +
                                labs(caption = "Error bars represent mean +/- 1 SD", size = 5) +
                                theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 7))   
ROI_gradeMEAN_graph
ggsave("graph-meanROI_by_grade.png")

ROI_grade_median <- as.data.frame(loan_data_vars_interested %>% 
                                  group_by(grade) %>%
                                  summarise(ROImedian = median(ROI), ROIsd = sd(ROI)))

ROI_gradeMEDIAN_graph <- ggplot(ROI_grade_median, aes(x = grade, y = ROImedian)) +
                                  geom_bar(stat = "identity", fill = "#FF6666") +
                                  ggtitle("Median ROI by Borrower Grade") +
                                  theme(plot.title = element_text(hjust = 0.5)) +
                                  xlab("Borrower Grade") +
                                  ylab("Median ROI") 
ROI_gradeMEDIAN_graph
ggsave("graph-medianROI_by_grade.png")

# By subgrade
ROI_subgrade_mean <- as.data.frame(loan_data_vars_interested %>% 
                                    group_by(sub_grade) %>%
                                    summarise(ROImean = mean(ROI), ROIsd = sd(ROI)))

ROI_subgradeMEAN_graph <- ggplot(ROI_subgrade_mean, aes(x = sub_grade, y = ROImean)) +
                                  geom_bar(stat = "identity", fill = "#FF6666") +
                                  ggtitle("Mean ROI by Borrower Subgrade") +
                                  theme(plot.title = element_text(hjust = 0.5)) +
                                  xlab("Borrower Subgrade") +
                                  ylab("Mean ROI") +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
ROI_subgradeMEAN_graph
ggsave("graph-meanROI_by_subgrade.png")

ROI_subgrade_median <- as.data.frame(loan_data_vars_interested %>% 
                                       group_by(sub_grade) %>%
                                       summarise(ROImedian = median(ROI), ROIsd = sd(ROI)))

ROI_subgradeMEDIAN_graph <- ggplot(ROI_subgrade_median, aes(x = sub_grade, y = ROImedian)) +
                                    geom_bar(stat = "identity", fill = "#FF6666") +
                                    ggtitle("Median ROI by Borrower Subgrade") +
                                    theme(plot.title = element_text(hjust = 0.5)) +
                                    xlab("Borrower Subgrade") +
                                    ylab("Median ROI") +
                                    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
ROI_subgradeMEDIAN_graph
ggsave("graph-medianROI_by_subgrade.png")

# By loan term
ROI_term_mean <- as.data.frame(loan_data_vars_interested %>% 
                                group_by(term) %>%
                                summarise(ROImean = mean(ROI), ROIsd = sd(ROI)))

ROI_termMEAN_graph <- ggplot(ROI_term_mean, aes(x = term, y = ROImean)) +
                              geom_bar(stat = "identity", fill = c("blue", "red")) +
                              ggtitle("Mean ROI by Loan Term") +
                              theme(plot.title = element_text(hjust = 0.5)) +
                              xlab("Loan Term") +
                              ylab("Mean ROI") 
ROI_termMEAN_graph
ggsave("graph-meanROI_by_term.png")

ROI_term_median <- as.data.frame(loan_data_vars_interested %>% 
                                   group_by(term) %>%
                                   summarise(ROImedian = median(ROI), ROIsd = sd(ROI)))

ROI_termMEDIAN_graph <- ggplot(ROI_term_median, aes(x = term, y = ROImedian)) +
                                geom_bar(stat = "identity", fill = c("blue", "red")) +
                                ggtitle("Median ROI by Loan Term") +
                                theme(plot.title = element_text(hjust = 0.5)) +
                                xlab("Loan Term") +
                                ylab("Median ROI") 
ROI_termMEDIAN_graph
ggsave("graph-medianROI_by_term.png")

# By loan amount
set.seed(47401)
ROI_LA_sample <- sample_n(loan_data_vars_interested, size = 10000)
ROI_loanamnt_graph <- ggplot(ROI_LA_sample, aes(x = loan_amnt, y = ROI)) +
                              geom_jitter(fill = "black", size = 0.05) +
                              geom_smooth(method = "lm", aes(color = "LM")) +
                              geom_smooth(method = "loess", aes(color = "Loess")) +
                              ggtitle("ROI by Loan Amount") +
                              theme(plot.title = element_text(hjust = 0.5)) +
                              xlab("Loan Amount") +
                              ylab("ROI") +
                              scale_colour_manual(name="Legend", values=c("blue", "red"))
ROI_loanamnt_graph
ggsave("graph-ROI_by_loanamnt.png")

# Model ROI
ROI_model <- lm(ROI ~ sub_grade_num + term + loan_amnt +
                      dti + delinq_2yrs + taxliens_bin,
                      data = loan_data_vars_interested)
summary(ROI_model)