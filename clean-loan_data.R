### FILE DESCRIPTION: Clean loan repayment data.

### Data source: LendingClub public release data for year 2015. Accessed from https://www.lendingclub.com/info/download-data.action on 2018/04/09.

### Load libraries needed for analysis
library(dplyr)
library(stringr)

### Import data 
loan_data <- read.csv("loan_data_raw.csv")

### Clean and prepare data for analysis

# Remove % from all values for interest rate and revolving credit utilization variables
loan_data$int_rate <- str_replace_all(loan_data$int_rate, "%", "")   
loan_data$revol_util <- str_replace_all(loan_data$revol_util, "%", "")   

# # Set variables to proper class for analysis
loan_data$emp_length <- factor(loan_data$emp_length, ordered = TRUE,
                              levels = c("< 1 year", "1 year", "2 years",
                              "3 years", "4 years", "5 years",
                              "6 years", "7 years", "8 years",
                              "9 years", "10+ years"))
loan_data$emp_length_num <- as.numeric(loan_data$emp_length)
loan_data$sub_grade_num <- as.numeric(loan_data$sub_grade)
loan_data$loan_amnt <- as.numeric(loan_data$loan_amnt)
loan_data$revol_bal <- as.numeric(loan_data$revol_bal)
loan_data$revol_util <- as.numeric(loan_data$revol_util)
loan_data$total_acc <- as.numeric(loan_data$total_acc)
loan_data$delinq_2yrs <- as.numeric(loan_data$delinq_2yrs)

# Generate binary variables for bankruptcy and tax liens
loan_data <- loan_data %>% mutate(bankruptcy_bin = factor(ifelse(loan_data$pub_rec_bankruptcies > 0, 1, 0)))
table(loan_data$bankruptcy)
loan_data <- loan_data %>% mutate(taxliens_bin = factor(ifelse(loan_data$tax_liens > 0, 1, 0)))


# Filter and summarize observations
loan_data_paidORdefault <- loan_data %>%
  filter(loan_status == "Charged Off" | loan_status == "Fully Paid") 

# Generate loan status variable where 1 = default and 2 = fully paid and verify proper variable creation
loan_data_paidORdefault <- loan_data_paidORdefault %>% mutate(status_bin = factor(ifelse(loan_data_paidORdefault$loan_status == "Charged Off", 1 , 0)))
table(loan_data_paidORdefault$loan_status, loan_data_paidORdefault$status_bin)

# Generate cleaned data which includes only variables of interest
vars_interested <- c("loan_status", "term", "sub_grade", "emp_length", "home_ownership",
                     "addr_state", "application_type", "purpose", "loan_amnt", "installment",
                     "annual_inc", "revol_bal", "revol_util", "total_acc", "dti", "delinq_2yrs",
                     "bankruptcy_bin", "taxliens_bin", "status_bin", "emp_length_num", "sub_grade_num")
loan_data_vars_interested <- loan_data_paidORdefault %>% select(one_of(vars_interested))


### Export cleaned data for analysis
write.csv(loan_data_vars_interested, "loan_data_cleaned.csv", row.names = FALSE)
