### FILE DESCRIPTION: Clean loan repayment data and perform exploratory analyses.

### Data source: LendingClub public release data for year 2015. Accessed from https://www.lendingclub.com/info/download-data.action on 2018/04/09.

### Load libraries needed for data cleaning
library(dplyr)
library(stringr)
library(ggplot2)


### Import data 
loan_data <- read.csv("loan_data_raw.csv")


### Clean and prepare data for analysis
# Remove % from all values for interest rate and revolving credit utilization variables
loan_data$int_rate <- str_replace_all(loan_data$int_rate, "%", "")   
loan_data$revol_util <- str_replace_all(loan_data$revol_util, "%", "")   

# Set variables to proper class for analysis
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
table(loan_data$bankruptcy, loan_data$bankruptcy_bin)

loan_data <- loan_data %>% mutate(taxliens_bin = factor(ifelse(loan_data$tax_liens > 0, 1, 0)))
table(loan_data$tax_liens, loan_data$taxliens_bin)

# Filter observations, keeping only "completed" or significantly late loans
status_keep <- c("Charged Off", "Default","Late (31-120 days)", "Fully Paid")
loan_data_paidORdefault <- loan_data[loan_data$loan_status %in% status_keep,]

# Generate loan status variable where 1 = undesirable loan and 0 = fully paid, and verify proper variable creation
loan_data_paidORdefault <- loan_data_paidORdefault %>% mutate(status_bin = factor(ifelse(loan_data_paidORdefault$loan_status == "Charged Off" | loan_data_paidORdefault$loan_status == "Default" | loan_data_paidORdefault$loan_status == "Late (31-120 days)", 1 , 0)))
table(loan_data_paidORdefault$loan_status, loan_data_paidORdefault$status_bin)

# Generate return on investment for each loan
loan_data_paidORdefault <- loan_data_paidORdefault %>% mutate(ROI = (total_pymnt - loan_amnt) * 100 / loan_amnt)


### Exploratory Analyses
# Distribution of loan status
status_dist <- as.data.frame(loan_data_paidORdefault %>% 
                               group_by(status_bin) %>%
                               summarise(count = n()))

status_dist$count <- status_dist$count / sum(status_dist$count)

status_dist_graph <- ggplot(status_dist, aes(x = status_bin, y = count)) +
                             geom_bar(stat = "identity", fill = c("blue", "red")) +
                             ggtitle("Distribution of Loan Outcomes") +
                             theme(plot.title = element_text(hjust = 0.5)) +
                             xlab("Loan Status") +
                             ylab("Proportion of All Loans") +
                             scale_x_discrete(labels = c("Paid", "Default")) +
                             scale_fill_discrete(name = "Loan Status", labels = c("Paid", "Default"))
status_dist_graph
ggsave("graph-loan_status_distribution.png")

#Loan status by term
status_term <- as.data.frame(loan_data_paidORdefault %>% 
                               group_by(term) %>%
                               summarise(count = sum(status_bin == 1), total = n(), pct = count / total))

status_term_graph <- ggplot(status_term, aes(x = term, y = pct)) +
                             geom_bar(stat = "identity", fill = c("blue", "red")) +
                             ggtitle("Default Rate by Loan Term") +
                             theme(plot.title = element_text(hjust = 0.5)) +
                             xlab("Term") +
                             ylab("Default Rate") 
status_term_graph
ggsave("graph-status_by_term.png")

# Loan status by grade
status_grade <- as.data.frame(loan_data_paidORdefault %>% 
                                group_by(grade) %>%
                                summarise(count = sum(status_bin == 1), total = n(), pct = count / total))

status_grade_graph <- ggplot(status_grade, aes(x = grade, y = pct)) +
                              geom_bar(stat = "identity", fill = "#FF6666") +
                              ggtitle("Default Rate by Borrower Grade") +
                              theme(plot.title = element_text(hjust = 0.5)) +
                              xlab("Loan Grade") +
                              ylab("Default Rate") 
status_grade_graph
ggsave("graph-status_by_grade.png")

# Loan status by subgrade
status_subgrade <- as.data.frame(loan_data_paidORdefault %>% 
                                  group_by(sub_grade) %>%
                                  summarise(count = sum(status_bin == 1), total = n(), pct = count / total))

status_subgrade_graph <- ggplot(status_subgrade, aes(x = sub_grade, y = pct)) +
                                  geom_bar(stat = "identity", fill = "#FF6666") +
                                  ggtitle("Default Rate by Borrower Subgrade") +
                                  theme(plot.title = element_text(hjust = 0.5)) +
                                  xlab("Subgrade") +
                                  ylab("Default Rate") +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
status_subgrade_graph
ggsave("graph-status_by_subgrade.png")

# Loan status by installment amount
loan_data_paidORdefault$installment_ordinal <- cut(loan_data_paidORdefault$installment, breaks = 8,
                                                    ordered_result = TRUE)

status_installment <- as.data.frame(loan_data_paidORdefault %>% 
                                     group_by(installment_ordinal) %>%
                                     summarise(count = sum(status_bin == 1), total = n(), pct = count / total))

status_installment_graph <- ggplot(status_installment, aes(x = installment_ordinal, y = pct)) +
                                    geom_bar(stat = "identity", fill = "#FF6666") +
                                    ggtitle("Default Rate by Installment Amount") +
                                    theme(plot.title = element_text(hjust = 0.5)) +
                                    xlab("Installment Amount, in dollars") +
                                    scale_x_discrete(labels = c("(29,207]", "(207,384]" , "(384,561]", "(561,738]", "(738,915]", "(915,1009]", "(1009,1270]", "(1270,1450]")) +
                                    ylab("Default Rate") +
                                    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
status_installment_graph
ggsave("graph-status_by_installment.png")


### Generate cleaned data which includes only variables of interest
vars_interested <- c("term", "sub_grade", "emp_length", "home_ownership",
                     "addr_state", "application_type", "purpose", "loan_amnt", "installment",
                     "annual_inc", "revol_bal", "revol_util", "total_acc", "dti", "delinq_2yrs",
                     "bankruptcy_bin", "taxliens_bin", "status_bin", "emp_length_num", "sub_grade_num",
                     "total_bc_limit", "total_bal_ex_mort", "num_accts_ever_120_pd",
                     "bc_util", "chargeoff_within_12_mths", "inq_last_12m", "inq_last_6mths","tot_cur_bal",
                     "pub_rec", "ROI", "total_pymnt", "loan_status", "grade")
loan_data_vars_interested <- loan_data_paidORdefault %>% select(one_of(vars_interested))


### Export cleaned data for analysis
write.csv(loan_data_vars_interested, "loan_data_cleaned.csv", row.names = FALSE)