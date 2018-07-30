# Loan Repayment
Description: Using LendingClub's publicly available loan data for 2015, I build logistic regression and random forest classification models for loan default. I also analyze return on investment as a function of a few key predictors. All analyses are performed using R.

## Analytical Report
The information in the README.md file below contains instructions and helpful information for replicating all analyses. For a detailed step-by-step report that walks through the analytical process, see visit my [website](https://nrgreenup.github.io/Loan-Default-and-Returns-on-Investment-Analyses/).

## Necessary Software 
You will need the following software and R packages installed to run code files and reproduce analyses.

Necessary software: `R` 

Necessary `R` packages: `car` , `caret` , `dplyr` , `ggplot2`, `pROC` , `randomForest` , `stringr`  

## File Descriptions
File and folder names begin with a prefix according to their function:

      analysis- : R analysis file
      clean-    : R data cleaning and exploratory analysis file
      /graphs/  : PNG files of all graphical output produced by analysis- and clean- files
  
## Installation and File Execution
To begin, download all files into a single folder. When using `R`, set this folder as the working directory using `setwd`.

`R` script files are executable once a working directory to the folder containing data files is set. Running these scripts will reproduce all data cleaning procedures, plots, and analyses. The clean- file must be run before the analysis- file.

## Data Source
[LendingClub 2015 Loan Statistics](https://www.lendingclub.com/info/download-data.action)

## Acknowledgments 
[Perceptive Analytics](https://www.r-bloggers.com/how-to-implement-random-forests-in-r/) : For guidance on debugging loop through multiple random forest models with different *mtry* parameters.

## License
See LICENSE.md for licensing details for this project. 

