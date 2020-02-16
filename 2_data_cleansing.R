#loan_amnt: The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
#term: The number of payments on the loan. Values are in months and can be either 36 or 60.
#installment: The monthly payment owed by the borrower if the loan originates.
#grade: LC assigned loan grade
#emp_length: Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.
#home_ownership: The home ownership status provided by the borrower during registration or obtained from the credit report. Our values are: RENT, OWN, MORTGAGE, OTHER
#annual_inc: The self-reported annual income provided by the borrower during registration
#verification_status: Indicates if income was verified by LC, not verified, or if the income source was verified
#delinq_2yrs: The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years
#?earliest_cr_line
#open_acc: The number of open credit lines in the borrower's credit file.
#?total_acc
#revol_bal: Total credit revolving balance
#?application_type: remove joint
#acc_now_delinq: The number of accounts on which the borrower is now delinquent.
#mort_acc: number of mortage accounts
#num_actv_bc_tl: Number of currently active bankcard accounts
#num_bc_sats: Number of satisfactory bankcard accounts
#total_bc_limit: Total bankcard high credit/credit limit


### load data
### 'data.frame':	887379 obs. of  74 variables
raw_loan <- read.csv("loan.csv")

head(raw_loan)
library(magrittr)
library(dplyr)
library(lubridate)
# Data Preprocessing: features and target selection
# loan_amnt, term, int_rate, grade, sub_grade
# emp_length, home_ownership, annual_income
# target: loan_status (no default: "Fully Paid", default: "Charged Off", "Default")
# selected loan data

s_loan <- raw_loan %>% select("issue_d", "loan_amnt", "term", "int_rate", "grade", "sub_grade", "emp_length", "home_ownership", "annual_inc", "installment","delinq_2yrs", "application_type", "loan_status")



# data cleaning
s_loan$issue_d <- as.character(s_loan$issue_d)
s_loan$issue_d <- paste0(s_loan$issue_d, "-01")
s_loan$issue_d <- myd(s_loan$issue_d)
sum(s_loan$loan_amnt <= 0) 
# sum == 0, no missing data or negative data


s_loan$term <- as.numeric(s_loan$term)
s_loan[s_loan$term == 1, "term"] <- 3
s_loan[s_loan$term == 2, "term"] <- 5
sum(s_loan$int_rate <=0 | s_loan$int_rate >30) 
# sum == 0, no missing data or inconsistent data




# grade: 7 levels: A, B, C, D, E, F, G
# sub_grade": 35 levels: "A1" "A2" "A3" "A4" "A5" "B1" "B2" "B3" "B4" "B5" "C1" "C2" "C3" "C4" "C5" "D1" "D2" "D3" "D4" "D5" "E1" "E2" "E3" "E4" "E5" "F1" "F2" "F3" "F4" "F5" "G1" "G2" "G3" "G4" "G5"
# emp_length: 13 levels: "< 1 year"  "1 year"    "10+ years" "2 years"   "3 years"   "4 years"   "5 years"   "6 years"   "7 years"   "8 years"  "9 years"   "n/a"  "RENT" 
# remove "n/a" and "RENT"
s_loan <- s_loan[!is_in(s_loan$emp_length, c("n/a", "RENT")),]
s_loan$emp_length <- as.character(s_loan$emp_length)


# convert emp_length from factor to numeric. emp_length less than 1 year is recorded as 0, and emp_length more than 10 years is recorded as 10.
s_loan$emp_length[s_loan$emp_length == "< 1 year"] <- 0
s_loan$emp_length <- as.numeric(gsub("([0-9]+).*$", "\\1", s_loan$emp_length))
s_loan <- s_loan[!is.na(s_loan$emp_length), ]



# extract "RENT", "OWN", "MORTGAGE"
s_loan <- s_loan[is_in(s_loan$home_ownership, c("RENT", "OWN", "MORTGAGE", "OTHER")), ]
s_loan$home_ownership <- as.factor(as.character(s_loan$home_ownership))



# changing annual_inc into numeric
s_loan$annual_inc <- as.numeric(as.character(s_loan$annual_inc))
s_loan <- s_loan[!is.na(s_loan$annual_inc), ]
#plot(s_loan$annual_inc)


# 842553 observations remaining.
# extract only "INDIVIDUAL" application type.
s_loan <- s_loan[s_loan$application_type == "INDIVIDUAL",]
s_loan$application_type <- as.factor(as.character(s_loan$application_type))


# 842102 observations remaining.
s_loan <- s_loan[is_in(s_loan$loan_status,c("Fully Paid", "Charged Off", "Default")),]
s_loan$loan_status <- as.factor(as.character(s_loan$loan_status))


# 244220 observation remaining.
s_loan$loan_status <- as.character(s_loan$loan_status)
# define "Fully Paid" as not risky, and "Charged Off" / "Default" as risky.
s_loan$loan_status[s_loan$loan_status == "Fully Paid"] <- 1
s_loan$loan_status[s_loan$loan_status == "Charged Off" | s_loan$loan_status == "Default"] <- 0

s_loan$loan_status <- as.numeric(s_loan$loan_status)

s_loan$delinq_2yrs <- as.numeric(s_loan$delinq_2yrs)


# calculate annual installment ratio
s_loan$installment <- s_loan$installment * 12 / s_loan$annual_inc


#balnce class
temp <- s_loan[order(s_loan$issue_d, decreasing = TRUE), ]

temp1 <- temp[temp$loan_status == 0, ] %>% slice(1:1000)
temp2 <- temp[temp$loan_status == 1, ] %>% slice(1:1000)
loan <- rbind(temp1, temp2)


#good and bad
rec_loan_1 = temp[ sample(which(temp$issue_d>=as.Date("2014-01-01") & temp$loan_status==1),1000),]
rec_loan_0 = temp[ sample(which(temp$issue_d>=as.Date("2014-01-01") & temp$loan_status==0),1000),]
loan <- rbind(rec_loan_1,rec_loan_0)



write.csv(loan,"clean_loan.csv")


loan_clean <- read.csv("3_clean_loan.csv")

identical(loan_clean, loan_clean1)













