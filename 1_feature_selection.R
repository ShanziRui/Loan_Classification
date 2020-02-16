### load data
raw_loan <- read.csv("loan.csv",stringsAsFactors=FALSE)
raw_loan <- read.csv("loan_test.csv",,stringsAsFactors=FALSE)

head(raw_loan)
summary(raw_loan)
ncol(raw_loan)  #145
nrow(raw_loan)  #2,260,668


## empty column
col.na <- sapply(raw_loan, function(x)all(is.na(x)))
index <- which(col.na == TRUE)
index  # 3 empty columns
raw_loan1 <- raw_loan[,-index]
summary(raw_loan2)


# loan_status
raw_loan$loan_status <- ifelse(raw_loan$loan_status=="Does not meet the credit policy. Status:Charged Off","Charged Off",raw_loan$loan_status)

raw_loan$loan_status <- ifelse(raw_loan$loan_status=="Does not meet the credit policy. Status:Fully Paid","Fully Paid",raw_loan$loan_status)

summary(as.factor(raw_loan$loan_status))


# frequency plot
library(ggplot2)
barplot(summary(as.factor(raw_loan$settlement_status)))


# convert missing values
raw_loan2 <- read.csv("loan_test.csv",na.string=c(c("","n/a"),"NA"))
head(raw_loan2,10)

unique(raw_loan2$emp_length)
unique(raw_loan2$home_ownership)


#convert empty_length to num
test <- gsub("([0-9]+).*$", "\\1", raw_loan2$emp_length)
test[test=="< 1"] <- "0"

unique(as.numeric(unique(test)))



summary(raw_loan2$emp_length)
summary(raw_loan2$annual_inc)
summary(as.numeric(as.character(raw_loan2$annual_inc)))
inc <- as.numeric(as.character(raw_loan2$annual_inc))
plot(inc[inc<4e+07])


ncol(raw_loan2)
summary(raw_loan2$issue_d)


##convert issue_d to Date and sample 1000 records for year 2017 and 2018
head(paste0(raw_loan2$issue_d,"-01"))
raw_loan2$issue_d <- as.Date(paste0(raw_loan2$issue_d,"-01"),format="%b-%Y-%d")


raw_loan2$issue_d <- as.character(raw_loan2$issue_d)

rec_loan_18 = raw_loan2[ sample(which(raw_loan2$issue_d>=as.Date("2018-01-01")),1000),]

rec_loan_18 = raw_loan2[ sample(which(grepl("2018",raw_loan2$issue_d)),1000),]


rec_loan_17 = raw_loan2[ sample(which(as.Date("2018-01-01")>raw_loan2$issue_d & raw_loan2$issue_d>=as.Date("2017-01-01")),1000),]

rec_loan_17 = raw_loan2[ sample(which(grepl("2017",raw_loan2$issue_d)),1000),]

rec_loan = rbind(rec_loan_17,rec_loan_18)


na_perc <- data.frame(colMeans(is.na(rec_loan)))
summary(na_perc)
hist(na_perc)
barplot(na_perc)

df <- data.frame(attributes=1:145,na_perc=na_perc[,1])
p<-ggplot(data=df, aes(x=attributes, y=na_perc, color = attributes)) + geom_bar(stat="identity", fill='white')

p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#999999", "#E69F00"))


library(ggplot2)

p<-ggplot(data=na_perc) +
  geom_bar(stat="identity")


library(dplyr)
not_all_na <- function(x) any(!is.na(x))

na_raw_loan <- raw_loan2[,colMeans(is.na(raw_loan2))<=0.1]
ncol(na_raw_loan)  #86
na_raw_loan1 <- na.omit(na_raw_loan)
nrow(na_raw_loan1) #1852334
summary(na_raw_loan) 

unique(na_raw_loan$purpose)   #14
unique(na_raw_loan$title)   #63157
unique(na_raw_loan$earliest_cr_line)  754

hist(na_raw_loan$delinq_2yrs)
unique(na_raw_loan$delinq_2yrs)
head(na_raw_loan[,c("delinq_2yrs","loan_status")])

unique(na_raw_loan$loan_status)

library(magrittr)
na_raw_loan1 <- na_raw_loan[is_in(na_raw_loan$loan_status,c("Fully Paid", "Charged Off", "Default")),]
na_raw_loan1$loan_status <- droplevels(na_raw_loan1$loan_status,except=is_in(na_raw_loan$loan_status,c("Fully Paid", "Charged Off", "Default")))
unique(na_raw_loan1$loan_status)

raw_loan3 <- raw_loan2[is_in(raw_loan2$loan_status,c("Fully Paid", "Charged Off", "Default")),]
raw_loan3$loan_status <- droplevels(raw_loan3$loan_status,except=is_in(raw_loan3$loan_status,c("Fully Paid", "Charged Off", "Default")))

unique(raw_loan3$loan_status)
nrow(raw_loan3)


#?application_type: remove joint during sampling?

#1.loan_amnt: The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
#2.term: The number of payments on the loan. Values are in months and can be either 36 or 60. >>> 3 or 5 years
#3.installment
#3.1 int_rate
#4.grade: LC assigned loan grade
#5.emp_length: Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.
#6.home_ownership: The home ownership status provided by the borrower during registration or obtained from the credit report. Our values are: RENT, OWN, MORTGAGE, OTHER
#7.annual_inc: The self-reported annual income provided by the borrower during registration
#7.1 installment*12/ann_inc


#8.loan_status >>> class(3->2)


#*sub_grade
#*delinq_2yrs: The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years



#installment: The monthly payment owed by the borrower if the loan originates.
#verification_status: Indicates if income was verified by LC, not verified, or if the income source was verified

#?earliest_cr_line
#open_acc: The number of open credit lines in the borrower's credit file.
#?total_acc: open_acc or total_acc (choose one of them)?


#revol_bal: Total credit revolving balance
#acc_now_delinq: The number of accounts on which the borrower is now delinquent.


#mort_acc: number of mortage accounts
#num_actv_bc_tl: Number of currently active bankcard accounts
#num_bc_sats: Number of satisfactory bankcard accounts
#total_bc_limit: Total bankcard high credit/credit limit




loan_fea <- na_raw_loan[,c("loan_amnt","term","installment","grade","emp_length","home_ownership","annual_inc","verification_status","delinq_2yrs","earliest_cr_line","open_acc","total_acc","revol_bal","acc_now_delinq","acc_now_delinq","mort_acc","num_actv_bc_tl","total_bc_limit","num_bc_sats")]
summary(loan_fea)

loan_pca <- prcomp(loan_fea[,c("loan_amnt","installment","annual_inc","delinq_2yrs","open_acc","total_acc","revol_bal","acc_now_delinq","acc_now_delinq","mort_acc","num_actv_bc_tl","total_bc_limit","num_bc_sats")],center=TRUE,scale.=TRUE)
summary(loan_pca)


library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(loan_pca,ellipse=TRUE,choices=c(10,11))



library(data.table)

library(dplyr)

library(formattable)

library(tidyr)

clean_loan <- read.csv("3_clean_loan.csv")

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"


col <- colnames(clean_loan)
col <- col[-1]
loan_plot <- na_raw_loan1[,col]


head(loan_plot)
attach(loan_plot)

formattable(head(loan_plot))

part <- loan_plot[c(1:4,7:9,13:16),]

formattable(part, align =c("l","c","c","c","c","c","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `home_ownership`= color_tile(customGreen, customGreen0),
  `term`= color_tile(customGreen, customGreen0),
  `installment`= color_tile(customGreen, customGreen0),
  `application_type`= color_tile(customGreen, customGreen0),
  `loan_status`= color_tile(customGreen, customGreen0),
  `emp_length`= color_tile(customGreen, customGreen0)
))



clean_loan <- read.csv("clean_loan.csv")

b <- data.frame(loan_status=c('Charged Off','Current','Default','Does not meet the credit policy. Status:Charged Off','Does not meet the credit policy. Status:Fully Paid','Fully Paid','Late'),Freq=c(261655,919695,31,761,1988,1041952,34586))





