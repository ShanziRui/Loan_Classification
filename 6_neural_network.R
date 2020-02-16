library(dplyr)
library(neuralnet)
library(caret)
library(reshape2)
library(ggplot2)

### neural network
loan <- read.csv("3_clean_loan.csv")
loan <- read.csv("clean_loan.csv")
loan <- loan[,c(3:12,14)]
head(loan)

#suffle and transform class
shuffle_index <- sample(1:nrow(loan))
loan <- loan[shuffle_index, ]


## scaling input and 
glimpse(loan)

#loan_amnt normalize
max_amnt <- max(loan$loan_amnt)
min_amnt <- min(loan$loan_amnt)
#plot(loan$loan_amnt)
loan$loan_amnt <- (loan$loan_amnt - min_amnt) / (max_amnt - min_amnt)

#term -> 0,1
loan$term <- ifelse(loan$term == 5, 1, 0)

#int_rate normalize
max_int <- max(loan$int_rate)
min_int <- min(loan$int_rate)
loan$int_rate <- (loan$int_rate - min_int) / (max_int - min_int)

# grade and subgrade ordinal ?
unique(loan$grade)
levels(loan$grade) <- c(7:1)
loan$grade <- (as.numeric(as.character(loan$grade)) - 1)/6

unique(loan$sub_grade)
levels(loan$sub_grade) <- c(35:1)
loan$sub_grade <- (as.numeric(as.character(loan$sub_grade)) - 1)/34

#emp_length
max_len <- max(loan$emp_length)
min_len <- min(loan$emp_length)
loan$emp_length <- (loan$emp_length - min_len)/(max_len - min_len)

#home_ownership -> -1,0,1
unique(loan$home_ownership)
levels(loan$home_ownership) <- c(-1,0,1)
loan$home_ownership <- as.numeric(as.character(loan$home_ownership))

#annual_inc normalize
max_inc <- max(loan$annual_inc)
min_inc <- min(loan$annual_inc)
loan$annual_inc <- (loan$annual_inc - min_inc)/(max_inc - min_inc)

#installment normalize
max_ins <- max(loan$installment)
min_ins <- min(loan$installment)
loan$installment <- (loan$installment - min_ins)/(max_ins - min_ins)

#delinq_2yrs normalize
max_del <- max(loan$delinq_2yrs)
min_del <- min(loan$delinq_2yrs)
loan$delinq_2yrs <- (loan$delinq_2yrs - min_del)/(max_del - min_del)

#loan_status categorical -> 1-to-C
loan <- mutate(loan, loan_status = factor(loan_status, levels = c(0,1), labels = c("Bad","Good")))
loan$good <- ifelse(as.character(loan$loan_status)=="Good", 1, 0)
loan$bad <- ifelse(as.character(loan$loan_status)=="Bad", 1, 0)

clean_loan <- loan[,c(1:10,12,13)]

## train + test
index <- sample(c(1:2000), 0.8*2000, replace = F)
train_set <- clean_loan[index, ]
test_set <- clean_loan[-index, ]
dim(train_set)
dim(test_set)


##neural network
nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=2,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=3,stepmax=1e6)


nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=4,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=5,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=6,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=7,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=8,stepmax=1e6)

nn <- neuralnet(bad+good ~ ., data = train_set, linear.output = F,hidden=9,stepmax=1e6)

#choose hidden=5
validation <- read.csv("validatioin.csv")
val <- melt(validation[-5], id.vars="hidden")

ggplot(val, aes(hidden, value, col=variable)) + geom_point() + stat_smooth() + xlab("hidden") +ylab("performance")


#plot
nn$weights

prediction(nn)

plot(nn,rep="best")


#accuracy

predict <- compute(nn,test_set[,c(1:10)])
predicted.class = apply(predict$net.result,1,which.max) - 1
confusionMatrix(factor(ifelse(predicted.class=="1","Good","Bad")),factor(ifelse(test_set$good=='1',"Good","Bad")),positive='Good')


## financial
test_ind <- loan[-index, ]
test_ind$predicted.class <- predicted.class
test_ind$loan_amnt <- (test_ind$loan_amnt*(max_amnt - min_amnt) + min_amnt)

loss_ind <- subset(test_ind, good==0 & predicted.class==0)

saved_loan <- sum(loss_ind$loan_amnt)


interest_ind <- subset(test_ind, good==1 & predicted.class==0)

loan$term <- ifelse(loan$term==1,5,3)

interest_ind$term <- ifelse(interest_ind$term==0,3,5)
interest_ind$int_rate <- (interest_ind$int_rate*(max_int - min_int) + min_int)
interest_ind$installment <- (interest_ind$installment*(max_ins - min_ins) + min_ins)
interest_ind$annual_inc <- (interest_ind$annual_inc*(max_inc - min_inc) + min_inc)


interest_loss <- sum(interest_ind$installment*interest_ind$annual_inc*interest_ind$term - interest_ind$loan_amnt)



## remove sub_grade
train_set1 <- train_set[, c(1:4,6:12)]
test_set1 <- test_set[, c(1:4,6:12)]

nn1 <- neuralnet(bad+good ~ ., data = train_set1, linear.output = F,hidden=5,stepmax=1e6)

plot(nn1,rep="best")

predict1 <- compute(nn1,test_set1[,c(1:9)])
predicted.class1 = apply(predict1$net.result,1,which.max) - 1
confusionMatrix(factor(ifelse(predicted.class1=="1","Good","Bad")),factor(ifelse(test_set$good=='1',"Good","Bad")),positive='Good')


## remove sub_grade and int_rate
train_set2 <- train_set[, c(1:2,4,6:12)]
test_set2 <- test_set[, c(1:2,4,6:12)]

nn2 <- neuralnet(bad+good ~ ., data = train_set2, linear.output = F,hidden=5,stepmax=1e6)

predict2 <- compute(nn2,test_set2[,c(1:8)])
predicted.class2 = apply(predict2$net.result,1,which.max) - 1
confusionMatrix(factor(ifelse(predicted.class2=="1","Good","Bad")),factor(ifelse(test_set$good=='1',"Good","Bad")),positive='Good')


## remove sub and int and grade
train_set3 <- train_set[, c(1:2,6:11)]
test_set3 <- test_set[, c(1:2,6:11)]

nn3 <- neuralnet(loan_status ~ ., data = train_set3, linear.output = F,stepmax=1e6)

predict3 <- compute(nn3,test_set3[,c(1:7)])
predicted.class3 = apply(predict3$net.result,1,which.max) - 1
confusionMatrix(factor(ifelse(predicted.class3=="1","Good","Bad")),factor(test_set3$loan_status))











