### Decision Tree
loan <- read.csv("3_clean_loan.csv")
loan <- read.csv("clean_loan.csv")
loan <- loan[,c(3:12,14)]
head(loan)
#loan$loan_status <- factor(loan$loan_status, levels=c(0,1), labels=c("Bad","Good"))

#suffle and transform class
shuffle_index <- sample(1:nrow(loan))
loan <- loan[shuffle_index, ]
library(dplyr)
clean_loan <- mutate(loan, loan_status = factor(loan_status, levels = c(0,1), labels = c("Bad","Good"))) 
glimpse(clean_loan)



## partition training set + test set
create_train_test <- function(data, size = 0.8) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <-  data[1:total_row,]
  test_sample <- data[(total_row+1):n_row,]
  out <- list()
  out$train <- train_sample
  out$test <- test_sample
  return (out)
} 


#set.seed(189)
#set.seed(88)
set.seed(40)
#set.seed(330)
split <- create_train_test(clean_loan, 0.8) 
train_set <- split$train
test_set <- split$test
dim(train_set)
dim(test_set)
dim(clean_loan)


## decision tree without sub_grade (better accuracy) !!!!!!final
train_set_sub1 <- train_set[,c(1:4,6:11)]
test_set_sub1 <- test_set[,c(1:4,6:11)]
fit1 <- rpart(loan_status~., data = train_set_sub1, method = 'class')
rpart.plot(fit1, extra=101)

predict_unseen2 <- predict(fit1, test_set_sub1, type = 'class')
confusionMatrix(factor(predict_unseen2), factor(test_set_sub1$loan_status),positive='Good')

test_set_sub1$predict_unseen2 <- predict_unseen2

loss_ind <- subset(test_set_sub1, as.character(loan_status)=="Bad" & as.character(predict_unseen2)=="Bad")

saved_loan <- sum(loss_ind$loan_amnt)

interest_ind <- subset(test_set_sub1, as.character(loan_status)=="Good" & as.character(predict_unseen2)=="Bad")

loan$term <- ifelse(loan$term=='1',3,5)
interest_ind$term <- ifelse(interest_ind$term=='1',3,5)

interest_loss <- sum(interest_ind$installment*interest_ind$annual_inc*interest_ind$term - interest_ind$loan_amnt)



## balance between two classes
prop.table(table(train_set$loan_status))
prop.table(table(test_set$loan_status))



## decision tree with all predictor ***
library(rpart)
library(rpart.plot)
fit <- rpart(loan_status~., data = train_set, method = 'class')
rpart.plot(fit, extra=101)

# accuracy   !!!!!
library(caret)
predict_unseen <- predict(fit, test_set, type = 'class')
confusionMatrix(factor(predict_unseen), factor(test_set$loan_status),positive='Good')


# best_pruned
class.tree <- rpart(loan_status~., data=train_set, method="class")
rpart.plot(class.tree, extra=101)

#**
pruned.ct <- prune(class.tree, cp=class.tree$cptable[which.min(class.tree$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
rpart.plot(pruned.ct, extra=101)


predict_unseen1 <- predict(pruned.ct, test_set, type = 'class')
confusionMatrix(factor(predict_unseen1), factor(test_set$loan_status),positive='Good')



## without grade
train_set_sub1_1 <- train_set[,c(1:3,5:11)]
test_set_sub1_1 <- test_set[,c(1:3,5:11)]
fit1_1 <- rpart(loan_status~., data = train_set_sub1_1, method = 'class')
rpart.plot(fit1_1, extra=101)

predict_unseen2_1 <- predict(fit1_1, test_set_sub1_1, type = 'class')
confusionMatrix(factor(predict_unseen2_1), factor(test_set_sub1_1$loan_status),positive='Good')



## decision tree without int_rate 
train_set_sub3 <- train_set[,c(1:2,4:11)]
test_set_sub3 <- test_set[,c(1:2,4:11)]
fit3 <- rpart(loan_status~., data = train_set_sub3, method = 'class')
rpart.plot(fit3, extra=101)

predict_unseen4 <- predict(fit3, test_set_sub3, type = 'class')
confusionMatrix(factor(predict_unseen4), factor(test_set_sub3$loan_status),positive='Good')




## decision tree without int_rate and sub_grade (better accuracy)
train_set_sub2 <- train_set[,c(1:2,4,6:11)]
test_set_sub2 <- test_set[,c(1:2,4,6:11)]
#train_set_sub2 <- train_set[,c(1:2,5:11)]
#test_set_sub2 <- test_set[,c(1:2,5:11)]
fit2 <- rpart(loan_status~., data = train_set_sub2, method = 'class')
rpart.plot(fit2, extra=101)

predict_unseen3 <- predict(fit2, test_set_sub2, type = 'class')
confusionMatrix(factor(predict_unseen3), factor(test_set_sub2$loan_status),positive='Good')

pred <- prediction(fit2, test_set_sub2)
RP.perf <- performance(pred, "prec", "rec")




# Compute roc
library(pROC)
plot(roc(test_set_sub2$loan_status, as.numeric(factor(predict(fit2, test_set_sub2, type = 'class')))))
res.roc <- roc(as.numeric(factor(predict_unseen3))-1, as.numeric(factor(test_set_sub2$loan_status))-1)
res.roc <- roc(as.numeric(factor(test_set_sub2$loan_status)),as.numeric(factor(predict_unseen3)),)
plot.roc(res.roc, print.auc = TRUE)



## decision tree without int_rate and sub_grade and grade ***
train_set_sub3 <- train_set[,c(1:2,6:11)]
test_set_sub3 <- test_set[,c(1:2,6:11)]
fit3 <- rpart(loan_status~., data = train_set_sub3, method = 'class')
rpart.plot(fit3, extra=101)

predict_unseen4 <- predict(fit3, test_set_sub3, type = 'class')
confusionMatrix(factor(predict_unseen4), factor(test_set_sub3$loan_status))


## decision tree sub_grade and grade but with int_rate   ***
train_set_sub4 <- train_set[,c(1:3,6:11)]
test_set_sub4 <- test_set[,c(1:3,6:11)]
fit4 <- rpart(loan_status~., data = train_set_sub4, method = 'class')
rpart.plot(fit4, extra=101)

predict_unseen5 <- predict(fit4, test_set_sub4, type = 'class')
confusionMatrix(factor(predict_unseen4), factor(test_set_sub4$loan_status))







