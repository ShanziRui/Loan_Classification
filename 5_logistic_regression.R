library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(psych)
library(GPArotation)

### Logistic regression
loan <- read.csv("3_clean_loan.csv")
loan <- read.csv("clean_loan.csv")
loan <- loan[,c(3:12,14)]
head(loan)

loan$gradeB <- ifelse(as.character(loan$grade)=="B", 1, 0)
loan$gradeC <- ifelse(as.character(loan$grade)=="C", 1, 0)
loan$gradeD <- ifelse(as.character(loan$grade)=="D", 1, 0)
loan$gradeE <- ifelse(as.character(loan$grade)=="E", 1, 0)
loan$gradeF <- ifelse(as.character(loan$grade)=="F", 1, 0)
loan$gradeG <- ifelse(as.character(loan$grade)=="G", 1, 0)

loan$home_ownershipOWN <- ifelse(as.character(loan$home_ownership)=="OWN", 1, 0)
loan$home_ownershipRENT <- ifelse(as.character(loan$home_ownership)=="RENT", 1, 0)


loan_pca <- loan[,c(-4,-5,-7)]

parallel <- fa.parallel(loan_pca, fm = 'minres', fa = 'fa')

twofactor <- fa(loan[,c(1,2,6,8,9,10)],nfactors = 2,rotate = "oblimin",fm="minres")
print(twofactor)

threefactor <- fa(loan[,c(1,2,6,8,9,10)],nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)

fourfactor <- fa(loan[,c(1,2,6,8,9,10)],nfactors = 4)
print(fourfactor)

fivefactor <- fa(loan_pca,nfactors = 5,rotate = "oblimin",fm="minres")
print(fivefactor)


oan_pca <- prcomp(loan[,c(1,2,6,8,9,10)],center=TRUE,scale.=TRUE)
summary(oan_pca)


#suffle and transform class
shuffle_index <- sample(1:nrow(loan))
loan <- loan[shuffle_index, ]


## train + test
index <- sample(c(1:2000), 0.8*2000, replace = F)
train_set <- loan[index, ]
test_set <- loan[-index, ]
dim(train_set)
dim(test_set)

## fit logistic regression with all variables
glm.model <- glm(loan_status~., train_set, family=binomial(link='logit'))
summary(glm.model)

# Prediction on test set
preds = predict(glm.model , test_set , type = 'response')

# Density of probabilities
ggplot(data.frame(preds) , aes(preds)) + 
        geom_density(fill = 'lightblue' , alpha = 0.4) +
        labs(x = 'Predicted Probabilities on test set')

# accuracy
k = 0
accuracy = c()
sensitivity = c()
specificity = c()
for(i in seq(from = 0.01 , to = 0.5 , by = 0.01)){
        k = k + 1
        preds_binomial = ifelse(preds > i , 1 , 0)
        confmat = table(test_set$loan_status , preds_binomial)
        accuracy[k] = sum(diag(confmat)) / sum(confmat)
        sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
        specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
}

threshold = seq(from = 0.01 , to = 0.5 , by = 0.01)

data = data.frame(threshold , accuracy , sensitivity , specificity)
head(data)

# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:4) , 
       aes(x = threshold , y = Value , color = Metric)) + 
        geom_line(size = 1.5)
        
        
        
        

## remove sub_grade (better accuracy)
loan1 <- loan[,c(1:4,6:11)]
train_set1 <- loan1[index, ]
test_set1 <- loan1[-index, ]

glm.model1 <- glm(loan_status~., train_set1, family=binomial(link='logit'))
summary(glm.model1)

# Prediction on test set
preds1 = predict(glm.model1 , test_set1 , type = 'response')

# Density of probabilities
ggplot(data.frame(preds1) , aes(preds1)) + 
        geom_density(fill = 'lightblue' , alpha = 0.4) +
        labs(x = 'Predicted Probabilities on test set')

# accuracy
k = 0
accuracy1 = c()
sensitivity1 = c()
specificity1 = c()
#min(preds1) is 0.03525573
for(i in seq(from = 0.07 , to = 0.5 , by = 0.01)){
        k = k + 1
        preds_binomial1 = ifelse(preds1 > i , 1 , 0)
        confmat1 = table(test_set1$loan_status , preds_binomial1)
        accuracy1[k] = sum(diag(confmat1)) / sum(confmat1)
        sensitivity1[k] = confmat1[1 , 1] / sum(confmat1[ , 1])
        specificity1[k] = confmat1[2 , 2] / sum(confmat1[ , 2])
}

threshold1 = seq(from = 0.07 , to = 0.5 , by = 0.01)

data1 = data.frame(threshold1 , accuracy1 , sensitivity1 , specificity1)
head(data1)
tail(data1)

# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data1 , key = 'Metric' , value = 'Value' , 2:4) , 
       aes(x = threshold1 , y = Value , color = Metric)) + 
        geom_line(size = 1.5)
      



## remove int_rate and sub_grade and other ***
loan2 <- loan[,c(2,4,6,7,10,11)]
loan3 <- loan[,c(4,6,7,9,10,11)]
train_set2 <- loan3[index, ]
test_set2 <- loan3[-index, ]

glm.model2 <- glm(loan_status~., train_set2, family=binomial(link='logit'))
summary(glm.model2)

# Prediction on test set
preds2 = predict(glm.model2 , test_set2 , type = 'response')
preds_binomial2 = ifelse(preds2 > 0.46 , 1 , 0)
test_set2$preds_binomial2 <- preds_binomial2

test_ind <- loan[-index, ]
test_ind$preds_binomial2 <- preds_binomial2

loss_ind <- subset(test_ind, loan_status==0 & preds_binomial2==0)

saved_loan <- sum(loss_ind$loan_amnt)

interest_ind <- subset(test_ind, loan_status==1 & preds_binomial2==0)

loan$term <- ifelse(loan$term=='1',3,5)
interest_ind$term <- ifelse(interest_ind$term=='1',3,5)

interest_loss <- sum(interest_ind$installment*interest_ind$annual_inc*interest_ind$term - interest_ind$loan_amnt)




# Density of probabilities
ggplot(data.frame(preds2) , aes(preds2)) + 
        geom_density(fill = 'lightblue' , alpha = 0.4) +
        labs(x = 'Predicted Probabilities on test set')

# accuracy
k = 0
accuracy2 = c()
sensitivity2 = c()
specificity2 = c()
#min(preds2) is 0.1730369
for(i in seq(from = 0.2 , to = 0.5 , by = 0.01)){
        k = k + 1
        preds_binomial2 = ifelse(preds2 > i , 1 , 0)
        confmat2 = table(test_set2$loan_status , preds_binomial2)
        accuracy2[k] = sum(diag(confmat2)) / sum(confmat2)
        sensitivity2[k] = confmat2[1 , 1] / sum(confmat2[ , 1])
        specificity2[k] = confmat2[2 , 2] / sum(confmat2[ , 2])
}

threshold2 = seq(from = 0.15 , to = 0.5 , by = 0.01)

data2 = data.frame(threshold2 , accuracy2 , sensitivity2 , specificity2)
head(data2)

# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data2 , key = 'Metric' , value = 'Value' , 2:4) , 
       aes(x = threshold2 , y = Value , color = Metric)) + 
        geom_line(size = 1.5)

preds_45 <- ifelse(preds2 > 0.45 , 1 , 0)
confmat2_45 = table(test_set2$loan_status , preds_45)
accuracy2_45 = sum(diag(confmat2_45)) / sum(confmat2_45)
sensitivity2_45 = confmat2_45[1 , 1] / sum(confmat2_45[ , 1])
specificity2_45 = confmat2_45[2 , 2] / sum(confmat2_45[ , 2])


preds_46 <- ifelse(preds2 > 0.46 , 1 , 0)
confmat2_46 = table(test_set2$loan_status , preds_46)
accuracy2_46 = sum(diag(confmat2_46)) / sum(confmat2_46)
sensitivity2_46 = confmat2_46[1 , 1] / sum(confmat2_46[ , 1])
specificity2_46 = confmat2_46[2 , 2] / sum(confmat2_46[ , 2])

preds_47 <- ifelse(preds2 > 0.47 , 1 , 0)
confmat2_47 = table(test_set2$loan_status , preds_47)
accuracy2_47 = sum(diag(confmat2_47)) / sum(confmat2_47)
sensitivity2_47 = confmat2_47[1 , 1] / sum(confmat2_47[ , 1])
specificity2_47 = confmat2_47[2 , 2] / sum(confmat2_47[ , 2])

        

## financial benefits