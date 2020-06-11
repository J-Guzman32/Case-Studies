
#Histograms and Box plots#Book Club Case Study
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(dplyr)
library(e1071)
library(lattice)
library(tidyverse)
library(readxl)
library(MASS)
library(kernlab)

book_Train<- read_xlsx("C:/Users/Jesus Guzman/Desktop/MSDA/Spring 2020/DA6813-Data Analytics Applications/BBBC-Train.xlsx")
book_Test <- read_xlsx("C:/Users/Jesus Guzman/Desktop/MSDA/Spring 2020/DA6813-Data Analytics Applications/BBBC-Test.xlsx")
book_Predic <-read_xlsx("C:/Users/Jesus Guzman/Desktop/MSDA/Spring 2020/DA6813-Data Analytics Applications/BBBC-Predict.xlsx")

#Removing Obs Col
book_Train <- book_Train[,-1]
book_Test <- book_Test[,-1]
book_Predic <- book_Predic[,-1]
str(book_Train)

#Looking for missing observations
anyNA(book_Train)
anyNA(book_Test)
nearZeroVar(book_Train)

#Dummy coding 
book_Train$Gender = as.factor(book_Train$Gender)
book_Test$Gender = as.factor(book_Test$Gender)
book_Predic$Gender = as.factor(book_Predic$Gender)

book_Train$Choice = as.factor(book_Train$Choice)
book_Test$Choice = as.factor(book_Test$Choice)

Num_book <- book_Train[,3:11]
Nom_var <- book_Train[,1:2]
#Correlation Matrix
Cor_book <- cor(Num_book)
corrplot(Cor_book, order = "hclust", tl.cex = .8)
Cor = round(cor(Num_book), 4)
highCor <- findCorrelation(Cor, cutoff = .8)
Nnum_book<- Num_book[,-highCor]

book_Train <-cbind(Nom_var,Nnum_book)
book_Test <- book_Test[,-5]
book_Predic <- book_Predic[,-4]

par(mfrow=c(3,3))
hist(book_Train$Amount_purchased, main = "Amount Purchased", xlab = "")
hist(book_Train$Frequency, main = "Frequency", xlab = "")
hist(book_Train$First_purchase, main = "First Purchase", xlab = "")
hist(book_Train$P_Child, main = "P Child", xlab = "")
hist(book_Train$P_Youth, main = "P Youth", xlab = "")
hist(book_Train$P_Cook, main = "P Cook", xlab = "")
hist(book_Train$P_DIY, main = "P DIY", xlab = "")
hist(book_Train$P_Art, main = "P Art", xlab = "")

par(mfrow=c(3,3))
boxplot(book_Train$Amount_purchased, main = "Amount Purchased", xlab = "")
boxplot(book_Train$Frequency, main = "Frequency", xlab = "")
boxplot(book_Train$First_purchase, main = "First Purchase", xlab = "")
boxplot(book_Train$P_Child, main = "P Child", xlab = "")
boxplot(book_Train$P_Youth, main = "P Youth", xlab = "")
boxplot(book_Train$P_Cook, main = "P Cook", xlab = "")
boxplot(book_Train$P_DIY, main = "P DIY", xlab = "")
boxplot(book_Train$P_Art, main = "P Art", xlab = "")
#Skewness
book_Skewness <- apply(book_Train[,3:10],2, skewness)
book_Skewness
test_skew <- apply(book_Test[,3:10],2, skewness)
test_skew
#Transformations
preb <- preProcess(book_Train, method = c("center", "scale", "spatialSign"))
Nbook_train <- predict(preb, book_Train)
preb_test <- preProcess(book_Test, method = c("center", "scale",  "spatialSign"))
Nbook_Test <- predict(preb_test, book_Test)
preb_predic<- preProcess(book_Predic, method = c("center", "scale",  "spatialSign"))
Nbook_Predic <- predict(preb_predic, book_Predic)

#Skewness After Transformation
book_tr_skew <- apply(Nbook_train[,3:10],2, skewness)
book_tr_skew
testtran_skew <- apply(Nbook_Test[,3:10],2, skewness)
testtran_skew

#Histograms and Box plots after Transformations
par(mfrow=c(3,3))
hist(Nbook_train$Amount_purchased, main = "Amount Purchased", xlab = "")
hist(Nbook_train$Frequency, main = "Frequency", xlab = "")
hist(Nbook_train$First_purchase, main = "First Purchase", xlab = "")
hist(Nbook_train$P_Child, main = "P Child", xlab = "")
hist(Nbook_train$P_Youth, main = "P Youth", xlab = "")
hist(Nbook_train$P_Cook, main = "P Cook", xlab = "")
hist(Nbook_train$P_DIY, main = "P DIY", xlab = "")
hist(Nbook_train$P_Art, main = "P Art", xlab = "")

par(mfrow=c(3,3))
boxplot(Nbook_train$Amount_purchased, main = "Amount Purchased", xlab = "")
boxplot(Nbook_train$Frequency, main = "Frequency", xlab = "")
boxplot(Nbook_train$First_purchase, main = "First Purchase", xlab = "")
boxplot(Nbook_train$P_Child, main = "P Child", xlab = "")
boxplot(Nbook_train$P_Youth, main = "P Youth", xlab = "")
boxplot(Nbook_train$P_Cook, main = "P Cook", xlab = "")
boxplot(Nbook_train$P_DIY, main = "P DIY", xlab = "")
boxplot(Nbook_train$P_Art, main = "P Art", xlab = "")

predict_lm = Nbook_Predic
predict_svm = Nbook_Predic
predict_log = Nbook_Predic

#Change Response Variable into Numeric
Nbook_train$Choice <- as.numeric(Nbook_train$Choice)
Nbook_Test$Choice <- as.numeric(Nbook_Test$Choice)
Nbook_train$Choice <- recode(Nbook_train$Choice,
                               "1"="0",
                               "2"="1")
Nbook_Test$Choice <- recode(Nbook_Test$Choice,
                              "1"="0",
                              "2"="1")
#Linear Regression Model
lin_reg = lm(formula = Choice ~ ., data=Nbook_train)
lin_reg
summary(lin_reg)

lm.prob <- predict(lin_reg, Nbook_Test, type = "response")

predict_lm$PV1 <- predict(lin_reg, Nbook_Predic)
predict_lm$PV1 <- ifelse(predict_lm$PV1 >= .5, 1, 0)

#Revert Response Variable to factor
Nbook_train$Choice <- as.factor(Nbook_train$Choice)
Nbook_Test$Choice <- as.factor(Nbook_Test$Choice)

#Logistic Model
set.seed(42)
log_reg = glm(formula = Choice ~ ., data=Nbook_train, family="binomial")
log_reg
summary(log_reg)

set.seed(42)
log_prob <- predict(log_reg, Nbook_Test, type = "response")
log_probs.Class <- ifelse(log_prob >= .5, 1, 0)
confusionMatrix(as.factor(log_probs.Class),Nbook_Test$Choice)

predict_log$PV2 <- predict(log_reg, Nbook_Predic)
predict_log$PV2 <- ifelse(predict_log$PV2 >= .5, 1, 0)

#Support Vector Machines Model
set.seed(42)
form <- Choice ~.
tuned = tune.svm(form, data = Nbook_train, gamma = seq(.01,.1, by= .01), cost = seq(.01,.1,by=.01))
tuned$best.parameters
svm_model = svm(form,
                  data = Nbook_train,
                  kernel = 'poly',
                  gamma = tuned$best.parameters$gamma,
                  degree = 4,
                  coef0 = 0,
                  cost = tuned$best.parameters$cost) 
summary(svm_model)

svmProbs <- predict(svm_model, Nbook_Test)
confusionMatrix(as.factor(svmProbs),Nbook_Test$Choice)

predict_svm$PV3 <- predict(svm_model, Nbook_Predic)
#Comparing Tables
table(predict_lm$PV1)

table(predict_log$PV2)
table(predict_svm$PV3)

table(book_Test$Choice)

#Check Accuracy
pred1 <- predict_lm$PV1
true1 <- book_Test$Choice
(sum(pred1==true1, na.rm=T)) / length(pred1)

pred2 <- predict_log$PV2
true2 <- book_Test$Choice
(sum(pred2==true2, na.rm=T)) / length(pred2)

pred3 <- predict_svm$PV3
true3 <- book_Test$Choice
(sum(pred3==true3, na.rm=T)) / length(pred3)

#Variable Importance
logvar <- varImp(log_reg, scale=FALSE)
logvar

lmvar <- varImp(lin_reg, scale=FALSE)
lmvar
