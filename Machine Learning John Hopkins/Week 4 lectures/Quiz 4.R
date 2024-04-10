library(ElemStatLearn)
library(caret)
install.packages("forecast")
library(forecast)
library(e1071)

setwd("~/Desktop/K's/Training/Machine Learning John Hopkins/Week 4 lectures")

data(vowel.train)
data(vowel.test) 

head(vowel.train)
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)
str(vowel.train)

set.seed(33833)
model_rf = train(y ~ .,method="rf",data=vowel.train,prox=TRUE)
model_rf
pbrf = predict(model_rf,vowel.test)
tab_rf = table(pbrf,vowel.test$y)
sum(diag(tab_rf))/nrow(vowel.test)
model_btree = train(y ~ .,method="gbm",data=vowel.train,verbose=FALSE)
pbtree = predict(model_btree,vowel.test)
tab_btree = table(pbtree,vowel.test$y)
sum(diag(tab_btree))/nrow(vowel.test)

predDF = data.frame(pbrf,pbtree,y=vowel.test$y)
combmodfit = train(y ~.,method="gam",data=predDF)
pbcomb = predict(combmodfit,vowel.test)
tab_comb = table(pbcomb,vowel.test$y)
sum(diag(tab_comb))/nrow(vowel.test)

### Q2 ###

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
model_rf = train(diagnosis ~ .,method="rf",data=training,prox=TRUE)
pb_rf = predict(model_rf,testing)
tab_rf = table(pb_rf,testing$diagnosis)
sum(diag(tab_rf))/nrow(testing)

model_btree = train(diagnosis ~ .,method="gbm",data=training,verbose=FALSE)
pb_btree = predict(model_btree,testing)
tab_btree = table(pb_btree,testing$diagnosis)
sum(diag(tab_btree))/nrow(testing)

model_lda = train(diagnosis ~ .,data=training,method="lda")
pb_lda = predict(model_lda,testing)
tab_lda = table(pb_lda,testing$diagnosis)
sum(diag(tab_lda))/nrow(testing)

predDF = data.frame(pb_lda,pb_btree,pb_lda,diagnosis=testing$diagnosis)
combmodfit = train(diagnosis ~.,method="rf",data=predDF)
tab_comb = table(pbcomb,testing$diagnosis)
sum(diag(tab_comb))/nrow(testing)

### Q3 ###

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
head(training)
str(training)

set.seed(233)
lasso = train(CompressiveStrength ~ .,data=training,method="lasso", metric="RMSE")
summary(lasso)
object = enet(training$CompressiveStrength,training[,-CompressiveStrength],lambda=1)
plot.enet(lasso)
plot(lasso)

### Q4 ###

install.packages("lubridate")  
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

t_series = bats(tstrain)
plot(forecast(t_series))

### Q5 ###

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
head(testing)
head(training)

svm.model <- svm(CompressiveStrength ~ ., data = training)
svm.pred <- predict(svm.model, testing)
