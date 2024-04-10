### Quiz 3 ###

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

head(segmentationOriginal)
str(segmentationOriginal)
head(segmentationOriginal$Case)

set.seed(125)
trainIndex = createDataPartition(segmentationOriginal$Case, p = 0.50,list=FALSE)
training = segmentationOriginal[trainIndex,]
testing = segmentationOriginal[-trainIndex,]

nrow(segmentationOriginal); nrow(training); nrow(testing); 

library(caret)

CARTmodel = train(Class ~ .,method="rpart",data=training)  
print(CARTmodel$finalModel)
plot(CARTmodel$finalModel,uniform=TRUE,main="Classification Tree")
text(CARTmodel$finalModel,use.n=TRUE,all=TRUE,cex=0.8)

install.packages("rattle")
library(rattle)
fancyRpartPlot(CARTmodel$finalModel)

### Q3 ###

install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
head(olive)
table(olive$Area)

CART_olive = train(Area ~ .,method="rpart",data=olive)  
print(CART_olive$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
newdata

predict(CART_olive,newdata=newdata)

### Q4 ###

install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
head(trainSA)
str(trainSA)

set.seed(13234)
log_model = glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, family=binomial)
prediction = predict(log_model, newdata=testSA, type="response") 
missClass = function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}
missClass(testSA$chd,prediction)
prediction = predict(log_model, newdata=trainSA, type="response") 
missClass(trainSA$chd,prediction)

### Q5 ###

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
head(vowel.train)

set.seed(33833)
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)
str(vowel.train)
str(vowel.test)
model_rf = train(y ~ .,method="rf",data=vowel.train,prox=TRUE)  
varImp(model_rf)
model_rf_test = train(y ~ .,method="rf",data=vowel.test,prox=TRUE)  
varImp(model_rf_test)
