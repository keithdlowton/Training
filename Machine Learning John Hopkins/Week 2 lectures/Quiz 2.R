# Q1

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

nrow(adData); nrow(training); nrow(testing); 

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

nrow(adData); nrow(train); nrow(test); 

# Q2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

nrow(mixtures); nrow(training); nrow(testing); 
head(mixtures)
head(training)
str(training)

mixtures$SuperPlasticizer = as.numeric(mixtures$SuperPlasticizer)
table(mixtures$Superplasticizer)
hist(training$Superplasticizer)
hist(mixtures$Superplasticizer)

# Q3

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

str(adData)
pred = training[,grep("^[IL]", names(training), value=TRUE)]
str(pred)
pred1 = pred[,-c(1,2,3,16:21)]
str(pred1)

prComp = prcomp(pred1)
prComp

screeplot(prComp, type="lines",col=3)
prComp$sdev/sum(prComp$sdev)
cumsum(prComp$sdev/sum(prComp$sdev))

# Q4

data1 = cbind(pred1,)

preProc = preProcess(pred1,method="pca",pcaComp=7)
trainPC = predict(preProc,pred1)
modelFit = train(training$diagnosis ~ .,method="glm",data=trainPC)
summary(modelFit)
confusionMatrix(training$diagnosis,predict(modelFit,trainPC))
