### Packages ###

library(caTools)
library(rpart)      
library(rpart.plot)
library(ROCR) 
library(caret)
library(e1071)
install.packages("rattle")
install.packages("pROC")
library(rattle)
library(klaR)

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

# The goal of your project is to predict the manner in which they did the exercise
# This is the "classe" variable in the training set. You may use any of the other 
# variables to predict with.

# You should create a report describing how you built your model, how you used cross 
# validation, what you think the expected out of sample error is, and why you made the 
# choices you did. You will also use your prediction model to predict 20 different test cases. 

# yaw, role, pitch
# forearm, dumbbell, belt, 
# min, max, avg, amplitude, kurtosis, skewness

setwd("~/Desktop/K's/Training/Machine Learning John Hopkins/Project")

## Read in data ##

train = read.csv("pml-training.csv",sep=",",stringsAsFactors=FALSE)
#train = read.csv("pml-training.csv")
head(train)
str(train)
summary(train)
table(train$user_name)
table(train$classe)
train$user_name = as.factor(train$user_name)
train$classe = as.factor(train$classe)
train$new_window = as.factor(train$new_window)
table(train$user_name)
train$date = substr(train$cvtd_timestamp,1,10)
table(train$date,train$classe)

### Examine the time situation ###

table(train$cvtd_timestamp)
table(train$raw_timestamp_part_2)

#train$date = as.Date(as.character(train$raw_timestamp_part_1), "%d%m%Y")
#head(train$date)
           
### Missing ###           

keep = c("X","user_name","cvtd_timestamp",
         "new_window","num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt",
         "gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x",
         "accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z",
         "roll_arm","pitch_arm","yaw_arm","total_accel_arm","gyros_arm_x","gyros_arm_y",
         "gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y",
         "magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell",
         "total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y","gyros_dumbbell_z","accel_dumbbell_x",
         "accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x","magnet_dumbbell_y","magnet_dumbbell_z",
         "roll_forearm","pitch_forearm","yaw_forearm","total_accel_forearm",
         "gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x","accel_forearm_y", 
         "accel_forearm_z","magnet_forearm_x","magnet_forearm_y","magnet_forearm_z","classe")             
train = train[,(names(train) %in% keep)]
str(train)
summary(train)

test = read.csv("pml-testing.csv")
test = test[,(names(test) %in% keep)]
test$date = substr(test$cvtd_timestamp,1,10)
head(test)
str(test)
summary(test)
nrow(test)

table(test$user_name)

### Partitioning the data ###

partition = createDataPartition(train$date,p=0.5,list=FALSE)
part_train = train[partition,]
nrow(train); nrow(part_train)
table(part_train$date)
table(part_train$date,part_train$classe)

### Check correlation between predictor variables ###

drops = c("classe","cvtd_timestamp","user_name","X")
cor_var = train[,!(names(train) %in% drops)]
str(cor_var)
cor_var = data.matrix(cor_var)
cor_var
M = abs(cor(cor_var))
diag(M) = 0
M
which(M > 0.8,arr.ind=T)

### Multinomial logistic regression ###

train$classe2 <- relevel(train$classe, ref = "A")
mlogreg <- multinom(classe2 ~ . -X -cvtd_timestamp, data = train)
ncol(train)
summary(mlogreg)

cv <- cv.glmnet(train,classe,family="multinomial",nfolds=50,standardize=FALSE)

## CART model ##

ctrl = trainControl(method="cv",number=50)
CARTmodel = train(classe ~ . -X -cvtd_timestamp -date,method="rpart",data=train,trControl=ctrl) 
CARTmodel
print(CARTmodel$finalModel)
plot(CARTmodel$finalModel,uniform=TRUE,main="Classification Tree")
text(CARTmodel$finalModel,use.n=TRUE,all=TRUE,cex=0.8)

fancyRpartPlot(CARTmodel$finalModel)
test_predict = predict(CARTmodel,newdata=test)
length(test_predict)
table(test_predict)
table(test$classe,predict(CARTmodel,newdata=test))

### Random Forest model ###

summary(train)
nrow(train)

ctrl = trainControl(method="cv",number=50)
model_rf = train(classe ~ . -X -cvtd_timestamp -date,method="rf",data=part_train,prox=TRUE,
                 trControl=ctrl)  
model_rf
test_predict_rf = predict(model_rf,newdata=test)
table(test_predict_rf)

### Boosting with trees ###

model_btree = train(classe ~ . -X -cvtd_timestamp -date,method="gbm",data=part_train,verbose=FALSE)
model_btree
pbtree = predict(model_btree,test)
pbtree
table(pbtree)

### LDA ###

ctrl = trainControl(method="cv",number=50)
model_lda = train(classe ~ . -X -cvtd_timestamp -date,data=part_train,method="lda",trControl=ctrl)
model_lda
summary(model_lda)
varImp(model_lda, scale = FALSE)
plda = predict(model_lda,test)
plda
table(plda)

### Naive Bayes ###

ctrl = trainControl(method="cv",number=50)
model_nb = train(classe ~ . -X -cvtd_timestamp -date,data=part_train,method="nb",trControl=ctrl)
model_nb
pnb = predict(model_nb,test)
table(pnb)
pnb

### SVM ###

ctrl = trainControl(method="cv",number=50)
model_svmr = train(classe ~ . -X -cvtd_timestamp -date,data=part_train,method="svmRadial",
                  trControl=ctrl)
model_svml = train(classe ~ . -X -cvtd_timestamp -date,data=part_train,method="svmLinear",
                  trControl=ctrl)
model_svm
svm_lin = predict(model_svml,test)
svm_lin
table(svm_lin)
svm_rad = predict(model_svmr,test)
svm_rad
table(svm_rad)

### PCA analysis ###

dropsa = c("classe","cvtd_timestamp","raw_timestamp_part_2","raw_timestamp_part_1","user_name","X",
           "classe","new_window")
pca_data = train[,!(names(train) %in% dropsa)]
pca_test_data = test[,!(names(test) %in% dropsa)]
str(pca_data)
prComp = prcomp(pca_data)
plot(prComp$x[,1],prComp$x[,2],col=train$classe)
prComp$rotation                    ### Get the specific attributes for the variables ###

model_pca = preProcess(pca_data,method="pca",pcaComp=2)
train_pca = predict(model_pca,pca_data)
modelFit_pca = train(train$classe ~ .,method="glm",data=train_pca)
test_pca = predict(model_pca,pca_test_data)
plot(test_pca)
text(test_pca,row.names(test_pca))

head(test_pca)

### Functions ###
    
inTrain = createDataPartition(y= ,p= ,list= )    ### Create test/training sets ###
training = dataset[inTrain,]

folds = createFolds(y= ,k= ,list=TRUE,returnTrain= )   ### k folds ###
sapply(folds,length)
folds[[1]]

folds = createResample(y= ,times= ,list=TRUE)   ### Resampling ###

### Time slicing ###

train(model,data=train,method="glm")   
confusionMatrix()                                   ### Sensitivity etc. ###

trainControl()      ### cross validation ### ### increase number of samples with large data ###

featurePlot(x= ,y= ,plot= )                         ### Scatter matrix plot ###
qq = qplot(x,y,color=group,data=training)           ### Scatter plot ###
qq + geom_smooth(method="lm",formula=y ~ x)

qplot(x,color=group,data=training,geom="density")   ### Density plot ###
preProcess(,method=c())   ### Standardisation ###
nearZeroVar(data,saveMetrics=TRUE)                  ### Check out variablility of variables ###

### Submission ###

answers = svm_lin
answers

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
