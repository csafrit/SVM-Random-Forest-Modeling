library(tidyverse)
library(readr)
library(car)
library(ISLR)
library(kernlab)
library(randomForest)
library(caret)
library(e1071)
library(pROC)
library(dplyr)

dfCS <- read.csv('~/_DSBA_UNCC/DBSA_6211_Advanced_Business_and_Analytics/BostonHousing.csv', na.strings=c('NA',''))
str(dfCS)
summary(dfCS)
dfCS$CAT..MEDV <- as.factor(dfCS$CAT..MEDV)
dfCS$CHAS <- as.factor(dfCS$CHAS)
dfCS = select(dfCS,-13)

trainIndex <- createDataPartition(dfCS$CAT..MEDV,
                                  p=0.7,
                                  list = FALSE,
                                  times=1)
dfCS.train <- dfCS[trainIndex,]
dfCS.valid <- dfCS[-trainIndex,]

#Random Forest Model
rfCS_default <- train(CAT..MEDV~.,
                    data=dfCS.train,
                    method ='rf',
                    metric = 'Accuracy',
                    ntree=50)
print(rfCS_default)

tuneGrid <- expand.grid(.mtry=c(1:17))

rfCS_mtry <- train(CAT..MEDV~.,
                 data=dfCS.train,
                 method='rf',
                 metric = 'Accuracy',
                 tuneGrid=tuneGrid,
                 importance=TRUE,
                 ntree=50)
print(rfCS_mtry)
plot(rfCS_mtry)

prediction <- predict(rfCS_mtry, dfCS.valid)
confusionMatrix(prediction,dfCS.valid$CAT..MEDV)

varImp(rfCS_mtry)
varImpPlot(rfCS_mtry)
class(rfCS_default)

#Criteria 2: the ROC curve and area under the curve
pred.probabilities <- predict(rfCS_mtry,newdata=dfCS.valid,type='prob')

rfCS.ROC <- roc(predictor=pred.probabilities$`1`,
                      response=dfCS.valid$CAT..MEDV,
                      levels=levels(dfCS.valid$CAT..MEDV))
plot(rfCS.ROC)
rfCS.ROC$auc

#SVM

trControl <- trainControl(method='cv',
                          number=10,
                          search='grid')

svmCS_linear <- train(CAT..MEDV~.,
                    data=dfCS.train,
                    method='svmLinear',
                    trControl=trControl,
                    preProcess=c('center','scale'))
print(svmCS_linear)
summary(svmCS_linear)

linear_predCS <- predict(svmCS_linear, dfCS.valid)
confusionMatrix(linear_predCS,dfCS.valid$CAT..MEDV)

svmfitCS = svm(CAT..MEDV~., 
             data = dfCS.valid, 
             kernel = "linear", 
             cost = 0.1, 
             scale = FALSE)

plot(svmfitCS, dfCS.valid)
svmfitCS$index

#Tuning 

set.seed(1)
tune_out = tune(svm, 
                CAT..MEDV~., 
                data = dfCS.valid, 
                kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune_out)
bestmod = tune_out$best.model
summary(bestmod)

print(bestmod)

#Tuning again

svmCS_linear_tune <- train(CAT..MEDV~.,
                           data=dfCS.train,
                           method = 'svmLinear',
                           trControl=trControl,
                           preProcess=c('center','scale'),
                           tuneGrid=grid_linear)

svmCS_linear_tune <- tune.svm(CAT..MEDV~., data = dfCS.train, 
                              cost = 2^(2:8), 
                              kernel = "linear") 

print(svmCS_linear_tune)

#Need to remove missing values from the validation dataset for evaluation
dfCS.valid.nonmissing <- na.omit(dfCS.valid)


#Criteria 2: the ROC curve and area under the curve
pred.probabilities <- predict(as.integer(linear_predCS),newdata=dfCS.valid.nonmissing,type='prob')


svmCS_linear.ROC <- roc(predictor= as.numeric(linear_predCS),
                 response=dfCS.valid.nonmissing$CAT..MEDV,
                 levels=levels(dfCS.valid.nonmissing$CAT..MEDV))
plot(svmCS_linear.ROC)
svmCS_linear.ROC$auc

svmCS_radial <- train(CAT..MEDV~.,
                    data=dfCS.train,
                    method='svmRadial',
                    trControl=trControl,
                    preProcess=c('center','scale'))
print(svmCS_radial)


radial_pred <- predict(svmCS_radial,dfCS.valid)
confusionMatrix(radial_pred,dfCS.valid$CAT..MEDV)

#Criteria 2: the ROC curve and area under the curve
pred.probabilities <- predict(svmCS_radial,newdata=dfCS.valid,type='prob')

svmCS_radial.ROC <- roc(predictor=as.numeric(radial_pred),
                response=dfCS.valid$CAT..MEDV,
                levels=levels(dfCS.valid$CAT..MEDV))
plot(svmCS_radial.ROC)
svmCS_radial.ROC$auc

grid_linear <- expand.grid(sigma = c(0.0,0.5,0.75,1.0,1.3,1.5))


confusionMatrix(svmCS_linear_tune,dfCS.valid$CAT..MEDV)
