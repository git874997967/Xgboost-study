#
# this is one example with features below:
#   cv  and fold selection
#   multi classification
#   save csv with no quote and row.names query
#   learned alot
#

library(xgboost)
library(methods)
library(reshape2)
library(dplyr)
library(tidyverse)
### read data to the R concle ???
featureclass = rep("numeric", 93)
colclasstrain = c('integer', featureclass, 'character')
colclasstest = c('integer', featureclass)
train = read.csv(file.choose(), header = T, colClasses = colclasstrain)
test = read.csv(file.choose(), header = T, colClasses = colclasstest)
###keep the record
id = test[, 1]
###remove the id col
train = train[, -1]
test = test[, -1]
###convert the target from char into int starting from 0
target = train$target
t = levels(train$target)
classnames = unique(target)
target = as.integer(colsplit(target, '_', names = c('x1', 'x2'))[, 2]) -
  1
table(target)
# t=train$target%>%str_replace("Class_","")%>%as.integer
##remove the target from the train
train = train[, -ncol(train)]
## convert into numeric format
trainMatrix = data.matrix(train)
testMatrix = data.matrix(test)
trainMatrix = scale(trainMatrix)
testMatrix = scale(testMatrix)
###cv  to choose the parameters
numberOfClasses = max(target) + 1
numberOfClasses
param <- list(
  "objective" = "multi:softprob",
  "eval_metric" = "mlogloss",
  "num_class" = numberOfClasses
)
# ####
# cv.nround = 200
# cv.nfold = 10
# 
# bst.cv = xgb.cv(
#   param = param,
#   data = trainMatrix,
#   label = target,
#   nfold = cv.nfold,
#   nrounds = cv.nround
# )
# 
# ##get the nround
# nround <-
#   which(
#     bst.cv$evaluation_log$test_mlogloss_mean == min(bst.cv$evaluation_log$test_mlogloss_mean)
#   )
# min(bst.cv$evaluation_log$test_mlogloss_mean)
 
###train the model
nround = 191
bst = xgboost(
  data = trainMatrix,
  label = target,
  param = param,
  nrounds = nround
)
ypred = predict(bst, testMatrix)
###prepare the output
predMatrix = data.frame(matrix(ypred, ncol = 9, byrow = T))
colnames(predMatrix) = classnames
res = data.frame(id, predMatrix)

write.csv(res, "submission.csv", quote = F, row.names = F)
