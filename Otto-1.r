featureclass = rep("numeric", 93)
colclasstrain = c('integer', featureclass, 'character')
train = read.csv(file.choose(), header = T, colClasses = colclasstrain)
colclasstest = c('integer', featureclass)
test = read.csv(fill.choose(), header = T, colClasses = colclasstest)
id = test[, 1]
train = train[, -c(1, ncol(train))]
test = test[, -1]
target = train$target %>% str_replace("Class_", "") %>% as.integer
###into matrix
trainMatrix = train %>% data.matrix %>% scale
testMatrix = test %>% data.matrix %>% scale
nclass = 9
###the model with multi classificaton
param = list(
  "objective" = "multi:softprob",
  "eval_metric" = "mlogloss",
  "num_class" = nclass
)
cv.nround = 500
cv.nfold = 10
bst.cv = xgb.cv(
  param,
  trainMatrix,
  label = target,
  nfold = cv.nfold,
  nrounds = cv.nround
)
nround = which(
  bst.cv$evaluation_log$test_mlogloss_mean == min(bst.cv$evaluation_log$test_mlogloss_mean)
)
bst = xgboost(
  data = trainMatrix,
  param = param,
  label = target,
  nrounds = nround,
  
)
pred = predict(bst, testMatrix)

predMatrix = data.frame(matrix(pred, ncol = nclass, byrow = T))

colnames(predMatrix) = unique(train$target)

res = data.frame(id, predMatrix)

###no quote query  no rowname delete
write.csv(res, "sub_191.csv", quote = F, row.names = F)
