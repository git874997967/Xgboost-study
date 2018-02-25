# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
#library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(xgboost)
library(caret)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.




#some parts of this code is taken from wti200

# load data


train <- fread('../input/train_2016_v2.csv', showProgress=FALSE)
prop <- fread('../input/properties_2016.csv', showProgress=FALSE)
sub = fread("../input/sample_submission.csv", showProgress=FALSE)

## Data Preparation
prop$hashottuborspa <- ifelse(prop$hashottuborspa == 'true', 1, 0)
prop$fireplaceflag <- ifelse(prop$fireplaceflag == 'true', 1, 0)
prop$taxdelinquencyflag <- ifelse(prop$taxdelinquencyflag == 'Y', 1, 0)
prop$propertycountylandusecode <- as.numeric(as.factor(prop$propertycountylandusecode))
prop$propertyzoningdesc <- as.numeric(as.factor(prop$propertyzoningdesc))

setkey(prop, parcelid)
setkey(train, parcelid)

# join on parcelid
training <- prop[train]
#dropping outliers
training <- training[ training$logerror > -0.4, ]
training <- training[ training$logerror < 0.418, ]
###################
# xgboost set up
###################

target <- training$logerror 

dtrain <- training[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]

feature_names <- names(dtrain)

dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target, missing=NA)

dtest <- xgb.DMatrix(data=as.matrix( prop[,feature_names,with=FALSE]), missing=NA)

####################
# Cross-validation
####################

# Set up cross-validation scheme (3-fold)
foldsCV <- createFolds(target, k=7, list=TRUE, returnTrain=FALSE)

# Set xgboost parameters. These are not necessarily the optimal parameters.
# Further grid tuning is needed. 


param <- list(booster = "gblinear"
              , objective = "reg:linear"
              , subsample = 0.7
              , max_depth = 5
              , colsample_bytree = 0.7
              , eta = 0.037
              , eval_metric = 'mae'
              , base_score = 0.012 #average
              , min_child_weight = 100)


# Perform xgboost cross-validation
# Won't fit under kernel limit. Uncomment to run locally. 
xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                nrounds=100,
                prediction=TRUE,
                maximize=FALSE,
                folds=foldsCV,
                early_stopping_rounds = 30,
                print_every_n = 5
)

# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration

################
# Final model
################

xgb <- xgb.train(params = param
                 , data = dtrain
                # , watchlist = list(train = dtrain)
                 , nrounds = nrounds
                 , verbose = 1
                 , print_every_n = 5
                 #, feval = amm_mae
                )

###############
# Results
###############

# Feature Importance
importance_matrix <- xgb.importance(feature_names,model=xgb)
#xgb.plot.importance(importance_matrix[1:10,])

# Predict
preds <- predict(xgb,dtest)

# For now, use same predictions for each time period. 
results <- data.table(parcelid=prop$parcelid, 
                      '201610'=preds, 
                      '201611'=preds, 
                      '201612'=preds, 
                      '201710'=0,
                      '201711'=0,
                      '201712'=0
)


#Write results to csv
fwrite(results, file='xgboost_submission.csv', row.names=FALSE)