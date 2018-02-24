###load libs
library(xgboost)
library(dplyr)
library(tidyverse)
library(magrittr)
###1 read the data shuffle the data
diseaseInfo = read.csv(file.choose(), stringsAsFactors = F)
set.seed(1234)
diseaseInfo = diseaseInfo[sample(1:nrow(diseaseInfo)),]
###2 look at the data
head(diseaseInfo)
str(diseaseInfo)
###3 cut unused cols
diseaseInfo_humanRemoved = diseaseInfo %>% select(-starts_with("human"))
####create labels vector
diseaseLabels = diseaseInfo %>% select(humansAffected) %>% is.na() %>%
  magrittr::not()
### keeping remove uncessary cols
diseaseInfo_numeric = diseaseInfo_humanRemoved %>%
  select(-Id) %>%
  select(-c(longitude, latitude)) %>%
  select_if(is.numeric)
str(diseaseInfo_numeric)
###convert to char into onehot encoding  change the factor into dummy vars
str(diseaseInfo$country)
region = model.matrix(~ -1 + country, diseaseInfo)
### one more process to split description in to domestic and wild
diseaseInfo_numeric$is_doestic = str_detect(diseaseInfo$speciesDescription, "domestic")
speciesList = diseaseInfo$speciesDescription %>% str_replace("[[:punct:]]", "") %>%
  ### remove punctuation
  str_extract("[a-z]*$")
##tibble function almost the same as data.frame but does not change the colnames
## more effective
speciesList = tibble(species = speciesList)
### convert speciesList into onehot matirx   but at beginning shouls pass the NA
options(na.action = "na.pass")
speciesList = model.matrix( ~ species, speciesList)
species = speciesList
###now we have numberic matix and two onehot matixs   combie them togather
diseaseInfo_numeric = cbind(diseaseInfo_numeric, region, species)
####train and test dataset
set.seed(4321)
sam = sample(2,
             nrow(diseaseInfo_numeric),
             replace = T,
             prob = c(0.7, 0.3))
diseaseInfo_matrix=data.matrix(diseaseInfo_numeric)
train_data = diseaseInfo_matrix[sam == 1, ]
test_data = diseaseInfo_matrix[sam == 2, ]
train_labels = diseaseLabels[sam == 1, ]
test_labels = diseaseLabels[sam == 2, ]
test_labels=head(diseaseLabels,5138)
dtrain=xgb.DMatrix(data=train_data,label=train_labels)
dtest=xgb.DMatrix(data=test_data)
 pos=sum(train_labels==T)

neg=sum(train_labels==F)
 
### because it is survive or not which means binary logic regression will be used
model=xgboost(data=dtrain,
              nrounds = 30,
            #  early_stopping_rounds = 5,
              scale_pos_weight=(neg/pos),
              objective="binary:logistic",
              gamma=0.7)
pred <- predict(model, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))
### plot the trees
xgb.plot.multi.trees(feature_names = colnames(diseaseInfo_matrix), 
                     model = model)

odds_to_probs <- function(odds){
  return(exp(odds)/ (1 + exp(odds)))
}
odds_to_probs(-0.599)

### generate an importance matrix 
importance_matrix=xgb.importance(names(diseaseInfo_matrix),model=model)
xgb.plot.importance(importance_matrix)
 
result=tibble(humanAffected=pred)
result=round(result)
table(result) 

