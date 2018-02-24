library(xgboost)
library(tidyverse)
library(utilTools)
library(dplyr)
library(DiagrammeR)
library(stringr)
diseaseInfo = read.csv(file.choose())
str(diseaseInfo)
missValue(diseaseInfo)
set.seed(1234)
##shuffle the dataframe
diseaseInfo = diseaseInfo[sample(1:nrow(diseaseInfo)), ]
###xgboost needs a sparse matrix
diseaseInfo_humanRemoved = diseaseInfo %>% select(-starts_with("human"))
diseaseLabels = diseaseInfo %>% select(humansAffected) %>% is.na() %>% magrittr::not()
head(diseaseLabels)
head(diseaseInfo$humansAffected)
str(diseaseInfo$humansAffected)
###get all numeric vars without long lat and Id
diseaseInfo_numeric = diseaseInfo_humanRemoved %>%
  select(-Id) %>%
  select(-c(longitude, latitude)) %>%
  select_if(is.numeric)
str(diseaseInfo_numeric)
###convert char factors into num
head(diseaseInfo$country)
##convert wuth one-hot encoding
region = model.matrix( ~ country - 1, diseaseInfo)
head(diseaseInfo$speciesDescription)
diseaseInfo_numeric$is_domestic = str_detect(diseaseInfo$speciesDescription, "domestic")
# various species and person sick
speciesList = diseaseInfo$speciesDescription %>%
  str_replace("[[:punct:]]", "") %>%
  str_extract("[a-z]*$")
speciesList = tibble(species = speciesList)
# sp=data.frame(species=speciesList)
options(na.action = 'na.pass')
species = model.matrix( ~ species - 1, speciesList)
diseaseInfo_numeric_combined = cbind(diseaseInfo_numeric, region, species)
diseaseInfo_matrix = data.matrix(diseaseInfo_numeric_combined)
dim(diseaseInfo_matrix)
str(diseaseInfo_humanRemoved)
###To sum up the process above is set some necessary numeric attrs and combined some  necessary one-hotted facotrs
### togather. Trans the data frame into data matrix
#### which means several attrs are filtered out
###but how to tell which should out?
set.seed(54321)
sam = sample(2,
             nrow(diseaseLabels),
             replace = T,
             prob = c(0.85, 0.15))
train_data = diseaseInfo_matrix[sam == 1, ]
test_data = diseaseInfo_matrix[sam == 2, ]
train_labels = diseaseLabels[sam == 1, ]
test_labels = diseaseLabels[sam == 2, 1]
###convert to xgbmatrix
dtrain <- xgb.DMatrix(data = train_data, label = train_labels)
dtest <- xgb.DMatrix(data = test_data, label = test_labels)
typeof(dtrain)
neg=sum(train_labels==F)
pos=sum(train_labels==T)
 
model = xgboost(
  data = dtrain,
  nround = 10,
  early_stopping_rounds = 3,
  max.depth = 3,
  scale_pos_weight =(neg/pos),
  objective = "binary:logistic",
  nthread = 4,
  gamma=0.8
)
pred = predict(model, dtest)
err = mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err, sep = " "))
xgb.plot.multi.trees(feature_names=colnames(diseaseInfo_matrix),model=model)
 importance_matrix=xgb.importance(colnames(diseaseInfo_matrix),model=model)
xgb.plot.importance(importance_matrix)
