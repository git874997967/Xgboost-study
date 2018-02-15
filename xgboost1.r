library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(dplyr)
library(sqldf)
library(mice)
library(rpart)
library("VIM")
library(varhandle)
library(Matrix)
# xgboost   极端梯度上升   类似  梯度上升框架  但是更高效
# 结合 线性学习 和  树模型  在单机上也有并行计算的能力
# 预测性能强大  过程缓慢
#  仅仅适用于数值型向量   使用如下函数
#te=as.numeric(iris$Species)
#
# sparse_matrix <- Matrix::sparse.model.matrix(Species ~ ., data = iris)
# sparse_matrix
set.seed(100)
## the key for this is read char as factor when use file.choose   big problem
df_train = read_csv("train_users_2.csv")
df_test = read_csv("test_users.csv")
labels=df_train["Loan_Status"]
df_train=df_train[,1:12]
str(df_train)
df_all=rbind(df_train,df_test)
edit(df_all)
nrow(df_all[is.na(df_all$Gender),])
# cleanning basic
df_all$Gender[is.na(df_all$Gender)] = "Male"
df_all$Married[is.na(df_all$Married)] = "No"
df_all$Self_Employed[is.na(df_all$Self_Employed)] = "No"

df_all$LoanAmount[is.na(df_all$LoanAmount)] = mean(df_all$LoanAmount, na.rm = TRUE)
df_all$Loan_Amount_Term[is.na(df_all$Loan_Amount_Term)] = 360
df_all$Credit_History[is.na(df_all$Credit_History)] = 1
df_all$Dependents[is.na(df_all$Dependents)] = 0

##  进行热编码   文字描述类型   数字类型的向量 不作处理

ohe_feats = c("Gender", "Married", "Education", "Self_Employed", "Property_Area")
dummies <- dummyVars(~ Gender + Married + Education + Self_Employed + Property_Area, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

#Split train and test data set

X = df_all_combined[df_all_combined$Loan_ID %in% df_train$Loan_ID,]
y <- recode(labels$Loan_Status,"'Y'=1; 'N'=0")
X_test = df_all_combined[df_all_combined$Loan_ID %in% df_test$Loan_ID,]

nrow(X)
nrow(X_test)
xgb <- xgboost(data = data.matrix(X[,-1]), 
               label = y, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)
y_pred <- predict(xgb, data.matrix(X_test[,-1]))
model <- xgb.dump(xgb, with.stats = T)
model[1:10] 
names <- dimnames(data.matrix(X[,-1]))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
importance_matrix
xgb.plot.importance(importance_matrix[1:10,])
sessionInfo()
