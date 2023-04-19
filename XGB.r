library(xgboost)
library(rpart)
library(tidyverse) 
library(caret)
library(Matrix)
library(ROSE)
library(smotefamily)
set.seed(1234)
data=read.csv("D:/python test/FITH/project/fraud_oracle.csv")





train_index <- createDataPartition(data$FraudFound_P, times = 1, p = 0.8, list = F)
train_data <- data[train_index,]
train_d_matrix=data.matrix(train_data[,c(0:29)])
test_data <- data[-train_index,]
table(train_data$FraudFound_P)
table(test_data$FraudFound_P)

train_un <- ovun.sample(FraudFound_P~., data=train_data, p=0.45, method="under")$data
train_un_matrix=data.matrix(train_data[,c(0:29)])
table(train_un$FraudFound_P)

traindata2 <- Matrix(train_d_matrix,sparse=T) # 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
traindata3 <- as.numeric(as.character(train_data[,c(30)])) # 将因变量转化为numeric
traindata4 <- list(data=traindata2,label=traindata3) # 将自变量和因变量拼接为list

dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label)


testset1 <- data.matrix(test_data[,c(0:29)]) # 将自变量转化为矩阵
testset2 <- Matrix(testset1,sparse=T) # 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
testset3 <- as.numeric(as.character(test_data[,c(30)])) # 将因变量转化为numeric
testset4 <- list(data=testset2,label=testset3) # 将自变量和因变量拼接为list
dtest <- xgb.DMatrix(data = testset4$data, label = testset4$label) # 构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵





model_xgb <- xgboost(data=dtrain,eta=0.2,booster='gbtree',objective='multi:softmax',nround=5000,num_class=2,n_estimators=800)

pre <- predict(model_xgb,newdata=dtest)

label = getinfo(dtest, "label")
pred <- predict(model_xgb, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

#模型评估
library(caret)
model_xgb.cf <-caret::confusionMatrix(as.factor(pre),as.factor(testset3), positive='1')
model_xgb.cf

require(pROC)
pred2=round(predict(model_xgb,newdata = testset4$data))
xgboost_roc<-roc (testset4$label,as.numeric(pred2),levels = c(0, 1), direction = "<")
plot(xgboost_roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),gird.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE,main='xgboost模型和ROC曲线')




