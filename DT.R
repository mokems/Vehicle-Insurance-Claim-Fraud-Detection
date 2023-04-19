library(tidyverse) # for data manipulation
library(caret) # for models
library(ROSE) #for over-/under-sampling function
library(precrec) #for Precision-Recall function
library(rpart)
library(randomForest)
library(NeuralNetTools)
library(neuralnet)
library(xgboost)
library(Matrix)


data <- read.csv("/Users/hanqingli/Downloads/fraud_oracle.csv")
train_index <- createDataPartition(data$FraudFound_P, times = 1, p = 0.8, list = F)
train_data <- data[train_index,]
test_data <- data[-train_index,]

table(train_data$FraudFound_P)
table(test_data$FraudFound_P)

set.seed(1234)
train_un <- ovun.sample(FraudFound_P~., data=train_data, p=0.3, method="under")$data
table(train_un$FraudFound_P)
train_un_matrix=data.matrix(train_un[,c(0:29)])
test_un <- ovun.sample(FraudFound_P~., data=test_data, p=0.1, method="under")$data

data[8291,]
data[73,]
data[10991,]
data[215,]
train_un = rbind(train_un,data[8291,])
train_un = rbind(train_un,data[73,])
train_un = rbind(train_un,data[10991,])
train_un = rbind(train_un,data[215,])
train_un = rbind(train_un,data[159,])

train_three = train_un[c(4,14,29,30)]
train_three
test_three = test_un[c(4,14,29,30)]
test_three


regressor <- rpart(
  formula = FraudFound_P ~.,
  data = train_un,
  method = 'class'
)

plot(regressor)
plot(regressor,compress=T,margin=0.3)
text(regressor,cex=1)
regressor


library(rpart.plot)
prp(regressor,type=2,extra=2,digits=3)
#train_result<-predict(regressor, train_un, type = 'class')
#train_un$FraudFound_P

#train_result <- predict(regressor, train_un, type = 'class')
#train_un$FraudFound_P

#table_mat <- table(train_un$FraudFound_P, train_result)

test_result <- predict(regressor, test_un, type = 'class')
test_result
#pred1 <- ifelse(test_result > 0.45, 1, 0)
testset <- as.numeric(as.character(test_data[,c(30)]))
library(caret)
#model_xgb.cf <-caret::confusionMatrix(as.factor(test_result),as.factor(testset), positive = '1')
#model_xgb.cf

#a=data.matrix(train_result)
#test_un$FraudFound_P
set.seed(1234)