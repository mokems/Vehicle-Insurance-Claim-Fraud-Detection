library(randomForest)
library(rpart)
library(tidyverse) 
library(caret)
library(Matrix)
library(ROSE)
library(smotefamily)

set.seed(1234)
data=read.csv("D:/python test/FITH/project/feature_selected.csv")





train_index <- createDataPartition(data$class, times = 1, p = 0.8, list = F)
train_data <- data[train_index,]
train_d_matrix=data.matrix(train_data[,c(0:30)])
test_data <- data[-train_index,]
table(train_data$class)
table(test_data$class)

train_un <- ovun.sample(class~., data=train_data, p=0.3, method="under")$data

table(train_un$class)
train_data$class<-as.factor(train_un$class)
data$class <- as.factor(data$class)
train_un$class <- as.factor(train_un$class)
table(train_un$FraudFound_P)


print(train_data[,c(0:30)]

bestmtry <- tuneRF(train_un[,c(0:29)],train_un$class,mtryStart = 5, 
                   ntreeTry=1000, 
                   stepFactor = 1.2, 
                   improve = 0.0001, 
                   trace=TRUE, 
                   plot = TRUE,
                   doBest = TRUE,
                   nodesize = 30, 
                   importance=TRUE) 
pre_best<-predict(bestmtry,newdata=test_data)
pre_best[pre_best>=0.5]=1
pre_best[pre_best<0.5]=0

model_xgb.cf <-caret::confusionMatrix(as.factor(pre_best),as.factor(test_data$class), positive='1')
model_xgb.cf


test_result_num = test_data$class
library(pROC)
as.numeric(pre_best)
modelroc <- roc(test_data$class,as.numeric(pre_best))
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

library(modEvA)
library(data.table)
library(ggplot2)
pr_auc <- AUC(obs = test_data$class, pred = pre_best, curve = 'PR')
pr_auc_data <- data.table(precision = pr_auc$thresholds$precision,
                          recall = pr_auc$thresholds$sensitivity,
                          thresholds = pr_auc$thresholds,
                          prauc = pr_auc$AUC)
# 画图

times <- seq(from=0, to=1, by=0.01)
df <- data.frame(thresholds = times, value = pr_auc$thresholds$sensitivity)
ggplot(data = df, mapping = aes(x = times, y = value)) + geom_line()

dd <- data.frame(thresholds = times, pr_auc$thresholds$sensitivity, pr_auc$thresholds$precision)
ggplot()+geom_line(data = dd,aes(x = times,y = pr_auc$thresholds$sensitivity,colour = "Recall"),size=2)+
  geom_line(data = dd,aes(x = times,y = pr_auc$thresholds$precision,colour ="Precision"),size=2) + 
  scale_colour_manual("",values = c("Recall" = "blue","Precision" = "red"))+
  ggtitle("Precision-Recall for different threshold values (Random Forest)") +
  xlab("Threshold")+ylab("Precision-Recall")
