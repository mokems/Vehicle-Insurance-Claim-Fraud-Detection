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


data <- read.csv("/Users/hanqingli/Downloads/train.csv")

a <- sub("Dec","12",data$Month)
a <- sub("Nov","11",a)
a <- sub("Oct","10",a)
a <- sub("Sep","9",a)
a <- sub("Aug","8",a)
a <- sub("Jul","7",a)
a <- sub("Jun","6",a)
a <- sub("May","5",a)
a <- sub("Apr","4",a)
a <- sub("Mar","3",a)
a <- sub("Feb","2",a)
a <- sub("Jan","1",a)

data$Month <- a

data$Month
data$Month <- as.numeric(data$Month)
typeof(data$WeekOfMonth)
data$Month

b <- sub("Monday","1",data$DayOfWeek)
b <- sub("Tuesday","2",b)
b <- sub("Wednesday","3",b)
b <- sub("Thursday","4",b)
b <- sub("Friday","5",b)
b <- sub("Saturday","6",b)
b <- sub("Sunday","7",b)
data$DayOfWeek <- b
data$DayOfWeek <- as.numeric(data$DayOfWeek)
typeof(data$DayOfWeek)
data$DayOfWeek

data$Make
c <- sub("Pontiac","1",data$Make)
c <- sub("Toyota","2",c)
c <- sub("Honda","3",c)
c <- sub("Mazda","4",c)
c <- sub("Chevrolet","5",c)
c <- sub("Ford","6",c)
c <- sub("Accura","7",c)
c <- sub("Saturn","8",c)
c <- sub("Mercury","9",c)
c <- sub("VW","10",c)
c <- sub("Dodge","11",c)
c <- sub("Saab","12",c)
c <- sub("Nisson","13",c)
c <- sub("Porche","14",c)
c <- sub("Jaguar","15",c)
c <- sub("Ferrari","16",c)
c <- sub("BMW","17",c)
c <- sub("Lexus","18",c)
c <- sub("Mecedes","19",c)

data$Make <- c
data$Make <- as.numeric(data$Make)
typeof(data$Make)
table(data$Make)

data$AccidentArea
d <- sub("Urban","1",data$AccidentArea)
d <- sub("Rural","2",d)
data$AccidentArea <- d
data$AccidentArea <- as.numeric(data$AccidentArea)
typeof(data$AccidentArea)
table(data$AccidentArea)

data$DayOfWeekClaimed
b <- sub("Monday","1",data$DayOfWeekClaimed)
b <- sub("Tuesday","2",b)
b <- sub("Wednesday","3",b)
b <- sub("Thursday","4",b)
b <- sub("Friday","5",b)
b <- sub("Saturday","6",b)
b <- sub("Sunday","7",b)
data$DayOfWeekClaimed <- b
data$DayOfWeekClaimed <- as.numeric(data$DayOfWeekClaimed)
typeof(data$DayOfWeekClaimed)
data$DayOfWeekClaimed

data$MonthClaimed
a <- sub("Dec","12",data$MonthClaimed)
a <- sub("Nov","11",a)
a <- sub("Oct","10",a)
a <- sub("Sep","9",a)
a <- sub("Aug","8",a)
a <- sub("Jul","7",a)
a <- sub("Jun","6",a)
a <- sub("May","5",a)
a <- sub("Apr","4",a)
a <- sub("Mar","3",a)
a <- sub("Feb","2",a)
a <- sub("Jan","1",a)
data$MonthClaimed <- a
data$MonthClaimed <- as.numeric(data$MonthClaimed)
typeof(data$MonthClaimed)
table(data$MonthClaimed)

data$Sex
e <- sub("Male","1",data$Sex)
e <- sub("Female","2",e)
data$Sex <- e
data$Sex <- as.numeric(data$Sex)
typeof(data$Sex)
table(data$Sex)
data$Sex

data$MaritalStatus
f <- sub("Single","1",data$MaritalStatus)
f <- sub("Married","2",f)
f <- sub("Divorced","3",f)
f <- sub("Widow","4",f)
data$MaritalStatus <- f
data$MaritalStatus <- as.numeric(data$MaritalStatus)
typeof(data$MaritalStatus)
table(data$MaritalStatus)
data$MaritalStatus

data$Fault
g <- sub("Policy Holder","1",data$Fault)
g <- sub("Third Party","2",g)
data$Fault <- g
data$Fault <- as.numeric(data$Fault)
typeof(data$Fault)
table(data$Fault)
data$Fault

table(data$VehicleCategory)
data$VehicleCategory
h <- sub("Sedan","1",data$VehicleCategory)
h <- sub("Sport","2",h)
h <- sub("Utility","3",h)
data$VehicleCategory <- h
data$VehicleCategory <- as.numeric(data$VehicleCategory)
typeof(data$VehicleCategory)
table(data$VehicleCategory)
data$VehicleCategory

table(data$BasePolicy)
data$BasePolicy
i <- sub("All Perils","1",data$BasePolicy)
i <- sub("Collision","2",i)
i <- sub("Liability","3",i)
data$BasePolicy <- i
data$BasePolicy <- as.numeric(data$BasePolicy)
typeof(data$BasePolicy)
table(data$BasePolicy)
data$BasePolicy

table(data$Days_Policy_Claim)
data$Days_Policy_Claim
j <- sub("15 to 30","22",data$Days_Policy_Claim)
j <- sub("8 to 15","12",j)
j <- sub("more than 30","31",j)
data$Days_Policy_Claim <- j
data$Days_Policy_Claim <- as.numeric(data$Days_Policy_Claim)
typeof(data$Days_Policy_Claim)
table(data$Days_Policy_Claim)
data$Days_Policy_Claim

table(data$Days_Policy_Accident)
data$Days_Policy_Accident
k <- sub("15 to 30","22",data$Days_Policy_Accident)
k <- sub("8 to 15","12",k)
k <- sub("1 to 7","4",k)
k <- sub("none","0",k)
k <- sub("more than 30","31",k)
data$Days_Policy_Accident <- k
data$Days_Policy_Accident <- as.numeric(data$Days_Policy_Accident)
typeof(data$Days_Policy_Accident)
table(data$Days_Policy_Accident)
data$Days_Policy_Accident

table(data$PastNumberOfClaims)
data$PastNumberOfClaims
l <- sub("1","1",data$PastNumberOfClaims)
l <- sub("2 to 4","3",l)
l <- sub("more than 4","5",l)
l <- sub("none","0",l)
data$PastNumberOfClaims <- l
data$PastNumberOfClaims <- as.numeric(data$PastNumberOfClaims)
typeof(data$PastNumberOfClaims)
table(data$PastNumberOfClaims)
data$PastNumberOfClaims

table(data$AgeOfPolicyHolder)
data$AgeOfPolicyHolder
m <- sub("18 to 20","19",data$AgeOfPolicyHolder)
m <- sub("21 to 25","23",m)
m <- sub("26 to 30","28",m)
m <- sub("31 to 35","33",m)
m <- sub("36 to 40","38",m)
m <- sub("41 to 50","45",m)
m <- sub("51 to 65","58",m)
m <- sub("over 65","66",m)
data$AgeOfPolicyHolder <- m
data$AgeOfPolicyHolder <- as.numeric(data$AgeOfPolicyHolder)
typeof(data$AgeOfPolicyHolder)
table(data$AgeOfPolicyHolder)
data$AgeOfPolicyHolder

table(data$PoliceReportFiled)
n <- sub("No","0",data$PoliceReportFiled)
n <- sub("Yes","1",n)
data$PoliceReportFiled <- n
data$PoliceReportFiled <- as.numeric(data$PoliceReportFiled)
typeof(data$PoliceReportFiled)
table(data$PoliceReportFiled)
data$PoliceReportFiled

table(data$WitnessPresent)
n <- sub("No","0",data$WitnessPresent)
n <- sub("Yes","1",n)
data$WitnessPresent <- n
data$WitnessPresent <- as.numeric(data$WitnessPresent)
typeof(data$WitnessPresent)
table(data$WitnessPresent)
data$WitnessPresent

table(data$AgentType)
o <- sub("External","0",data$AgentType)
o <- sub("Internal","1",o)
data$AgentType <- o
data$AgentType <- as.numeric(data$AgentType)
typeof(data$AgentType)
table(data$AgentType)
data$AgentType

table(data$NumberOfSuppliments)
o <- sub("1 to 2","2",data$NumberOfSuppliments)
o <- sub("3 to 5","4",o)
o <- sub("more than 5","6",o)
o <- sub("none","0",o)
data$NumberOfSuppliments <- o
data$NumberOfSuppliments <- as.numeric(data$NumberOfSuppliments)
typeof(data$NumberOfSuppliments)
table(data$NumberOfSuppliments)
data$NumberOfSuppliments

table(data$AddressChange_Claim)
p <- sub("1 year","1",data$AddressChange_Claim)
p <- sub("2 to 3 years","2",p)
p <- sub("4 to 8 years","6",p)
p <- sub("no change","0",p)
p <- sub("under 6 months","3",p)
data$AddressChange_Claim <- p
data$AddressChange_Claim <- as.numeric(data$AddressChange_Claim)
typeof(data$AddressChange_Claim)
table(data$AddressChange_Claim)
data$AddressChange_Claim

table(data$NumberOfCars)
q <- sub("1 vehicle","1",data$NumberOfCars)
q <- sub("2 vehicles","2",q)
q <- sub("3 to 4","3",q)
q <- sub("5 to 8","6",q)
q <- sub("more than 8","9",q)
data$NumberOfCars <- q
data$NumberOfCars <- as.numeric(data$NumberOfCars)
typeof(data$NumberOfCars)
table(data$NumberOfCars)
data$NumberOfCars


table(data$VehiclePrice)
r <- sub("20000 to 29000","25000",data$VehiclePrice)
r <- sub("30000 to 39000","35000",r)
r <- sub("40000 to 59000","50000",r)
r <- sub("60000 to 69000","65000",r)
r <- sub("less than 20000","10000",r)
r <- sub("more than 69000","70000",r)
data$VehiclePrice <- r
data$VehiclePrice <- as.numeric(data$VehiclePrice)
typeof(data$VehiclePrice)
table(data$VehiclePrice)
data$VehiclePrice

table(data$AgeOfVehicle)
z <- sub("2 years","2",data$AgeOfVehicle)
z <- sub("3 years","3",z)
z <- sub("4 years","4",z)
z <- sub("5 years","5",z)
z <- sub("6 years","6",z)
z <- sub("7 years","7",z)
z <- sub("more than 7","8",z)
z <- sub("new","0",z)
data$AgeOfVehicle <- z
data$AgeOfVehicle <- as.numeric(data$AgeOfVehicle)
typeof(data$AgeOfVehicle)
table(data$AgeOfVehicle)
data$AgeOfVehicle


typeof(data)
data
#data = data[c(4,14,29,30)]

train_index <- createDataPartition(data$FraudFound_P, times = 1, p = 0.8, list = F)
train_data <- data[train_index,]
test_data <- data[-train_index,]

table(train_data$FraudFound_P)
table(test_data$FraudFound_P)

#set.seed(1234)
train_un <- ovun.sample(FraudFound_P~., data=train_data, p=0.4, method="under")$data
table(train_un$FraudFound_P)
train_un_matrix=data.matrix(train_un[,c(0:29)])
#train_un_matrix=data.matrix(train_un[,c(0:4)])
test_un <- ovun.sample(FraudFound_P~., data=test_data, p=0.1, method="under")$data
test_three = test_data[c(4,12,13,14,29,30)]
data[8291,]
data[73,]
data[10991,]
data[215,]
train_un = rbind(train_un,data[8291,])
train_un = rbind(train_un,data[73,])
train_un = rbind(train_un,data[10991,])
train_un = rbind(train_un,data[215,])
train_un = rbind(train_un,data[159,])
summary(train_un)
#train_three = train_un[c(4,12,13,14,29,30)]
train_three = data[c(4,12,13,14,29,30)]
summary(train_three)
train_three$Make <- (train_three$Make - min(train_three$Make)) / (max(train_three$Make) - min(train_three$Make))
hist(train_three$Make)
train_three$Fault <- (train_three$Fault - min(train_three$Fault)) / (max(train_three$Fault) - min(train_three$Fault))
train_three$BasePolicy <- (train_three$BasePolicy - min(train_three$BasePolicy)) / (max(train_three$BasePolicy) - min(train_three$BasePolicy))
train_three$VehicleCategory <- (train_three$VehicleCategory - min(train_three$VehicleCategory)) / (max(train_three$VehicleCategory) - min(train_three$VehicleCategory))
train_three$VehiclePrice <- (train_three$VehiclePrice - min(train_three$VehiclePrice)) / (max(train_three$VehiclePrice) - min(train_three$VehiclePrice))

test_three$Make <- (test_three$Make - min(test_three$Make)) / (max(test_three$Make) - min(test_three$Make))
hist(test_three$Make)
test_three$Fault <- (test_three$Fault - min(test_three$Fault)) / (max(test_three$Fault) - min(test_three$Fault))
test_three$BasePolicy <- (test_three$BasePolicy - min(test_three$BasePolicy)) / (max(test_three$BasePolicy) - min(test_three$BasePolicy))
test_three$VehicleCategory <- (test_three$VehicleCategory - min(test_three$VehicleCategory)) / (max(test_three$VehicleCategory) - min(test_three$VehicleCategory))
test_three$VehiclePrice <- (test_three$VehiclePrice - min(test_three$VehiclePrice)) / (max(test_three$VehiclePrice) - min(test_three$VehiclePrice))
table(test_three$FraudFound_P)
table(train_three$class)

#m <- colMeans(train_three)
#s <- apply(train_three, 2, sd)
#training <- scale(train_three, center = m, scale = s)
#test <- scale(test, center = m, scale = s)
#train[6] <- train_three[6]

library(neuralnet)
#set.seed(333)
n <- neuralnet(class~.,
               data = train_three,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 6,
               algorithm = "rprop+",
               stepmax = 100000)
#plot(n, rep = 1)
plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')
n$result.matrix

output <- compute(n, rep = 1, test_three[, -1])
head(output$net.result)

p1 <- output$net.result
table(p1)
pred1 <- ifelse(p1 > 0.64, 1, 0)
#plot(test_three$FraudFound_P, pred1) 
tab1 <- table(pred1, test_three$FraudFound_P)
tab1
aaaa =as.matrix(tab1)
require(pROC)
#pred2=round(predict(model_xgb,newdata = testset4$data))
xgboost_roc<-roc(test_three$FraudFound_P,as.numeric(pred1),levels = c(0, 1), direction = "<")
plot(xgboost_roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),gird.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE,main='ROC curve of Neural Network')

model_xgb.cf <-caret::confusionMatrix(as.factor(pred1),as.factor(test_three$FraudFound_P), positive = '1')
model_xgb.cf

library(gplots)
library(RColorBrewer)

#display.brewer.all() #显示所有调色板
coul <- colorRampPalette(brewer.pal(9, "Paired"))(20)#换个好看的颜色
hM <- format(round(aaaa, 2))#对数据保留2位小数

heatmap.2(aaaa,
          trace="none",#不显示trace
          col=coul,#修改热图颜色
          density.info = "none",#图例取消density
          key.xlab ='Correlation',
          key.title = "",
          cexRow = 1,cexCol = 1,#修改横纵坐标字体
          Rowv = F,Colv = F, #去除聚类
          margins = c(6, 6),
          cellnote = hM,notecol='black'#添加相关系数的值及修改字体颜色
)
test_result_num = test_three$FraudFound_P
library(pROC)
as.numeric(pred1)
modelroc <- roc(test_three$FraudFound_P,as.numeric(p1))
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)


library(modEvA)
library(data.table)
pr_auc <- AUC(obs = test_three$FraudFound_P, pred = p1, curve = 'PR')
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
  ggtitle("Precision-Recall for different threshold values (Neural Network)") +
  xlab("Threshold")+ylab("Precision-Recall")


