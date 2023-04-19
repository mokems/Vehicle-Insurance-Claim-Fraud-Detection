library(corrplot)
library(caret)
library(ggplot2)
library(gridExtra)

data <- read.csv("D:\\HKU Course\\Financial Fraud\\Project\\fraud_oracle.csv")
table(data$ FraudFound_P)/length(data$ FraudFound_P)

dataFraud <- data[data$FraudFound_P == 1,]
dataNotFraud <- data[data$FraudFound_P == 0,]

#corrplot(corr=cor(data[,c(2,8,11,15,16,28,30)]),method = "color", order = "AOE",addCoef.col = "grey")
corrplot(corr=cor(data), tl.col="black", tl.cex=0.5, method = "color", order = "AOE")


#or geom_boxplot()
ggplot(data = data,mapping=aes(x=Month, fill=DayOfWeek))+ geom_bar(stat="count",position="stack")
ggplot(data = data,mapping=aes(x=Year, fill=Month))+ geom_bar(stat="count",position="stack")


table(data $Make)
#Make& AgeOfVehicle
#All data
table(data $Make, data $AgeOfVehicle)
pieAge1<-ggplot(data[data$Make == "Toyota",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$AgeOfVehicle)+labs(title="Toyota")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  #+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieAge2<-ggplot(data[data$Make == "Pontiac",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$AgeOfVehicle)+labs(title="Pontiac ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  #+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieAge3<-ggplot(data[data$Make == "Honda",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$AgeOfVehicle)+labs(title="Honda")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  #+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieAge4<-ggplot(data[data$Make == "Chevrolet",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$AgeOfVehicle)+labs(title="Chevrolet")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
  #+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
grid.arrange(pieAge1,pieAge2,pieAge3,pieAge4,ncol=2,nrow=2)

#Fraud data
pieAgeFraud1<-ggplot(dataFraud[dataFraud$Make == "Toyota",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$AgeOfVehicle)+labs(title="Toyota")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieAgeFraud2<-ggplot(dataFraud[dataFraud$Make == "Pontiac",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$AgeOfVehicle)+labs(title="Pontiac ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
pieAgeFraud3<-ggplot(dataFraud[dataFraud$Make == "Honda",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$AgeOfVehicle)+labs(title="Honda")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
pieAgeFraud4<-ggplot(dataFraud[dataFraud$Make == "Chevrolet",],aes(x=1,fill=AgeOfVehicle))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$AgeOfVehicle)+labs(title="Chevrolet")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
grid.arrange(pieAgeFraud1,pieAgeFraud2,pieAgeFraud3,pieAgeFraud4,ncol=2,nrow=2)

#Make& VehiclePrice
table(data $Make, data $VehiclePrice)
#All data
piePrice1<-ggplot(data[data$Make == "Toyota",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$VehiclePrice)+labs(title="Toyota")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
piePrice2<-ggplot(data[data$Make == "Pontiac",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$VehiclePrice)+labs(title="Pontiac ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
piePrice3<-ggplot(data[data$Make == "Honda",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$VehiclePrice)+labs(title="Honda")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
piePrice4<-ggplot(data[data$Make == "Chevrolet",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$VehiclePrice)+labs(title="Chevrolet")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
grid.arrange(piePrice1,piePrice2,piePrice3,piePrice4,ncol=2,nrow=2)

#Fraud data
piePriceFraud1<-ggplot(dataFraud[dataFraud$Make == "Toyota",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$VehiclePrice)+labs(title="Toyota")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
piePriceFraud2<-ggplot(dataFraud[dataFraud$Make == "Pontiac",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$VehiclePrice)+labs(title="Pontiac ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
piePriceFraud3<-ggplot(dataFraud[dataFraud$Make == "Honda",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$VehiclePrice)+labs(title="Honda")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
piePriceFraud4<-ggplot(dataFraud[dataFraud$Make == "Chevrolet",],aes(x=1,fill=VehiclePrice))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$VehiclePrice)+labs(title="Chevrolet")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(dataFraud), digits = 2), "%")),size=4)
grid.arrange(piePriceFraud1,piePriceFraud2,piePriceFraud3,piePriceFraud4,ncol=2,nrow=2)


#Based on the corrplot
#BasePolicy
pieBasePolicyAll<-ggplot(data,aes(x=1,fill=BasePolicy))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$BasePolicy)+labs(title="All data")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieBasePolicyFraud<-ggplot(dataFraud,aes(x=1,fill=BasePolicy))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$BasePolicy)+labs(title="Fraud data ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
grid.arrange(pieBasePolicyAll,pieBasePolicyFraud,ncol=2,nrow=2)

#Fault
pieBasePolicyAll<-ggplot(data,aes(x=1,fill=Fault))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$Fault)+labs(title="All data")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieBasePolicyFraud<-ggplot(dataFraud,aes(x=1,fill=Fault))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$Fault)+labs(title="Fraud data ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
grid.arrange(pieBasePolicyAll,pieBasePolicyFraud,ncol=2,nrow=2)

#VehicleCategory
pieBasePolicyAll<-ggplot(data,aes(x=1,fill=VehicleCategory))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=data$VehicleCategory)+labs(title="All data")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
#+geom_text(aes(x=1.2,label = paste(round(100*AgeOfVehicle/length(data), digits = 2), "%")),size=4)
pieBasePolicyFraud<-ggplot(dataFraud,aes(x=1,fill=VehicleCategory))+geom_bar(stat = "count")+coord_polar(theta = "y")+
  scale_fill_discrete(breaks=dataFraud$VehicleCategory)+labs(title="Fraud data ")+
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
grid.arrange(pieBasePolicyAll,pieBasePolicyFraud,ncol=2,nrow=2)
