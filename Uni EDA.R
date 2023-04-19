install.packages('corrplot')
install.packages('caret')
install.packages('ggplot2')
library(corrplot)
library(caret)
library(ggplot2)

data <- read.csv("F:\\HKUSemester1\\Financial Fraud Analytics (7410A)\\project\\fraud_oracle.csv")
str(data)

ggplot(data = data, aes(y = WeekOfMonth)) + geom_boxplot() + 
  labs(x = 'FraudFound_P', y = 'WeekOfMonth',) + 
  ggtitle('WeekOfMonth & FraudFound')

#hist FraudFound_P
hist(data$ FraudFound_P)

#hist WeekOfMonth
hist(x=data$WeekOfMonth,breaks = 5,col = "orange",border = "blue")

#hist Deductible
hist(x=data$Deductible)

#hsit DriverRating
hist(x=data$DriverRating)

#pie Type of the Fraud data
pie(table(data $DriverRating), col = rainbow(length(data)), main = "Type of the Fraud data")

#pie AccidentArea
pie(table(data $AccidentArea), main = "AccidentArea")

#pie DriverRating
pie(table(data $DriverRating), main = "DriverRating")

#hist WeekOfMonth
hist(x=data$WeekOfMonth,col = "orange",border = "blue")

#pie Sex
pie(table(data $Sex), main = "Sex")

#hist Deductible
hist(x=data$Deductible,col = "orange",border = "blue")

#pie VehicleCategory
pie(table(data $VehicleCategory), main = "VehicleCategory")

#pie Fault
pie(table(data $Fault), main = "Fault")

#hist Age
hist(x=data$Age,col = "orange",border = "blue")

#pie PastNumberOfClaims
pie(table(data $PastNumberOfClaims), main = "PastNumberOfClaims")

#pie PoliceReportFiled
pie(table(data $PoliceReportFiled), main = "PoliceReportFiled")

#pie WitnessPresent
pie(table(data $WitnessPresent), main = "WitnessPresent")

#pie NumberOfSuppliments
pie(table(data $NumberOfSuppliments), main = "NumberOfSuppliments")

#pie NumberOfCars
pie(table(data $VehiclePrice), main = "NumberOfCars")
