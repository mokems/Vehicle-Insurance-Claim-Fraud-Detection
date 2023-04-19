install.packages("ROSE")
install.packages("smotefamily")

library(ROSE)
library(smotefamily)
library(dplyr)

# read dataset
data = read.csv("fraud_oracle_1.csv")

# divide the data into training set and test set
#set.seed(1234)
#index = sample(nrow(df), 0.7*nrow(data)) 
#train = data[index,] 
#test = data[-index,] 

# normalization
data_scaled <- scale(data[,-30])
class(data_scaled)

#####
#data_scaled <- data[,-30]

data.df <- as.data.frame(data_scaled)
str(data.df)
data.df[30] <- data[,30]
names(data.df)[30]<- "class"
str(data.df)

#write.csv(newTR.df,file="data_normalization.csv",quote=F,row.names = F)

# divide dataset
SCL1 <- filter(data.df, data.df$class==0)
SCL2 <- filter(data.df, data.df$class==1)
dim(SCL1)
dim(SCL2)
#CL1
set.seed(1)
indexesCL1 <- sample(1:nrow(SCL1), size = 0.2*nrow(SCL1))
testCL1<- SCL1[indexesCL1,]
trainCL1<- SCL1[-indexesCL1,]
testCL1.Y<- SCL1$class [indexesCL1]
trainCL1.Y<- SCL1$class [-indexesCL1]
#CL2
set.seed(1)
indexesCL2 <- sample(1:nrow(SCL2), size = 0.2*nrow(SCL2))
testCL2<- SCL2[indexesCL2,]
trainCL2<- SCL2[-indexesCL2,]
testCL2.Y<- SCL2$class [indexesCL2]
trainCL2.Y<- SCL2$class [-indexesCL2]

TESTSET<- rbind(testCL1,testCL2)
TRAINSET12 <- rbind(trainCL1,trainCL2)
dim(TESTSET)
dim(TRAINSET12)
sum(is.na(TRAINSET12))

write.csv(TESTSET,file="test_ori.csv",quote=F,row.names = F)
# Class imbalance processing with 'smotefamily'
library(smotefamily)
set.seed(12345)

for (i in 1:nrow(TRAINSET12)){
  TRAINSET12$CL2[i] <- ifelse(TRAINSET12$class[i] == 1,1,0)
}

table(TRAINSET12$CL2)
TRAINSET12.2 <- TRAINSET12[,-30]
smote_result22 = SMOTE(TRAINSET12.2[,-30],target = TRAINSET12.2$CL2, K = 5, dup_size = 14)
oversampled22 = smote_result22$data
str(oversampled22)

BS2<- filter(oversampled22, oversampled22$class == 1)
nrow(BS2)
length(BS2)
str(BS2)

# build new training dataset
newTR.df <- rbind(trainCL1,BS2)
newTR <- newTR.df[,-30]
newTR.LABEL <- newTR.df$class


# Use the same way to build new test set
# library(smotefamily)
# set.seed(12345)
# 
# for (i in 1:nrow(TESTSET)){
#   TESTSET$CL2[i] <- ifelse(TESTSET$class[i] == 1,1,0)
# }
# 
# TESTSET.2 <- TESTSET[,-30]
# smote_result.T2 = SMOTE(TESTSET.2[,-30],target = TESTSET.2$CL2, K = 5, dup_size = 14)
# oversampledT2 = smote_result.T2$data
# str(oversampledT2)
# 
# library(dplyr)
# TS2<- filter(oversampledT2, oversampledT2$class == 1)
# nrow(TS2)
# length(TS2)
# str(TS2)
# 
# newTE.df <- rbind(testCL1,TS2)
# newTE <- newTE.df[,-30]
# newTE.LABEL <- newTE.df$class

# Compare
table(TRAINSET12$class)
table(newTR.df$class)
# table(TESTSET$class)
# table(newTE.df$class)

# save file
write.csv(newTR.df,file="train_ori.csv",quote=F,row.names = F)
#write.csv(newTE.df,file="test_SMOTE_k_5_d_14.csv",quote=F,row.names = F)

