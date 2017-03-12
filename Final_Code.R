########## Black Friday Sales Prediction Using H20##############

# A retail company "ABC Private Limited" wants to understand the 
#customer purchase behaviour (specifically, purchase amount) 
#against various products of different categories. 
#They have shared purchase summary of various customers for 
#selected high volume products from last month


#Load the Libraries

library(data.table)
library(caret)
library(randomForest)
library(rpart)
library(gbm)
library(dplyr)
library(ggplot2)
library(dummies)
library(h2o)

# STEP 1 : Set the working directory and load the Train and Test Data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Setting the Target Variable in Test Data
test$Purchase <- 1


# Creating an index so that test and train could be segregated later
train<- cbind(train,index =0)
test <- cbind(test, index=1)

#Mearging test and train datasets
combi_data <- rbind(train,test)


#Exploratory Analysis
plot(x= train$Gender, y = train$Purchase, col="blue", xlab = "Gender", ylab = "Purchase")
ggplot(train, aes(x=Age, fill=Gender)) + geom_bar(position = "dodge")
ggplot(train, aes(x=Product_Category_1, fill=Gender)) + geom_bar(position = "dodge")
ggplot(train, aes(x=Product_Category_2, fill=Gender)) + geom_bar(position = "dodge")
ggplot(train, aes(x=Product_Category_3, fill=Gender)) + geom_bar(position = "dodge")


# Checking the Missing Values
# There are many missing values in Product Cat 2 and 3 which are actual 0 category

combi_data$Product_Category_2 <- as.character(combi_data$Product_Category_2)
combi_data$Product_Category_2[is.na(combi_data$Product_Category_2)]=0
combi_data$Product_Category_2 <- as.factor(combi_data$Product_Category_2)
combi_data$Product_Category_3 <- as.character(combi_data$Product_Category_3)
combi_data$Product_Category_3[is.na(combi_data$Product_Category_3)]=0
combi_data$Product_Category_3 <- as.factor(combi_data$Product_Category_3)

#Extracting Important Features

a<- aggregate(x= train$Purchase, by= list(train$Occupation,train$Age), FUN = median )
colnames(a) <- c("Occupation", "Age", "Purchase_Median")
a$Spending[a$Purchase_Median > 9500] <- "High"
a$Spending[a$Purchase_Median > 8000 & a$Purchase_Median < 9500] <- "Medium"
a$Spending[a$Purchase_Median < 8000] <- "Low"
a <- a[,c(1,2,4)]
a$Occupation <- as.numeric(a$Occupation)


b<- aggregate(x= train$Purchase, by= list(train$Product_ID), FUN = median )
b$Cost[b$x>= 20000] <- "VeryCostly"
b$Cost[b$x>=15000 & b$x< 20000] <- "Costly"
b$Cost[b$x>=10000 & b$x< 15000] <- "Average"
b$Cost[b$x < 10000] <- "Cheap"
b <- b[,c(1,3)]
colnames(b)[1] <- "Product_ID"
b$Product_ID <- as.character(b$Product_ID)

c<- aggregate(x= train$Purchase, by= list(train$Product_Category_1), FUN = median )
colnames(c)[1]<- "Product_Category_1"
c$Product_Cost[c$x> 15000] <- "CostlyProd"
c$Product_Cost[c$x> 8000 & c$x< 15000] <- "AverageCost"
c$Product_Cost[c$x< 8000] <- "CheapCost"
c <- c[,-2]


d <- aggregate(x=train$Purchase, by = list(train$City_Category, train$Stay_In_Current_City_Years), FUN = median)
colnames(d)[1]<- "City_Category"
colnames(d)[2]<- "Stay_In_Current_City_Years"
d$Feature[d$x> 8300] <- "A"
d$Feature[d$x<=8300] <- "B"
d <- d[,-3]
d$City_Category<- as.factor(d$City_Category)
d$Stay_In_Current_City_Years<- as.factor(d$Stay_In_Current_City_Years)

# Combining the extracted features to the main data set.

combi_data<- full_join(combi_data,a,by=c("Occupation","Age"))
combi_data<- full_join(combi_data,b,by=c("Product_ID"))
combi_data<- full_join(combi_data,c,by=c("Product_Category_1"))
combi_data<- full_join(combi_data,d,by=c("City_Category", "Stay_In_Current_City_Years"))


# Setting the right data types:

combi_data$Occupation<- as.factor(combi_data$Occupation)
combi_data$Stay_In_Current_City_Years = as.character(combi_data$Stay_In_Current_City_Years)
combi_data$Stay_In_Current_City_Years[combi_data$Stay_In_Current_City_Years=="4+"] <- "4"
combi_data$Stay_In_Current_City_Years = as.factor(combi_data$Stay_In_Current_City_Years)
combi_data$Marital_Status = as.factor(combi_data$Marital_Status )
combi_data$Product_Category_1 = as.factor(combi_data$Product_Category_1)
combi_data$Product_Category_2 = as.factor(combi_data$Product_Category_2)
combi_data$Product_Category_3 = as.factor(combi_data$Product_Category_3)
combi_data1 <- combi_data
combi_data1$Stay_In_Current_City_Years <- as.numeric(combi_data1$Stay_In_Current_City_Years)
combi_data1$Spending <- as.factor(combi_data1$Spending)
combi_data1$Cost <- as.factor(combi_data$Cost)
combi_data1$Product_Cost <- as.factor(combi_data1$Product_Cost)
combi_data1$Feature <- as.factor(combi_data1$Feature)
levels(combi_data1$Age)[levels(combi_data1$Age) == "0-17"] <- 0
levels(combi_data1$Age)[levels(combi_data1$Age) == "18-25"] <- 1
levels(combi_data1$Age)[levels(combi_data1$Age) == "26-35"] <- 2
levels(combi_data1$Age)[levels(combi_data1$Age) == "36-45"] <- 3
levels(combi_data1$Age)[levels(combi_data1$Age) == "46-50"] <- 4
levels(combi_data1$Age)[levels(combi_data1$Age) == "51-55"] <- 5
levels(combi_data1$Age)[levels(combi_data1$Age) == "55+"] <- 6
combi_data1$Age <- as.numeric(combi_data1$Age)
combi_data1$Gender <- as.numeric(combi_data1$Gender)
combi_data1$Product_Category_1 <- as.numeric(combi_data1$Product_Category_1)
combi_data1$Product_Category_2 <- as.numeric(combi_data1$Product_Category_2)
combi_data1$Product_Category_3 <- as.numeric(combi_data1$Product_Category_3) 
combi_data1$Occupation <- as.numeric(combi_data1$Occupation)
combi_data1$Marital_Status <- as.numeric(combi_data1$Marital_Status)

#Dummy Variable
combi_data2 <- dummy.data.frame(combi_data1, names = c("City_Category", "Spending", "Cost", "Product_Cost", "Feature"), sep ="_")
combi_data2 <- combi_data2[,-23]


# Perparing Data for Modeling
modelDataTrain=combi_data2[combi_data2$index==0, ]
modelDataTest=combi_data2[combi_data2$index==1, ]


#Removing Index Variable created earlier
modelDataTrain = modelDataTrain[,-15]
modelDataTest = modelDataTest[,-15]
modelDataTrain=as.data.frame(modelDataTrain)
modelDataTest=as.data.frame(modelDataTest)


# Initiating H2o and setting the H2O data frames
localH2O <- h2o.init(nthreads=-1)
train.h2o <- as.h2o(modelDataTrain)
test.h2o <- as.h2o(modelDataTest)
colnames(train.h2o)

#Setting Dependent and Independent Variable
target <- 14
predictor <- c(3:13,15:26)

#Applying Basic Regression Model
regression.model <- h2o.glm( y = target, x = predictor, training_frame = train.h2o)
h2o.performance(regression.model)
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))


#Applying Random Forest Model with 100 trees
rforest.model <- h2o.randomForest(y=target, x=predictor, training_frame = train.h2o, ntrees = 100, mtries = 3, max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model,test.h2o))


# GBM Model
gbm.model <- h2o.gbm(y=target, x=predictor, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

# Deep Learning
dlearning.model <- h2o.deeplearning(y = target,x = predictor,training_frame = train.h2o,epoch = 60,
        hidden = c(100,100),activation = "Rectifier",seed = 1122)

predict.dl1 <- as.data.frame(h2o.predict(dlearning.model,test.h2o))



# Creating Submission File
predict <- 0.5*predict.gbm +0.5*predict.dl1
submission <- modelDataTest[,c("User_ID","Product_ID")]
submission<- cbind(submission, predict)
colnames(submission)[3] <- "Purchase"
write.csv(submission,file = "Submission.csv", row.names = F)
