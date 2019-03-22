# Bank Marketing Data Analysis R Project

# Loading data
bank_marketing_data <- read.table("bank-full.csv", header=TRUE,sep=";") 
# Structure of the data set 
str(bank_marketing_data)
# If newdata has same number of observation that implies no NA value present 
newdata <- na.omit(bank_marketing_data) 
nrow(newdata)==nrow(bank_marketing_data)

# Outlier detection and treatment
# Range of individual variables 
summary(bank_marketing_data)
boxplot(bank_marketing_data$age, main="Age Box plot",yaxt="n", xlab="Age", horizontal=TRUE, col=terrain.colors(2))
# By plotting histogram we can ensure if there are outliers or not 
hist(bank_marketing_data$age,col=terrain.colors(10))

# Correlation
library(psych) 
pairs.panels(bank_marketing_data[, c(1:8,17)]) 
pairs.panels(bank_marketing_data[, c(9:17)])


# Subset Selection
bank_marketing_data_sub <-bank_marketing_data[, c(1:4,7:9,12,14,15,17)] 
str(bank_marketing_data_sub) 
pairs.panels(bank_marketing_data_sub)



# Binning and Data Transformation
bank_marketing_data_sub$age <- cut(bank_marketing_data_sub$age, c(1,20,40,60,100)) 
bank_marketing_data_sub$is_divorced <- ifelse( bank_marketing_data_sub$marital == "divorced", 1, 0) 
bank_marketing_data_sub$is_single <- ifelse( bank_marketing_data_sub$marital == "single", 1, 0) 
bank_marketing_data_sub$is_married <- ifelse( bank_marketing_data_sub$marital == "married", 1, 0) 
bank_marketing_data_sub$marital <- NULL 
str(bank_marketing_data_sub)


# Finding overlap between predictor and outcome/target variable 
boxplot(duration~y,data=bank_marketing_data_sub, main="Finding Overlap between predictor and outcome", yaxt="n", xlab="Duration", horizontal=TRUE, col=terrain.colors(3))


# Boundry between two predictors
library(ggplot2) 
qplot(bank_marketing_data_sub$pdays,bank_marketing_data_sub$duration,data=bank_marketing_data_sub,colour=y,size=3)



# Training and testing split
#CreateDataPartition present in caret packagesplit in such a way that 
#training and testing data will have same ratio for target variable
library(caret) #Rows selection for training data set 
inTrain <- createDataPartition(y=bank_marketing_data_sub$y ,p=0.7,list=FALSE) 
training <- bank_marketing_data_sub[inTrain,] 
testing <- bank_marketing_data_sub[-inTrain,] 
# As we said we have imbalanced data so how can we do sampling 
#Caret will take care of that, So createDataPartition does the magic 
dim(training);dim(testing) #,p=0.7,list=FALSE)
# We can see imbalancing has been taken care of or not 
table(training$y); table(testing$y)




# Decision Tree
library(rpart) 
library(rpart.plot) 
library(rattle) 
library(caret) 
dt_model<- rpart(y ~ ., data = training) 
summary(dt_model)


# Testing Decision Tree
predictions <- predict(dt_model, testing, type = "class") 
#What is predicted 
table(predictions) 
# Lets look at the confusion matrix 
confusion.matrix <- prop.table(table(predictions, testing$y)) 
confusion.matrix 
confusionMatrix(predictions,testing$y)
fancyRpartPlot(dt_model)
