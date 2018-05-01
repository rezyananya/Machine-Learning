#Reading data

library("readxl")
my_data <- read_excel("C:/Users/Priyanka/Documents/Vitthal_Data/Macgrill_Data2.xlsx")
str(my_data)
library(dplyr)
data_fac= my_data %>% mutate_if(is.character, as.factor)
str(data_fac)
as.numeric(as.character(data_fac))
str(data_fac)

#Checking the total count of lost and retained
data_fac$active <- ifelse(data_fac$lostaccount < 2, 'lost','noinformation')
data_fac$active[data_fac$lostaccount == 2] <- 'retained'
data_fac$active <- as.factor(data_fac$active)

table(data_fac$active)

#Before we build our model, let's separate our data into testing and training sets.
library(caret)
set.seed(1234)
samp <- sample(nrow(data_fac), 0.7 * nrow(data_fac))
train <- data_fac[samp, ]
test <- data_fac[-samp, ]

#This will place 70% of the observations in the original dataset 
#into train and the remaining 30% of the observations into test.



# I am deleting columns having all missing values in it.
TrainingData<-train[,colSums(is.na(train)) == 0]
TestData <-test[,colSums(is.na(test)) == 0]
TrainingData <-TrainingData[,c(1:15)]
TestData <-TestData[,c(1:15)]



#Graph showing the count of lost and retained
hist(TrainingData$lostaccount, xlab = 'account info', main = 'Frequency', col = 'red')


#Building the model:
#Model using via Decision Tree:

library(rpart)
library(rpart.plot)
tree = rpart(lostaccount ~ ., data=TrainingData, method = "class")
rpart.plot(tree,main="Tree Structure of lostaccount", extra=103, under=TRUE, faclen=2)

TreeModelOne <- predict(tree,TestData, type = "class")
confusionMatrix(TreeModelOne,TestData$lostaccount)

#Model using via Random Forest:
TrainingData$lostaccount <- as.character(TrainingData$lostaccount)
TrainingData$lostaccount <- as.factor(TrainingData$lostaccount)

TestData$lostaccount <- as.character(TestData$lostaccount)
TestData$lostaccount <- as.factor(TestData$lostaccount)



library(randomForest)
RandomForest = randomForest(lostaccount ~ ., data=TrainingData, method = "class")
RandomForestModelTwo <- predict(RandomForest,TestData, type = "class")
confusionMatrix(RandomForestModelTwo,TestData$lostaccount)

#Plot
plot(RandomForest, main = "Error rate of random forest")

#plot shows the class error rates of the random forest model. 
#As the number of trees increases, the error rate approaches zero.

#Prediction:

PredictionModel3<- predict(RandomForest,TestData,type="class")
PredictionModel3

PredictionModel4<- predict(RandomForest,TrainingData,type="class")
PredictionModel4

write.csv(PredictionModel3, file="C:/Users/Priyanka/Documents/Vitthal_Data/Prediction3.csv")

write.csv(PredictionModel4, file="C:/Users/Priyanka/Documents/Vitthal_Data/Prediction4.csv")

