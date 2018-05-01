

library("readxl")
my_data <- read_excel("C:/Users/Priyanka/Documents/Vitthal_Data/Macgrill_Lost_Retained.xlsx")
library(caret)
library(kernlab)
data(my_data)
inTrain <- createDataPartition(y=my_data$lostaccount,p = 0.75, list = FALSE)
training <- my_data[inTrain,]
testing <- my_data[-inTrain,]
dim(training)

#Model:
set.seed(1234)
modelfit <- train(lostaccount~.,data = training,method = "glm")
modelfit



str(my_data)
library(dplyr)
data_fac= my_data %>% mutate_if(is.character, as.factor)
str(data_fac)
as.numeric(as.character(data_fac))
str(data_fac)