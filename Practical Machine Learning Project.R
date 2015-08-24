###########################################################
#title: 'Practical Machine Learning: Practical Machine Learning Course Project'
#author: "Matthew Fergusson"
#date: "Aug 23, 2015"
#output: Human Activity Recognition analysis 
###########################################################

#Remove all objects in [r] environment
#rm(list = ls(all.names = TRUE))

setwd("C:/Users/mfergusson/Desktop/MTF Personal/Education - Coursera Data Science Specialization/08 - Practical Machine Learning/Course_Projects/Data Sets")

#install.packages("caret")
#install.packages("randomForest")
#install.packages('e1071', dependencies=TRUE)

library(caret)
library(randomForest)
library(tidyr)
library(dplyr)
library(sqldf)

Raw_HAR_Data_Train <- read.csv("pml-training.csv", header = TRUE)
Raw_HAR_Data_Test <- read.csv("pml-testing.csv", header = TRUE)

HAR_Data_Train <- Raw_HAR_Data_Train
HAR_Data_Test <- Raw_HAR_Data_Test

# Data Manipulation
  # Remove columnss with NA > 75%
    na_col <- names(HAR_Data_Train[, colSums(is.na(HAR_Data_Train)) > nrow(HAR_Data_Train)*.75])
    HAR_Data_Train[,na_col] <- list(NULL)
    #93 Variables
  # Remove nearZeroVar fields
    ZVar <- nearZeroVar(HAR_Data_Train)
    HAR_Data_Train[,ZVar] <- list(NULL)
    #59 Variables
  # Discart ID columns
    HAR_Data_Train[,1:6]<- list(NULL)
    #53 Variables

set.seed(1986)

#set train and test
  Partitions <- createDataPartition(y= Raw_HAR_Data_Train$classe, p = 0.60, list = FALSE)
  model_train = HAR_Data_Train[Partitions,]
  model_test = HAR_Data_Train[-Partitions,]

set.seed(1989)
  #fit <- train(classe ~., data = model_train, method = "rf", prox = TRUE)
  fit <- randomForest(classe ~., data=model_train, ntree=1000,keep.forest=TRUE)
  
  prediction_train <- predict(fit, model_test)

  confusionMatrix(model_test$classe, prediction_train)

  prediction_test <- predict(fit, HAR_Data_Test)
  
  
  setwd("C:/Users/mfergusson/Desktop/MTF Personal/Education - Coursera Data Science Specialization/08 - Practical Machine Learning/Course_Projects/Answers")
  
  
  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }

  pml_write_files(prediction_test)
  