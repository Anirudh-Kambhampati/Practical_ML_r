# Load the required Libraries for the project -----------------------------

library(caTools)
library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(RColorBrewer)
library(RGtk2)
library(gbm)

#Loading training and test data that is downloaded from the links provided

train_link <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_link <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
train <- read.csv(url(train_link))
test <- read.csv(url(test_link))
dim(train)
dim(test)


# Data Cleaning -----------------------------------------------------------
# Removing Variables with negligible Variance

var <- nearZeroVar(train)
train_data <- train[,-var]
test_data <- test[,-var]
dim(train_data)
dim(test_data)


# Removing NA values ------------------------------------------------------

na_val_col <- sapply(train_data, function(x) mean(is.na(x))) > 0.95
train_data <- train_data[,na_val_col == FALSE]
test_data <- test_data[,na_val_col == FALSE]
dim(train_data)
dim(test_data)

# Removing the first 7 lines ----------------------------------------------

train_data<- train_data[, 8:59]
test_data<- test_data[, 8:59]
dim(train_data) 
dim(test_data)

# Split the data into train and test sets ---------------------------------
# Now we split our data into 60-40 percent of training and test datasets

inTrain<- createDataPartition(train_data$classe, p=0.6, list=FALSE)
inTrain<- createDataPartition(train_data$classe, p=0.6, list=FALSE)
training<- train_data[inTrain,]
testing<- train_data[-inTrain,]
dim(training)
dim(testing)


# Cross Validation Model --------------------------------------------------
# Constructing a Decision tree and visualizing

library(rattle)
DT_model<- train(classe ~. , data=training, method= "rpart")
fancyRpartPlot(DT_model$finalModel)
 
# predicting the classifier and finding the errors using confusionMatrix

set.seed(143)
DT_prediction<- predict(DT_model, testing)
confusionMatrix(DT_prediction, testing$classe)

# Here we observe that the  prediciton accuracy of the model is bad ~ 53%

# Random Forest model(Bagging at every branch) -------------------------------------------

set.seed(1)

# Fit the model
RF_model<- train(classe ~. , data=training, method= "rf", ntree=100)

# Prediction  
RF_prediction <- predict(RF_model, testing)
RF_cm<-confusionMatrix(RF_prediction, testing$classe)
RF_cm

# Plot
plot(RF_cm$table, col=RF_cm$byClass, main="Random Forest Accuracy")


# We see that the prediction accuracy has been increased to ~ 99%  --------
# Now we implemnt Gradient Boosting for comparision

# Boosting and Model Prediciton -------------------------------------------

set.seed(1000)
gbm_model<- train(classe~., data=training, method="gbm", verbose= FALSE)
gbm_model$finalmodel

# Prediciton

gbm_prediction<- predict(gbm_model, testing)
gbm_cm<-confusionMatrix(gbm_prediction, testing$classe)
gbm_cm


# The Boositng model prediction accuracy is now ~ 94% -----------------------
# Now we compare this accuracy with overall accuracy

RF_cm$overall
gbm_cm$overall


# OUTLINE -----------------------------------------------------------------
# We can now see that Random Forest gives more accurate prediciton than Boosting

prediction_test<- predict(RF_model, test_data)
prediction_test
