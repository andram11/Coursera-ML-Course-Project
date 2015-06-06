#Set working directory
dir <- "/home/rstudio/PLM"
setwd(dir)

# Specify data sources and data destinations
train_ds <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_ds <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training.set <- 'ds_training.csv'
test.set    <- 'ds_test.csv'

# Download the data to each corresponding data set
download.file(train_ds, training.set, method='wget')
download.file(test_ds,  test.set,  method='wget')

# Read downloaded data in data frames
test <- read.csv("/home/rstudio/PLM/ds_test.csv")
training <- read.csv("/home/rstudio/PLM/ds_training.csv")

# Clean the data and remove NA's from both training & test data sets
training <- training[, colSums(is.na(training)) == 0] 
test <- test[, colSums(is.na(test)) == 0] 

# Remove un-useful columns from both data sets
# grep1 function used for pattern matching and replacement
classe <- training$classe
trainRemove <- grepl("^X|timestamp|window", names(training))
training <- training[, !trainRemove]
trainCleaned <- training[, sapply(training, is.numeric)]
trainCleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(test))
test <- test[, !testRemove]
testCleaned <- test[, sapply(test, is.numeric)]

# Partition training data set in 70% pure training and 30% for validation purposes
library(caret)
A <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[A, ]
validdata <- trainCleaned[-A, ]

#Apply Random Forest algorithm
library(randomForest)
control <- trainControl(method="cv", 5)
random_forest <- train(classe ~ ., data=trainData, method="rf", trControl=control)
random_forest

#Check & compare results on the actual test data set
validate <- predict(random_forest, validdata)
matrix <- confusionMatrix(validdata$classe, validate)
matrix

#Accuracy of our model
matrix$overall[1]

#Out of sample error
sample_error <- 1 - as.numeric(confusionMatrix(validdata$classe, validate)$overall[1])
sample_error

#Submit files

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

submission <- predict(random_forest, testCleaned[, -length(names(testCleaned))]
pml_write_files(submission)


