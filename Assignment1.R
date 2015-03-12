# Load all necessary Libraries
library(caret)
library(rattle)
library(rpart)
library(randomForest)

# Set seed for research reproducibility
set.seed(12323)

# Getting the train and test files URL:
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

directory <- getwd()
download.file(trainUrl, paste(directory, "/pml-training.csv", sep=""), method="curl")
download.file(testUrl, paste(directory, "/pml-testing.csv", sep=""), method="curl")

training <- read.csv("pml-training.csv", header=TRUE, sep=",", na.strings=c("", "NA", "NULL"))
testing <- read.csv("pml-testing.csv", header=TRUE, sep=",", na.strings=c("", "NA", "NULL"))

# Explore data
dim(training)
dim(testing)
totalNAvaluesByColumnsInTrain <- colSums(!is.na(training))
totalNAvaluesByColumnsInTest <- colSums(!is.na(testing))

# Data Cleaning
# If it has more than 40% of NA values, we will remove it.
goodTrainData <- totalNAvaluesByColumnsInTrain>0.6*(nrow(training))
goodTrainDataName <- names(goodTrainData[goodTrainData==TRUE])
goodTestData <- totalNAvaluesByColumnsInTest>0.6*(nrow(testing))
goodTestDataName <- names(goodTestData[goodTestData==TRUE])

# Keep only the good columns in both train and test sets.
training <- training[goodTrainData]
testing <- testing[goodTrainData]

# Partition data
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain,]
myTesting <- training[-inTrain,]

# To make all the testing and validation consistent with myTraining dataset
col1 <- colnames(myTraining)
# Remove classe column as it does not exist in our validation dataset
col2 <- colnames(myTraining[, -60]) 
myTesting <- myTesting[col1]
testing <- testing[col2]

# Take 1st row of myTraining and combine with testing. This coerce the data into the same type as myTraining dataset
myTesting <- rbind(myTraining[1,] , myTesting) 
# Now removing the first row of the data. Nothing in testing dataset has changed.
myTesting <- myTesting[-1,]

testing <- rbind(myTraining[1, -60] , testing) 
# Now removing the first row of the data. Nothing in testing dataset has changed.
testing <- testing[-1,]

# Finally, removing all first column variable `x` as it cannot be used as a predictor.
myTraining <- subset(myTraining, select= -X)
myTesting <- subset(myTesting, select= -X)
testing <- subset(testing, select= -X)

modFitRPart <- train(classe ~ ., data=myTraining,  method="rpart")
# Prediction on myTesting dataset
predictionRPart <- predict(modFitRPart, myTesting)
# ConfusionMatrix
CMRPart <- confusionMatrix(predictionRPart, myTesting$classe)
# Accuracy
CMRPart$overall[1]
CMRPart[3]

# Using ML algorithms for prediction: Random Forests
modFitRF <- randomForest(classe ~. , data=myTraining)
# Prediction on myTesting dataset
predictionsRF <- predict(modFitRF, training, type = "class")
# ConfusionMatrix
CMRF <- confusionMatrix(predictionsRF, training$classe)
# Accuracy
CMRF$overall[1]
CMRF[3]

# Function to generate files with predictions to submit for assignment:
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

predictionsRF2 <- predict(modFitRF, testing, type = "class")
pml_write_files(predictionsRF2)
answers <- as.character(predictionsRF2)
answers


### RPart Figure Plot
fancyRpartPlot(modFitRPart$finalModel)

### Random Forest Table
#result hidden as it is too long
#randomForestTable <- getTree(modFitRF, 1, labelVar=TRUE) 


