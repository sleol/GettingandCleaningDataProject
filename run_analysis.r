run_analysis <- function() {
  # Source of data for the project:
  # https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
  
  #
  # 1. Merges the training and the test sets to create one data set.
  #
  setwd('/Users/George/Documents/GitHub/GettingandCleaningDataProject/UCI HAR Dataset')
  
  
  #read in training data set
  features <- read.table('./features.txt',header=FALSE); #imports features.txt
  activityType <- read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
  subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
  xTrain <- read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
  yTrain <- read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
   
  # Assigin column names to the data imported above
  colnames(activityType) <- c('activityId','activityType');
  colnames(subjectTrain) <- "subjectId";
  colnames(xTrain) <- features[,2];
  colnames(yTrain) <- "activityId"; 
  #tidied Training data
  trainingData <- cbind(subjectTrain,yTrain,xTrain)
  

  #read in test data set
  subjectTest <- read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
  xTest <- read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
  yTest <- read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt
    
  # Assigin column names to the data imported above
  colnames(activityType) <- c('activityId','activityType');
  colnames(subjectTest) <- "subjectId";
  colnames(xTest) <- features[,2];
  colnames(yTest) <- "activityId"; 
  #tidied Test data
  testData <- cbind(subjectTest,yTest,xTest)
  
  #combine the two data sets
  wholeData<- rbind(trainingData,testData)
  
  #
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  #
  
  #subset to only include the mean & std dev of each variable
  
  #indexes of means & std devs
  ind<-grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  ind3<-ind+2
  subData<-wholeData[,ind3]
  subData<-cbind(wholeData[,1:2],subData)
  
  #
  # 3. Uses descriptive activity names to name the activities in the data set
  #
  
  labeledData<-merge(activityType,subData, by="activityId")
  
  
  # 
  # 4. Appropriately labels the data set with descriptive variable names. 
  #
  
}