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
  
  # Updating the colNames vector to include the new column names after merge
  colNames  = colnames(labeledData); 
  
  # Cleaning up the variable names
  for (i in 1:length(colNames))
  {
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  };
  
  # Reassigning the new descriptive column names to the finalData set
  colnames(labeledData) = colNames;

  # write data to csv
  write.table(labeledData,"mergedmeans&std.txt")
  
  
  #
  # 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  #

  # remove activity type column from data (to make getting the means possible across the whole data set)
  labeledDnoAT  <- labeledData[,names(labeledData) != 'activityType'];
  
  # aggregate the data by activityID, SubjectID taking means
  tidyData <- aggregate(labeledDnoAT[,names(labeledDnoAT) != c('activityId','subjectId')],
                       by=list(activityId=labeledDnoAT$activityId,subjectId = labeledDnoAT$subjectId),mean);
  # write data set to csv
  write.table(tidyData,"tidyData.txt")
}