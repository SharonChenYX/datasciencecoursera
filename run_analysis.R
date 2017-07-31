#this R code does:
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names---namaed tidy1.
#From tidy1, creates a second, independent tidy data set (tidy2) with the average of each variable for each activity and each subject.

#data source:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#disclosure about the data
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

###########################################################################################################################
#1
#get working directory/where the data files are
dir1 <- paste(getwd(), "/UCI HAR Dataset", sep="")
dir2 <- paste(getwd(), "/UCI HAR Dataset/train", sep="")
dir3 <- paste(getwd(), "/UCI HAR Dataset/test", sep="")


#2
#the following codes get different things from different files
#get activity labels
activitydata <- read.table(
  paste(dir1,"/activity_labels.txt",sep=""),
  header=FALSE,
  col.names = c("activityindex","activity"))
activitydata$activity <- tolower(activitydata$activity)

#get feature names
featuredata <- read.table(
  paste(dir1,"/features.txt",sep=""),
  header=FALSE,
  col.names = c("featureindex","feature"))
allfeatures <- gsub("-","",featuredata$feature)
where_feature <- grep(".*(mean\\(\\)|std\\(\\)).*", allfeatures)
feature <- allfeatures[where_feature]

#get training related data
#subject
trainsub <- read.table(
  paste(dir2,"/subject_train.txt",sep=""),
  header=FALSE,
  col.names = c("subject"))
#activity
trainlabel <- read.table(
  paste(dir2,"/y_train.txt",sep=""),
  header=FALSE,
  col.names = c("activityindex")
  )
library(dplyr)
trainactivity <- select(
  merge(x=trainlabel,y=activitydata,by.x = "activityindex",by.y="activityindex"),
  -activityindex)
#data from 561 features
trainset <- read.table(
  paste(dir2,"/X_train.txt",sep=""),
  header=FALSE,
  col.names = allfeatures
  )
#select required features
trainset <- select(trainset,where_feature)

#get test related data
#subject
testsub <- read.table(
  paste(dir3,"/subject_test.txt",sep=""),
  header=FALSE,
  col.names = c("subject"))
#activity
testlabel <- read.table(
  paste(dir3,"/y_test.txt",sep=""),
  header=FALSE,
  col.names = c("activityindex")
)
testactivity <- select(
  merge(x=testlabel,y=activitydata,by.x = "activityindex",by.y="activityindex"),
  -activityindex)
#data from 561 features
testset <- read.table(
  paste(dir3,"/X_test.txt",sep=""),
  header=FALSE,
  col.names = allfeatures
)
#select required features
testset <- select(testset,where_feature)

#3
#merging data
#merge above into train data and test data
traindata <- cbind(trainsub,trainactivity,trainset)
testdata <- cbind(testsub,testactivity,testset)
#then merge these two into all data
alldata <- rbind(traindata,testdata)
#tidy it up by subject and activity
tidy1 <- aggregate(alldata[,3:ncol(alldata)],by=list("subject"=alldata$subject,"activity"=alldata$activity),FUN=mean)
#sort by subject to make it pretty
tidy1 <- tidy1[order(tidy1$subject),]
print(tidy1)

############################################################################################################################

#4
#create tidy2 data set with the average of each variable for each activity and each subject.
#feature name is unique, but subject and activity are not
sub <- unique(tidy1$subject)
Nsub <- length(sub)
Nvar <- ncol(tidy1)-2

tidy2_subject <- c()
tidy2_activity <- c()
tidy2_mean <- data.frame()

for (ss in 1:Nsub){
  sshere <- sub[ss]
  #for this subject, he/she has these activity(s)
  act <- as.character(unique(
    tidy1[which(tidy1$subject==sshere),]$activity))
  Nact <- length(act)
  
  for (aa in 1:Nact){
    aahere <-act[aa]
    #extract data into a matrix
    ddhere <- 
      tidy1[which(tidy1$subject==sshere & tidy1$activity==aahere),][,3:ncol(tidy1)]
      
    #for this activity, calculate the mean of each variable
    meanhere <- lapply(
      ddhere,mean)
    
    #include into tidy2
    tidy2_subject <- c(tidy2_subject,sshere)
    tidy2_activity <- c(tidy2_activity,aahere)
    tidy2_mean <- rbind(tidy2_mean,meanhere)
  }
}
tidy2 <- data.frame(subject=tidy2_subject,activity=tidy2_activity,feature=tidy2_mean)
print(tidy2)
#############################################################################################################################

write.table(tidy2, paste(dir1, "/tidy2.txt", sep=""), col.names=FALSE, row.names=FALSE)
