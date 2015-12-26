#Program Name: run_analysis.R
#Name: mfmcshea
#Date of First Draft: December 21, 2015, 9:57PM
#Revisions on Final Draft: December 26, 4:14PM
#Purpose: To complete Getting Data Course Project

#Set working directory
setwd ("C:/Users/McShea/Desktop/DataScience/GettingData/")

#The following downloads the data onto my computer
if(!file.exists("./projectdata")) {
  dir.create("./projectdata")
}
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl, destfile ="./projectdata/allgcc.zip")
dateDownloaded <- date()

#Unzip the file into the /projectdata/directory
unzip(zipfile="./projectdata/allgcc.zip", exdir="./projectdata")

#Load libraries
library(dplyr)
library(data.table)
#Download tidyr package
#install.packages("tidyr")
library(tidyr)

AllFilesPath <-"C:/Users/McShea/Desktop/DataScience/GettingData/projectdata/UCI HAR Dataset"

#Read Subject Files
dataSubjectTrain <-tbl_df(read.table(file.path(AllFilesPath, "train", "subject_train.txt")))
dataSubjectTest  <-tbl_df(read.table(file.path(AllFilesPath, "test", "subject_test.txt")))

#Read Activity Files
dataActivityTrain<-tbl_df(read.table(file.path(AllFilesPath, "train", "Y_train.txt")))
dataActivityTest <-tbl_df(read.table(file.path(AllFilesPath, "test", "Y_test.txt")))

#Read Data Files
dataTrain        <-tbl_df(read.table(file.path(AllFilesPath, "train", "X_train.txt")))
dataTest         <-tbl_df(read.table(file.path(AllFilesPath, "test", "X_test.txt")))

#Checking out dimentions of data files
#head(dataSubjectTrain) # 7352 obs of 1 variable
#head(dataSubjectTest)  # 2947 obs of 1 variable
#head(dataActivityTrain) #7352 obs of 1 variable
#head(dataActivityTest) #2947 obs of 1 variable
#head(dataTrain) #7352 obs of 561 variables
#head(dataTest) #2947 obs of 561 variables

#ITEM 1
#Merge training and test sets to create one data set
# Do this for both activity and subject files using rbind
# Rename the variables subject and activitynum
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject,"V1", "subject")

alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#Combine the data training and test files
dataTable <- rbind(dataTrain, dataTest)

#Name variables according to feature
#V1 = tBodyAccc-mean()=X
dataFeatures <-tbl_df(read.table(file.path(AllFilesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(AllFilesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum", "activityName"))

#Merge columns
alldataSubjAct <- cbind(alldataSubject, alldataActivity) #10299 obs. of 2 variables
dataTable <- cbind(alldataSubjAct, dataTable) #10299 obs. of 563 variables

#head(dataTable, n=1)

#ITEM 2
#Extracts only the measurements on the mean and standard deviation for each measurement

#head(dataFeatures) #Var featurenum and featureName

#Reading features.txt and extracting only the mean and std dev.

dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)", dataFeatures$featureName, value=TRUE)

#Taking only measurements for mean and std dev. Add subject and activityNum

dataFeaturesMeanStd <- union(c("subject", "activityNum"), dataFeaturesMeanStd)

dataTable <- subset(dataTable, select=dataFeaturesMeanStd)

#head(dataTable, n=3)

#ITEM 3
# Uses descriptive activity names to name the activities in # data set
# Include name of activity with dataTable
dataTable <- merge(activityLabels, dataTable, by="activityNum", all.x =TRUE)
dataTable$activityName <-as.character(dataTable$activityName)

# Create dataTable with variable means sorted by subject and activity
dataAggr<- aggregate(.~ subject - activityName, data = dataTable, mean) #180 obs of 69 vars
dataTable <- tbl_df(arrange(dataAggr, subject, activityName)) #180 obs of 69 variables

#head(dataTable, n=3)

#ITEM 4: Appropriately labels the data set w/ descriptive var names.

#This is the data before descriptive names.
head(str(dataTable, 2))

#Prepare descriptive var names.
names(dataTable) <-gsub("std()", "SD", names(dataTable))
names(dataTable) <-gsub("mean()", "MEAN", names(dataTable))
names(dataTable) <-gsub("^t", "time", names(dataTable))
names(dataTable) <-gsub("^f", "frequency", names(dataTable))
names(dataTable) <-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable) <-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable) <-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable) <-gsub("BodyBody", "Body", names(dataTable))

#This is with the new names.
head(str(dataTable), 6)

#ITEM 5
#From step 4 data, create a second, independent tidy data set w/ aveg of each variable for each activity and each subject

#Send text file to disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)

#What are the characteristics of TidyData.txt?
whatisintxt<-read.table("TidyData.txt", header=TRUE)
head(str(whatisintxt), 2)

#Text file has 180 obs of 69 variables.

