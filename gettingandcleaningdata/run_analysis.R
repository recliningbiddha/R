# run_analysis.R function for Getting and Cleaning Data Peer Review Asignment
# November 2018

run_analysis <- function() {  #start function body

#set directories
  workingdir <- "~/Downloads/"
  setwd(workingdir)  
  
# if the UCI HAR Dataset folder does not exist in the working directory,
# pull the zip file from the Coursera-provided link and unzip it, cleaning up after
folder <- "UCI HAR Dataset"
if(!file.exists(folder)) {
  zipFile <- "getdata_projectfiles_UCI_HAR_Dataset.zip"
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile=zipFile, method="curl")
  unzip(zipFile)
  } # close if statement

# load required libraries
library(dplyr)
library(reshape2)

#
# 1. Merges the training and the test sets to create one data set.
#  

# change to dirctory created
  setwd(paste(workingdir, "/UCI HAR Dataset", sep=""))
  testdir <- "test"
  traindir <- "train"

# read activity labels file
  activitylabels <- read.table("activity_labels.txt")
  features <- read.table("features.txt", stringsAsFactors=FALSE)[,2]
    
# read data from test directory
  setwd(paste("./",testdir,sep=""))
  testsubjects <- read.table("subject_test.txt", col.names="subject")
  testset <- read.table("X_test.txt", col.names = features, check.names = FALSE)
  testlabels <- read.table("y_test.txt", col.names="activity")
  
# read data from train directory
  setwd(paste("../",traindir,sep=""))
  trainsubjects <- read.table("subject_train.txt", col.names="subject")
  trainset <- read.table("X_train.txt", col.names = features, check.names = FALSE)
  trainlabels <- read.table("y_train.txt", col.names = "activity")
  
setwd("..")

# Merge data into one dataset
test <- cbind (testset, testlabels, testsubjects)
train <- cbind(trainset, trainlabels, trainsubjects)
dataset <- rbind(test, train)

#
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#
# Identify the column names containing mean or std
keepcols <- grep("(mean\\(\\)|std\\(\\)|activity|subject)", names(dataset))
dataset <- dataset[,keepcols]

#
# 3. Uses descriptive activity names to name the activities in the data set
#
activitylabels <- read.table("activity_labels.txt", col.names = c("activity", "activityname"))
dataset <- merge(dataset, activitylabels)
dataset <- dataset[,(names(dataset) != "activity")]
colnames(dataset)[colnames(dataset) == "activityname"] <- "activity"

#
# 4. Appropriately labels the data set with descriptive variable names. 
#
# already done while reading in dataset

#
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 
dataset <- melt(dataset, id=c("subject","activity"))
dataset <- dcast(dataset, subject+activity~variable, mean)
write.table(dataset, file = "tidy_data.txt", row.names = FALSE)

}  # end function body

