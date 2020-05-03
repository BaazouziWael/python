################################################################################
#
# Getting Clean Data Coursera Project
#
# Project Requirements:
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each
#    measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
#
################################################################################

# Load necessary libraries
library(dplyr)

# Check for data and download/unzip if necessary
zipfilename <- "ProjectData.zip"
unzipdirectory <- "UCI HAR Dataset"

if(!file.exists(zipfilename)){
    datalocation <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"    
    download.file(url = datalocation, destfile = zipfilename, method = "curl")
}

if(!file.exists(unzipdirectory)){
    unzip(zipfilename)
}

################################################################################
### STEP 1 Merges the training and the test sets to create one data set.###
# Import test data
test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/Y_test.txt")
test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Import training data
train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_labels <- read.table("./UCI HAR Dataset/train/Y_train.txt")
train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Join test and training data
combine_data <- rbind(test_data, train_data) #This fulfills Step 1
combine_labels <- rbind(test_labels, train_labels)
combine_subjects <- rbind(test_subjects, train_subjects)

################################################################################
### STEP 2 Extracts only the measurements on the mean and standard deviation 
###        for each measurement.
features <- read.table("./UCI HAR Dataset/features.txt")
meanstdcols <- grep("(.*)mean[^F]|std(.*)",features[,2])
dataMeanStd <- combine_data[,meanstdcols] #This fulfills Step 2

################################################################################
### STEP 3 Uses descriptive activity names to name the activities in the data set
activitynames <- read.table("./UCI HAR Dataset/activity_labels.txt")
activies <- right_join(activitynames, combine_labels, by = "V1") #Partially satisfies Step 4
activies <- select(activies, activity = V2) #Fulfills Step 3

################################################################################
### STEP 4 Appropriately labels the data set with descriptive variable names.
dataLabels <- as.vector(features[meanstdcols, 2])
dataLabels <- sub("mean\\(\\)","Mean",dataLabels)
dataLabels <- sub("std\\(\\)","SD",dataLabels)
names(dataMeanStd) <- dataLabels
combine_subjects$V1 <- as.factor(combine_subjects$V1)
names(combine_subjects) <- "subject"
combineall <- bind_cols(combine_subjects, activies, dataMeanStd)

################################################################################
### STEP 5 From the data set in step 4, creates a second, independent tidy data 
###        set with the average of each variable for each activity and each 
###        subject.
meansummary <- combineall %>% arrange(subject, activity) %>% group_by(subject, activity) %>% summarize_each(funs(mean))
summaryLabels <- c("Subject", "Activity", paste("Mean", dataLabels, sep = "_"))
names(meansummary) <- summaryLabels

# Write final tidy summary to a text file for submission
write.table(meansummary, file = "finalTidySummary.txt")



#####
## Downloading the data and preparing the data
## Use the directory ".data' as working directory
currdir <- "./data"
if(!dir.exists("./data")) dir.create("./data")
setwd(currdir)

downloadurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI HAR Dataset.zip"
download.file(downloadurl, zipfile)

if(file.exists(zipfile)) unzip(zipfile)

####
## Files are downloaded and the following files exist
##
basedir <- "UCI HAR Dataset"
featuresfile <- paste(basedir, "features.txt", sep="/")
activitylabelsfile <- paste(basedir, "activity_labels.txt", sep="/")
testvariablesfile <- paste(basedir, "test/X_test.txt", sep="/")
testactivityfile <- paste(basedir, "test/y_test.txt", sep="/")
testsubjectfile <- paste(basedir, "test/subject_test.txt", sep="/")
trainvariablesfile <- paste(basedir, "train/X_train.txt", sep="/")
trainactivityfile <- paste(basedir, "train/y_train.txt", sep="/")
trainsubjectfile <- paste(basedir, "train/subject_train.txt", sep="/")

neededfiles <- c(featuresfile,
                 activitylabelsfile,
                 testvariablesfile,
                 testactivityfile,
                 testsubjectfile,
                 trainvariablesfile,
                 trainactivityfile,
                 trainsubjectfile
                 )
sapply(neededfiles, function(f) if(!file.exists(f)) stop(paste("Needed file ", f, " doesn't exist. Exitting ...", sep="")))
## $`UCI HAR Dataset/features.txt`
## NULL
## 
## $`UCI HAR Dataset/activity_labels.txt`
## NULL
## 
## $`UCI HAR Dataset/test/X_test.txt`
## NULL
## 
## $`UCI HAR Dataset/test/y_test.txt`
## NULL
## 
## $`UCI HAR Dataset/test/subject_test.txt`
## NULL
## 
## $`UCI HAR Dataset/train/X_train.txt`
## NULL
## 
## $`UCI HAR Dataset/train/y_train.txt`
## NULL
## 
## $`UCI HAR Dataset/train/subject_train.txt`
## NULL
####
## Read featuresfile
features <- read.table(featuresfile, col.names=c("rownumber","variablename"))
####

####
## Fix the issue with duplicate names (e.g.) 516. fBodyBodyAccJerkMag-mean()
####
allvariables <- 
  mutate(features, variablename = gsub("BodyBody", "Body", variablename))

####
## Filter the 66 variables - mean() and std()
####
neededvariables <- filter(allvariables, grepl("mean\\(\\)|std\\(\\)", variablename))

####
## Make the allvariables readable
##    Remove special characters, Convert to lower case
####
allvariables <- mutate(allvariables, variablename = gsub("-", "", variablename),
                                     variablename = gsub("\\(", "", variablename),
                                     variablename = gsub("\\)", "", variablename),
                                     variablename = tolower(variablename))

####
## Make the neededvariables readable
##    Remove special characters, Convert to lower case
####
neededvariables <- mutate(neededvariables, variablename = gsub("-", "", variablename),
                                           variablename = gsub("\\(", "", variablename),
                                           variablename = gsub("\\)", "", variablename),
                                           variablename = tolower(variablename))

####
## Read activitylabelsfile
activitylabels <- read.table(activitylabelsfile, col.names=c("activity", "activitydescription"))
####

####
## Read in test data stats
####
testvalues <- read.table(testvariablesfile, col.names = allvariables$variablename)
testneededvalues <- testvalues[ , neededvariables$variablename]
####

## Read in test activities
testactivities <- read.table(testactivityfile, col.names=c("activity"))
####

####
## Read in test subjects
testsubjects <- read.table(testsubjectfile, col.names=c("subject"))
####

####
## Add a readable activity description
testactivitieswithdescr <- merge(testactivities, activitylabels)
####

####
## Put the test data together
##    Assuming that the data is in the same order and all we need is cbind
##    Combining values, activities, subjects
testdata <- cbind(testactivitieswithdescr, testsubjects, testneededvalues)
####

####
## Read in train variables
####
trainvalues <- read.table(trainvariablesfile, col.names = allvariables$variablename)
trainneededvalues <- trainvalues[ , neededvariables$variablename]
####

## Read in train activities
trainactivities <- read.table(trainactivityfile, col.names=c("activity"))
####

####
## Read in train subjects
trainsubjects <- read.table(trainsubjectfile, col.names=c("subject"))
####

####
## Add a readable activity description
trainactivitieswithdescr <- merge(trainactivities, activitylabels)
####

####
## Put the train data together
##    Assuming that the data is in the same order and all we need is cbind
##    Combining values, activities, subjects
traindata <- cbind(trainactivitieswithdescr, trainsubjects, trainneededvalues)
####

####
## Combine the testdata and traindata
## Additionally make subject a factor
alldata <- rbind(testdata, traindata) %>% select( -activity )
alldata <- mutate(alldata, subject = as.factor(alldata$subject))
####

####
## Write the data out
write.table(alldata, "Mean_And_StdDev_For_Activity_Subject.txt")
####

####
## Create a second, independent tidy data set with the average of each 
##        variable for each activity and each subject.
## Group the data by activity, subject
allgroupeddata <- group_by(alldata,activitydescription,subject)
## Get the average of each variable
summariseddata <- summarise_each(allgroupeddata, funs(mean))
## Write the data out
write.table(summariseddata, "Average_Variable_By_Activity_Subject.txt", row.names = FALSE)
####
