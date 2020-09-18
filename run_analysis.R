#Step 1; load libraries
library(plyr)

#Step 2; load and read in the data
if(!file.exists("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project")){dir.create("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/dataset.zip")
unzip(zipfile ="~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/dataset.zip", exdir= "~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project")

#Merge the training and the test sets to create one data set
#Step 3(a); read in training datasets 
x_training_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/train/X_train.txt")
y_training_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/train/y_train.txt")
subject_training_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/train/subject_train.txt")

#Step 3(b); read in testing dataset
x_testing_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/test/X_test.txt")
y_testing_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/test/y_test.txt")
subject_testing_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/test/subject_test.txt")

#Step 3(c); Read in feature vector
features_data <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/features.txt")

#Step 3(d); Read in activity labels
activityLabels <- read.table("~/Desktop/Additional Learning/Data Science Coursera/Getting and Cleaning Data/project/UCI HAR Dataset/activity_labels.txt")

#Step 3(e); assign column names
colnames(x_training_data) <- features_data[,2]
colnames(x_testing_data) <- features_data[,2]
colnames(y_training_data) <- "activityID"
colnames(y_testing_data) <- "activityID"
colnames(subject_training_data) <- "subjectID"
colnames(subject_testing_data) <- "subjectID" 
colnames(activityLabels) <- c("activityID", "activityType")

#Step 3(f): Merge all datasets into one dataset 
training_data <- cbind(y_training_data, subject_training_data, x_training_data)
testing_data <- cbind(y_testing_data, subject_testing_data, x_testing_data)
finaldataset <- rbind(training_data, testing_data)

#Extract only the measurements on the mean and standard deviation for each measurement.
#Step 4(a): Read in column names
colNames <- colnames(finaldataset)

#Step 4(b): Create vector for defining ID, mean, and sd
mean_and_std <- (grepl("activityID", colNames) |grepl("subjectID", colNames) |
                   grepl("mean..", colNames) | grepl("std...", colNames))

#Step 4(c): Subset the data
setforMeanandStd <- finaldataset [, mean_and_std == TRUE]

#Use descriptive activity names to name the activities in the data set
DataWithActivityNames <- merge(setforMeanandStd, activityLabels,
                              by = "activityID",
                              all.x = TRUE)
#Appropriately label the data set with descriptive variable names.
#This has already been covered in the above code

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Step 1: Make a second tidy data set
tidySet <- aggregate(. ~subjectID + activityID, DataWithActivityNames, mean)
tidySet <- tidySet[order(tidySet$subjectID, tidySet$activityID), ]

# Step 2: Write second tidy data set into a txt file
write.table(tidySet, "tidySet.txt", row.names = FALSE)




