## System (Linux) 
## Set the working directory as the "UCI HAR Dataset" folder
setwd("./UCI HAR Dataset")

# Loading the packages required
library(dplyr)   #  Package dplyr version 0.4.1
library(tidyr)   #  Package tidyr version 0.2.0

## Reading data
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

subject_train <- read.table("train/subject_train.txt")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

subject_test <- read.table("test/subject_test.txt")
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

###################################################################################
#       1. Merging the training and the test sets to create one data set.
###################################################################################

## Creating the complete train_data and test_data sets
train_data <- cbind(subject_train, y_train, X_train, Set="train")
test_data <- cbind(subject_test, y_test, X_test, Set="test")

## Joining the training and the test sets to create one data set
all_data <- rbind(train_data, test_data)

## Adding columns names
colnames(all_data) <- c("Subject","Activity",as.character(features[,"V2"]),"Set")

###################################################################################
#       2. Extracting only the measurements on the mean and standard deviation for each measurement. 
###################################################################################

subject_column <- which(colnames(all_data)=="Subject")
activity_column <- which(colnames(all_data)=="Activity")
mean_columns <- grep("mean[()]", colnames(all_data))
std_columns <- grep("std[()]", colnames(all_data))
set_column <- which(colnames(all_data)=="Set")

extracted_data <- all_data[,c(set_column,subject_column, activity_column, mean_columns, std_columns)]

###################################################################################
#       3. Using descriptive activity names to name the activities in the data set. 
###################################################################################

extracted_data$Activity <- activity_labels[,2][match(extracted_data$Activity, activity_labels[,1])]

###################################################################################
#       4. Appropriately labeling the data set with descriptive variable names.  
###################################################################################

colnames(extracted_data) <- gsub("[()]", "", colnames(extracted_data))
colnames(extracted_data) <- gsub("BodyBody", "Body", colnames(extracted_data))

###################################################################################
#       5. From the data set in step 4, let's create a second, independent tidy data set 
#          with the average of each variable for each activity and each subject. 
###################################################################################


## Grouping the data by activity and by subject
data_by_activity_subject <- group_by(extracted_data, Set, Subject, Activity)

## Average of each variable for each activity and each subject 
each_variable_summarised <- summarise_each(data_by_activity_subject, funs(mean), -(1:3))
tidy_data_set <- gather(each_variable_summarised, Measurement, Average,-(1:3))
