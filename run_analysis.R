library(dplyr)

zipfile <- "getdata_projectfiles_UCI HAR Dataset.zip"

# Checking if zip file already exists.
if (!file.exists(zipfile)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, zipfile, method="curl")
}  

datafolder <- "UCI HAR Dataset"

# Checking if folder exists
if (!file.exists(datafolder)) { 
  unzip(zipfile) 
}

features <- read.table(paste(sep="",datafolder,"/features.txt"), col.names = c("n","functions"))
activities <- read.table(paste(sep="",datafolder,"/activity_labels.txt"), col.names = c("code", "activity"))

subject_test <- read.table(paste(sep="",datafolder,"/test/subject_test.txt"), col.names = "subject")
x_test <- read.table(paste(sep="",datafolder,"/test/X_test.txt"), col.names = features$functions)
y_test <- read.table(paste(sep="",datafolder,"/test/y_test.txt"), col.names = "code")

subject_train <- read.table(paste(sep="",datafolder,"/train/subject_train.txt"), col.names = "subject")
x_train <- read.table(paste(sep="",datafolder,"/train/X_train.txt"), col.names = features$functions)
y_train <- read.table(paste(sep="",datafolder,"/train/y_train.txt"), col.names = "code")

# Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)

# Extracts only the measurements on the mean and standard deviation for each measurement.
selectedCols <- grep("-(mean|std).*", as.character(features[,2]))
X <- X[selectedCols]
MergedData <- cbind(Subject, Y, X)

# Uses descriptive activity names to name the activities in the data set
MergedData$code <- activities[MergedData$code, 2]

#Appropriately labels the data set with descriptive variable names
selectedColNames <- features[selectedCols, 2]
selectedColNames <- gsub("-mean", "Mean", selectedColNames)
selectedColNames <- gsub("-std", "Std", selectedColNames)
selectedColNames <- gsub("[-()]", "", selectedColNames)

colnames(MergedData) <- c("Subject", "Activity", selectedColNames)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
TidyData <- MergedData %>%
  group_by(Subject, Activity) %>%
  summarise_all(mean)
write.table(TidyData, "TidyData.txt", row.name=FALSE)
