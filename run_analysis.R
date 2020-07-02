#run_analysis.R

#get information from original dataset
dataset <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dataset)

x_train <- read.table(unz(dataset, "UCI HAR Dataset/train/X_train.txt"))
y_train <- read.table(unz(dataset, "UCI HAR Dataset/train/y_train.txt"))
sub_train <- read.table(unz(dataset, "UCI HAR Dataset/train/subject_train.txt"))

x_test <- read.table(unz(dataset, "UCI HAR Dataset/test/X_test.txt"))
y_test <- read.table(unz(dataset, "UCI HAR Dataset/test/y_test.txt"))
sub_test <- read.table(unz(dataset, "UCI HAR Dataset/test/subject_test.txt"))

features <- read.table(unz(dataset, "UCI HAR Dataset/features.txt"))
activities <- read.table(unz(dataset, "UCI HAR Dataset/activity_labels.txt"))

unlink(dataset)

#merge training and test datasets together into labData
x_merge <- rbind(x_train, x_test)
y_merge <- rbind(y_train, y_test)
sub_merge <- rbind(sub_train, sub_test)
labData <- cbind(sub_merge, x_merge, y_merge)

#label dataset columns 
colnames(labData) <- c("subject", features[,2], "activity")

#extract only mean and standard deviation for each measurement
keepCols <- grepl("subject|activity|mean|std", colnames(labData))
labData <- labData[, keepCols]

#use activity names to name activities
labData$activity <- factor(labData$activity, levels = activities[,1], labels = activities[,2])

#label data set with descriptive variable names
labDataCols <- colnames(labData)
labDataCols <- gsub("()-", "", labDataCols)
labDataCols <- gsub("Mag", "Magnitude", labDataCols)
labDataCols <- gsub("Gyro", "Gyroscope", labDataCols)
labDataCols <- gsub("^f", "frequencyDomain", labDataCols)
labDataCols <- gsub("^t", "timeDomain", labDataCols)
labDataCols <- gsub("mean", "Mean", labDataCols)
labDataCols <- gsub("std", "StandardDeviation", labDataCols)

colnames(labData) <- labDataCols

#create second tidy data set with average of each variable for each activity and each subject
labDataMeans <- labData %>% group_by(subject, activity) %>% summarize_each(list(mean))

write.table(labDataMeans, "tidy_data.txt", quote=FALSE, row.names=FALSE)
