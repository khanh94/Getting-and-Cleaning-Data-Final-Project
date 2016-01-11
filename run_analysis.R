
# Step1. Merges the training and the test sets to create one data set.
trainData <- read.table("X_train.txt")
dim(trainingData)
head(trainingData)
trainingLabel <- read.table("y_train.txt")
table(trainingLabel)
trainingSubject <- read.table("subject_train.txt")
testingData <- read.table("X_test.txt")
dim(testingData)
testingLabel <- read.table("y_test.txt") 
table(testingLabel) 
testSubject <- read.table("subject_test.txt")
joinedData <- rbind(trainingData, testingData)
dim(joinedData) # 10299*561
joinedLabel <- rbind(trainingLabel, testingLabel)
dim(joinedLabel) # 10299*1
joinedSubject <- rbind(trainingSubject, testingSubject)
dim(joinedSubject) # 10299*1

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinedData <- joinedData[, meanStdIndices]
dim(joinedData) # 10299*66
names(joinedData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinedData) <- gsub("mean", "Mean", names(joinedData)) # capitalize M
names(joinedData) <- gsub("std", "Std", names(joinedData)) # capitalize S
names(joinedData) <- gsub("-", "", names(joinedData)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinedLabel[, 1], 2]
joinedLabel[, 1] <- activityLabel
names(joinedLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinedSubject) <- "subject"
finalData <- cbind(joinedSubject, joinedLabel, joinedData)
dim(finalData) 
write.table(finalData, "final_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLength <- length(table(joinedSubject)) # 30
activityLength <- dim(activity)[1] # 6
columnLength <- dim(finalData)[2]
finalResult <- matrix(NA, nrow=subjectLength*activityLength, ncol=columnLength) 
finalResult <- as.data.frame(finalResult)
colnames(finalResult) <- colnames(finalData)
row <- 1
for(i in 1:subjectLength) {
  for(j in 1:activityLength) {
    finalResult[row, 1] <- sort(unique(joinedSubject)[, 1])[i]
    finalResult[row, 2] <- activity[j, 2]
    bool1 <- i == finalData$subject
    bool2 <- activity[j, 2] == finalData$activity
    finalResult[row, 3:columnLength] <- colMeans(finalData[bool1&bool2, 3:columnLength])
    row <- row + 1
  }
}
head(finalResult)
write.table(finalResult, "data_with_means.txt") # write out the 2nd dataset
