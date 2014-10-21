# Step1. Merges the training and the test sets to create one data set.

trainData_X <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt")
dim(trainData_X)
trainData_Y <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\y_train.txt")
table(trainData_Y)
trainSubject <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt")

testData_X <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt")
dim(testData_X)
testData_Y <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\y_test.txt") 
table(testData_Y) 
testSubject <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt")

joinData_X <- rbind(trainData_X, testData_X)
dim(joinData_X)

joinData_Y <- rbind(trainData_Y, testData_Y)
dim(joinData_Y)

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\features.txt")
dim(features)

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)

joinData <- joinData_X[, meanStdIndices]
dim(joinData) 

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("F:\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))

activityLabel <- activity[joinData_Y[, 1], 2]
joinData_Y[, 1] <- activityLabel
names(joinData_Y) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinData_Y, joinData)
dim(cleanedData) 
#write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "cleanedData.txt") # write out the 2nd dataset
