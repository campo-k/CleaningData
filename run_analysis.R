# function for data load in local directory
loadData <- function() {
    # if you want run this code, change the directory.
    testS <- read.table("/Users/kbs/UCI HAR Dataset/test/subject_test.txt")
    testL <- read.table("/Users/kbs/UCI HAR Dataset/test/y_test.txt")
    testX <- read.table("/Users/kbs/UCI HAR Dataset/test/X_test.txt")
    trainS <- read.table("/Users/kbs/UCI HAR Dataset/train/subject_train.txt")
    trainL <- read.table("/Users/kbs/UCI HAR Dataset/train/y_train.txt")
    trainX <- read.table("/Users/kbs/UCI HAR Dataset/train/X_train.txt")
    featureName <- read.table("/Users/kbs/UCI HAR Dataset/features.txt")
    activityLabel <- read.table("/Users/kbs/UCI HAR Dataset/activity_labels.txt")
    
    # return data by list
    dataList <- list(testS, testL, testX, trainS, trainL, trainX, featureName, activityLabel)
}

# function for merge data
dataMerge <- function(dataList) {
    # variable assignment from argument
    testS <- dataList[[1]]
    testL <- dataList[[2]]
    testX <- dataList[[3]]
    trainS <- dataList[[4]]
    trainL <- dataList[[5]]
    trainX <- dataList[[6]]
    featureName <- dataList[[7]]
    activityLabel <- dataList[[8]]
    
    # change column name (subject & activity)
    colnames(testS) <- c("subject")
    colnames(testL) <- c("activity")
    colnames(trainS) <- c("subject")
    colnames(trainL) <- c("activity")
    
    # change row name (to rbind function below)
    rownames(trainS) <- c(nrow(testS) + 1):c(nrow(testS) + nrow(trainS))
    rownames(trainL) <- c(nrow(testL) + 1):c(nrow(testL) + nrow(trainL))
    rownames(trainX) <- c(nrow(testX) + 1):c(nrow(testX) + nrow(trainX))
    
    # merge test data & train data
    mergeS <- rbind(testS, trainS)
    mergeL <- rbind(testL, trainL)
    mergeX <- rbind(testX, trainX)
    
    # change column name (a 561-feature vector's real variable name)
    an1 <- c("^tBodyAcc-", "^tGravityAcc-", "^tBodyAccJerk-", "^tBodyGyro-", "^tBodyGyroJerk-",
             "^tBodyAccMag-", "^tGravityAccMag-", "^tBodyAccJerkMag-", "^tBodyGyroMag-",
             "^tBodyGyroJerkMag-", "^fBodyAcc-", "^fBodyAccJerk-", "^fBodyGyro-", 
             "^fBodyAccMag-", "^fBodyBodyAccJerkMag-", "^fBodyBodyGyroMag-", "^fBodyBodyGyroJerkMag-")
    an2 <- c("t01-", "t02-", "t03-", "t04-", "t05-", "t06-", "t07-", "t08-", "t09-", "t10-",
             "f01-", "f02-", "f03-", "f04-", "f05-", "f06-", "f07-", "f08-", "f09-", "f10-",
             "f11-", "f12-", "f13-", "f14-")
    for (i in c(1:17)) { featureName$V2 <- sub(an1[i], an2[i], featureName$V2) }
    colnames(mergeX) <- featureName$V2
    
    # measurements mean & standara deviation's column number
    extNameMean <- grep("mean\\(\\)", featureName$V2)
    extNamestd <- grep("std\\(\\)", featureName$V2)
    extName <- sort(c(extNameMean, extNamestd))
    
    # create tidy data
    dataSet <- mergeS
    dataSet$activity <- mergeL$activity
    for (i in extName) { dataSet[as.character(featureName$V2[i])] <- mergeX[, i] }
    for (i in c(1:6)) { 
        dataSet$activity <- sub(as.character(activityLabel$V1[i]), as.character(activityLabel$V2[i]), dataSet$activity) 
    }
    colnames(dataSet) <- sub("-mean\\(\\)-", "m", colnames(dataSet))
    colnames(dataSet) <- sub("-mean\\(\\)", "m", colnames(dataSet))
    colnames(dataSet) <- sub("-std\\(\\)-", "s", colnames(dataSet))
    colnames(dataSet) <- sub("-std\\(\\)", "s", colnames(dataSet))
    dataSet
    
    #dataSet["mean"] <- rowMeans(dataSet[,3:ncol(dataSet)])
}

# function for second data, average of each variable for each subject.
avrBySubject <- function(dataSet) {
    # use reshape2 library
    library(reshape2)
    meltDataSet <- melt(dataSet, id=c("subject", "activity"))
    aggData <- dcast(meltDataSet, subject ~ variable, mean)
    aggData
}

# function for second data, average of each variable for each activity.
avrByActivity <- function(dataSet) {
    # use reshape2 library
    library(reshape2)
    meltDataSet <- melt(dataSet, id=c("subject", "activity"))
    aggData <- dcast(meltDataSet, activity ~ variable, mean)
    aggData
}