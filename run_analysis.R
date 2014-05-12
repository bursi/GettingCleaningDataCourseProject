# load data

traindataraw <- read.table(file="D:/R/UCI HAR Dataset/train/X_train.txt", header=FALSE, skip=0, sep="");
testdataraw <- read.table(file="D:/R/UCI HAR Dataset/test/X_test.txt", header=FALSE, skip=0, sep="");
trainlabels <- read.table(file="D:/R/UCI HAR Dataset/train/y_train.txt");
testlabels <- read.table(file="D:/R/UCI HAR Dataset/test/y_test.txt");
trainsubject <- read.table(file="D:/R/UCI HAR Dataset/train/subject_train.txt");
testsubject <- read.table(file="D:/R/UCI HAR Dataset/test/subject_test.txt");
features <- read.table(file="D:/R/UCI HAR Dataset/features.txt");
activities <- read.table(file="D:/R/UCI HAR Dataset/activity_labels.txt");

# label the columns with the labels in the feature description

colnames(testdataraw) <- features[,2];
colnames(traindataraw) <- features[,2];

# extract only measurements with mean and standard deviation using grep

traindata <- traindataraw[grep("mean|std",features[,2])];
testdata <- testdataraw[grep("mean|std",features[,2])];
rm(traindataraw);
rm(testdataraw);

# add the Activity label and the subject to the datasets

traindata$ActivityLabel <- trainlabels[,1];
testdata$ActivityLabel <- testlabels[,1];
traindata$Subject <- trainsubject[,1];
testdata$Subject <- testsubject[,1];

# merge the two datasets

dataset <- merge(traindata, testdata, all=TRUE, sort=FALSE);

# use descriptive activity names

for (i in 1:dim(dataset)[1])
{
    dataset$ActivityName[i] <- as.character(activities[dataset$ActivityLabel[i],2]);
}

# calculate averages of the measurements for each test subject and each activity

tidymeans <- data.frame(matrix(nrow=36, ncol=81));
colnames(tidymeans)[1] <- "Subject";
colnames(tidymeans)[2] <- "Activity";
colnames(tidymeans)[3:81] <- colnames(dataset)[1:79];
for (j in 1:30)
{
    k <- 1;
    for (k in 1:6)
    {
        tidymeans[6*(j-1)+k,1] <- j;
        tidymeans[6*(j-1)+k,2] <- as.character(activities[k,2]);
        tidymeans[6*(j-1)+k,3:81] <- colMeans(dataset[dataset$Subject==j & dataset$ActivityLabel==k,1:79]);
    }
}

# write the resulting data set to a .txt file

write.table(tidymeans, file="D:/R/TidyMeanDataSet.txt", sep=",", row.names=FALSE);

