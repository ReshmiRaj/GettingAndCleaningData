# Load the DataSet file and unzip it
activity_labels <-read.table("activity_labels.txt")
colnames(activity_labels)<-c("Activity_ID","Activity_Name")
# Features 
features <- read.table("features.txt")
# Get the test data 
YTestData <- read.table("YTestData.txt")
XTestData <- read.table("XTestData.txt")
subjectTestData <- read.table("subjectTestData.txt")
# Get the training data
YTrainData <- read.table("YTrainData.txt")
XTrainData <- read.table("XTrainData.txt")
subjectTrainData <- read.table("subjectTrainData.txt")
# Setting column names
colnames(XTestData)<-features[,2]
colnames(YTestData)<-c("Activity_ID")
colnames(subjectTestData)<-c("Subject ID")

colnames(XTrainData)<-features[,2]
colnames(YTrainData)<-c("Activity_ID")
colnames(subjectTrainData)<-c("Subject ID")
# Merge Test
testData<-cbind(YTestData,subjectTestData,XTestData)
# Merge Train
trainData<-cbind(YTrainData,subjectTrainData,XTrainData)
# Merge the test and train data together
completeData<-rbind(testData,trainData)
meanColIds<-grep("mean",colnames(completeData),ignore.case = TRUE)
stdColIds<-grep("std",colnames(completeData),ignore.case=TRUE)
requiredColIDs<-c(1,2,meanColIds,stdColIds)
# Get a data set with only columns providing mean and std values
dataSetWithOnlyMeanAndStdCols<-completeData[,requiredColIDs]
dataSetWithNames <- merge(activity_labels,dataSetWithOnlyMeanAndStdCols,by.x="Activity_ID",by.y="Activity_ID",all=TRUE)
# Cleaning up the variable names
columnNames<-colnames(dataSetWithNames)
for (i in 1:length(columnNames)) 
{
    columnNames[i] = gsub("\\()","",columnNames[i])
    columnNames[i] = gsub("-std$","StdDev",columnNames[i])
    columnNames[i] = gsub("-mean","Mean",columnNames[i])
    columnNames[i] = gsub("^(t)","time",columnNames[i])
    columnNames[i] = gsub("^(f)","freq",columnNames[i])
    columnNames[i] = gsub("([Gg]ravity)","Gravity",columnNames[i])
    columnNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNames[i])
    columnNames[i] = gsub("[Gg]yro","Gyro",columnNames[i])
    columnNames[i] = gsub("AccMag","AccMagnitude",columnNames[i])
    columnNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columnNames[i])
    columnNames[i] = gsub("JerkMag","JerkMagnitude",columnNames[i])
    columnNames[i] = gsub("GyroMag","GyroMagnitude",columnNames[i])
};
colnames(dataSetWithNames)<-columnNames
finalData  = dataSetWithNames[,names(dataSetWithNames) != 'Activity_Name'];
finaDataSet    = aggregate(finalData[,names(finalData) != c('Activity_ID','Subject ID')],by=list('Activity_ID'=finalData$'Activity_ID','Subject ID' = finalData$'Subject ID'),mean);## Create a file with the new tidy dataset
finalTidyDataSet <- merge(activity_labels,finaDataSet,by.x="Activity_ID",by.y="Activity_ID",all=TRUE)
# Export the finaDataSet set 
write.table(finalTidyDataSet, './Tidy.txt',row.names=TRUE,sep='\t')