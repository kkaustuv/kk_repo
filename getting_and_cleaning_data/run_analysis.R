#library(dplyr)
#library(plyr)
#Step 1 - Store the file path ia variable
f_path<-file.path(".","UCI HAR Dataset")
#Step 2 - Read Activity data from both test and train
Activity_Test<-read.table(file.path(f_path,"test","y_test.txt"),header=FALSE)
Activity_Train<-read.table(file.path(f_path,"train","y_train.txt"),header=FALSE)
#Step 3 - Read Subject files
Subject_Test<-read.table(file.path(f_path,"test","subject_test.txt"),header=FALSE)
Subject_Train<-read.table(file.path(f_path,"train","subject_train.txt"),header=FALSE)
#Step 4 - Read Feature files
Feature_Test<-read.table(file.path(f_path,"test","X_test.txt"),header=FALSE)
Feature_Train<-read.table(file.path(f_path,"train","X_train.txt"),header=FALSE)
#Step 5 - Combine/Merge Activity , Subject and Feature data for both test and train
m_activity<-rbind(Activity_Test,Activity_Train)
m_subject<-rbind(Subject_Test,Subject_Train)
m_feature<-rbind(Feature_Test,Feature_Train)
#Step 6 - Assign Descriptive Column names : For feature data column names are fetched from feature.txt
names(m_activity)<-c("activity")
names(m_subject)<-c("subject")
feature_names<-read.table(file.path(f_path,"features.txt"),header = FALSE)
names(m_feature)<-feature_names$V2
# Step 7 - Merge all data into a sigle dataset 
m_as_data<-cbind(m_activity,m_subject)
m_data<-cbind(m_as_data,m_feature)
#Step 8 - Extract only mean ans standard deviation related measurement
mean_std_col_names<-feature_names$V2[grep("mean\\(\\)|std\\(\\)",feature_names$V2)]
select_names<-c(as.character(mean_std_col_names),"activity","subject")
m_mean_std_data<-subset(m_data,select=select_names)
#Step 9 - Assign Activity Lables for all activities in dataset
activity<-read.table(file.path(f_path,"activity_labels.txt"),header = FALSE)
m_mean_std_data$activity<-activity[m_mean_std_data$activity,2]
#Step 10 - Appropreatly label variable names with descriptive values
names(m_mean_std_data)<-gsub("^t","time",names(m_mean_std_data))
names(m_mean_std_data)<-gsub("^f","frequency",names(m_mean_std_data))
names(m_mean_std_data)<-gsub("Acc", "Accelerometer",names(m_mean_std_data))
names(m_mean_std_data)<-gsub("Gyro", "Gyroscope",names(m_mean_std_data))
names(m_mean_std_data)<-gsub("Mag", "Magnitude",names(m_mean_std_data))
names(m_mean_std_data)<-gsub("BodyBody", "Body",names(m_mean_std_data))
#Step 11 - Create new tidy dataset with average of all variables grouped on subject and activity
m_avg_data <- aggregate(. ~subject+activity,m_mean_std_data,mean)
write.table(m_avg_data, file = "averagedata.txt")
