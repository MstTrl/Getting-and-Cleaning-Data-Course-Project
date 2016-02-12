# run_analysis.R does the following:
# 
# 1- Merges the training and the test sets to create one data set.
# 2- Extracts only the measurements on the mean and standard deviation for each measurement.
# 3- Uses descriptive activity names to name the activities in the data set
# 4- Appropriately labels the data set with descriptive variable names.
# 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Function to check whether package is installed
is.installed <- function(pkg){is.element(pkg, installed.packages()[,1])} 

# check if package "dplyr" is installed, if not install
if (!is.installed("dplyr")){install.packages("dplyr")}

#Load the dplyr
library(dplyr)

########## 1 - Merges the training and the test sets to create one data set ###################

#Read the test data
X_test<-read.table('./test/X_test.txt')
y_test<-read.table('./test/y_test.txt')
subject_test<-read.table('./test/subject_test.txt')

#Read the training data
X_train<-read.table('./train/X_train.txt')
y_train<-read.table('./train/y_train.txt')
subject_train<-read.table('./train/subject_train.txt')

#Merge the data sets
X<-rbind(X_test,X_train)
y<-rbind(y_test,y_train)
colnames(y)<-"activityID"
subject<-rbind(subject_test,subject_train)
colnames(subject)<-"subject"

#remove test and training data sets from the global environment
rm("subject_test","subject_train","X_test","X_train","y_test","y_train")

########## 2 - Extracts only the measurements on the mean and standard deviation for each measurement. #############

#Read Features Table
features<-read.table('./features.txt')
colnames(features)<-c("ID","measurement")

#Create an index of the measurements which have mean (mean()) or standard deviation (std()) in the text
index<-grepl("mean\\(\\)|std\\(\\)",features$measurement)

#subset the features with selection criteria
features<-features[index,]

#subset the data set X with the index
X<-X[,index]

########## 3 - Uses descriptive activity names to name the activities in the data set. #############

#read activity labels
activity_labels<-read.table('./activity_labels.txt')
colnames(activity_labels)<-c("activityID","activityName")

#join data set y with the activity labels
y<-left_join(y,activity_labels,by='activityID')

########## 4 - Appropriately labels the data set with descriptive variable names.  #############

#name X data set variables
colnames(X)<-features$measurement
colnames(X)<-gsub("-","_",colnames(X))
colnames(X)<-gsub("mean\\(\\)","Mean",colnames(X))
colnames(X)<-gsub("std\\(\\)","STD",colnames(X))

#merge all the data into one single data set
data<-cbind(subject,activityName=y$activityName,X)

########## 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject..  #############
data_grouped<-group_by(data,subject,activityName)
summary_table<-summarize_each(data_grouped,funs(mean))
write.table(summary_table,'summary_table.txt',row.name=FALSE)
