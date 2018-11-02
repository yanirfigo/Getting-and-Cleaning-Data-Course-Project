library(dplyr)
library(tidyr)
library(stringer)
library(data.table)

###getting the data into R

##getting the activity lables and vars names

# getting the activies labels
activity_labels<- read.table("UCI HAR Dataset/activity_labels.txt")
#getting the vars names
features<- read.table("UCI HAR Dataset/features.txt")

####extracting only data with means and std's
mean_std_col<- grep("mean|std",features[,2])
mean_std_col_names<- as.character(features[mean_std_col,2])
##getting the test data

#getting the subject ID
subject_test<- read.table("UCI HAR Dataset/test/subject_test.txt")
#getting the activity ID
y_test<- read.table("UCI HAR Dataset/test/y_test.txt")
#getting the observations
x_test<- fread("UCI HAR Dataset/test/x_test.txt",select =mean_std_col)

###merging the test data
test_data<- cbind(subject_test,y_test,x_test)

##getting the train data

#getting the subject ID
subject_train<- read.table("UCI HAR Dataset/train/subject_train.txt")
#getting the activity ID
y_train<- read.table("UCI HAR Dataset/train/y_train.txt")
#getting the observations
x_train<- fread("UCI HAR Dataset/train/x_train.txt",select =mean_std_col)

###merging the train data
train_data<- cbind(subject_train,y_train,x_train)

####merging train and test data
full_data<- rbind(test_data,train_data)

### give names to the vars and 
colnames(full_data)<- c("subject_id","activity",mean_std_col_names)

##give better names to the vars
colnames(full_data)<- gsub("-mean"," MEAN",colnames(full_data))
colnames(full_data)<- gsub("-std"," STD",colnames(full_data))
colnames(full_data)<- gsub("[()]"," ",colnames(full_data))
colnames(full_data)<- gsub("-","",colnames(full_data))
colnames(full_data)<- gsub("  "," ",colnames(full_data))

###give a descrptive names for the activies
full_data$activity <- factor(full_data$activity, levels = activity_labels[,1], labels = activity_labels[,2])

#####build a new data with summary of means grouped by activity and subject ID
means_data<- aggregate(full_data,by= list(full_data$activity,full_data$subject_id),FUN = mean)
means_data<- means_data%>%
        select(-(1:2))

write.table(means_data, "tidy.txt", row.names = FALSE, quote = FALSE)