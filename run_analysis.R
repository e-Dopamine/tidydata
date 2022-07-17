
library(dplyr)
get_data <- function(data_file='UCI_HAR_Dataset.zip'){
  url<-' https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  if(!file.exists(data_file)){
    download.file(url,data_file)
  }
  if(!file.exists('UCI HAR Dataset')){  
    unzip('UCI_HAR_Dataset.zip')
  }
}

get_data()
activity_labels <- read.table('UCI HAR Dataset/activity_labels.txt',col.names=c('label','act_name'))
features <- read.table('UCI HAR Dataset/features.txt',as.is=T,
                       col.names=c('col_num','act_name'))


features$act_name<-gsub('[.]{3}','.',make.names(features$act_name))
features$act_name<-gsub('[.]{2}','.',make.names(features$act_name))
features$act_name<-gsub('[.]$','',make.names(features$act_name))
features$act_name<-make.unique(features$act_name)

x_train<-read.table('UCI HAR Dataset/train/X_train.txt',
                    col.names = features$act_name)
y_train<-read.table('UCI HAR Dataset/train/y_train.txt', col.names = c('activity'))
subject_train <- read.table('UCI HAR Dataset/train/subject_train.txt',col.names=c('subject'))


x_test<-read.table('UCI HAR Dataset/test/X_test.txt',
                   col.names = features$act_name)
y_test<-read.table('UCI HAR Dataset/test/y_test.txt', col.names = c('activity'))
subject_test <- read.table('UCI HAR Dataset/test/subject_test.txt',col.names=c('subject'))

# "Merges the training and the test sets to create one data set."
# Merge the test and training data. 
df <- rbind(cbind(subject_train, y_train, x_train),cbind(subject_test, y_test, x_test))



df <-
  df %>% select(subject, activity, contains('mean'), contains('std'))

# Q4
df$activity<-activity_labels[df$activity,'act_name']


tidy<-aggregate(df[,3:ncol(df)],by=list(df$subject,df$activity), mean)


colnames(tidy)[2]<-'activity'
colnames(tidy)[1]<-'subject'
tidy<-tidy[order(tidy$subject),]
write.table(tidy,'tidy.txt',row.names=F)