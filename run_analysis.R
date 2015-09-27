##You should create one R script called run_analysis.R that does the following. 

run_analysis <- function(){
    library(dplyr)
    library(data.table)
    library(tidyr)
    
    #Getting the data
    if(!file.exists("../project/dataset.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url, "../project/dataset.zip", method = "curl")
        unzip("../project/dataset.zip", exdir = "../project/")
    }
    
    ##1-Merges the training and the test sets to create one data set.
        #Reading and Merging train data
        dt_subject_train <- read.table("../project/UCI HAR Dataset/train/subject_train.txt")
        dt_x_train <- read.table("../project/UCI HAR Dataset/train/X_train.txt")
        dt_y_train <- read.table("../project/UCI HAR Dataset/train/y_train.txt")
     
        dt_train <- cbind(dt_x_train, dt_y_train,dt_subject_train)
        rm(dt_subject_train, dt_x_train, dt_y_train)
        
        #Reading and Merging test data
        dt_subject_test <- read.table("../project/UCI HAR Dataset/test/subject_test.txt")
        dt_x_test <- read.table("../project/UCI HAR Dataset/test/X_test.txt")
        dt_y_test <- read.table("../project/UCI HAR Dataset/test/y_test.txt")
  
        dt_test <- cbind( dt_x_test, dt_y_test, dt_subject_test)
        rm(dt_subject_test, dt_x_test, dt_y_test)
    
        #Merging train and test data
        dt <- rbind(dt_train, dt_test)
        colnames(dt) <- paste("col", 1:563, sep = "")
    
    ##2-Extracts only the measurements on the mean and standard deviation for each measurement. 
    dt_features <- read.table("../project/UCI HAR Dataset/features.txt")
    mean_std_columns <- grep("mean|std", dt_features[,2], ignore.case = TRUE)
    dt_std_mean <- cbind(dt[mean_std_columns], dt[562:563])

    ##3-Uses descriptive activity names to name the activities in the data set
    dt_activities <- read.table("../project/UCI HAR Dataset/activity_labels.txt")
    colnames(dt_activities) <- paste("col", 1:2, sep = "")
    dt_std_mean <- merge(dt_std_mean, dt_activities, by.x = "col562", by.y = "col1", all=TRUE)
        
    ##4-Appropriately labels the data set with descriptive variable names. 
    activity_names <- c("activity_id", as.vector(dt_features[mean_std_columns,2]), "subject", "activity")
    colnames(dt_std_mean) <- activity_names

    ##5-From the data set in step 4, creates a second, independent tidy data set with the average 
    ##of each variable for each activity and each subject.
       
        #In tidy data: Each variable forms a column and each observation forms a row.
        #Melting data because the features are mixed they and are columns instead of rows.
        dt_std_mean <- melt(dt_std_mean, id=c("subject", "activity", "activity_id"), measure.vars=2:87)

        #Calculating the average measure of each feature by activity and subject 
        dt_result <-  dt_std_mean %>%
             group_by(subject, 
                     activity,
                     variable)%>%
            summarize(avg_feature = mean(value))

        #Adjusting column names
        column_names <- c("subject_id", "activity", "measurement", "measurement_value")
        colnames(dt_result) <- column_names
}


