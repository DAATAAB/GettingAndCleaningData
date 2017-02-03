  
  #This is the run_analysis.R script for the Coursera Getting and Cleaning Data course final assignment

  library(dplyr)

  #Getting the data set from the URL
  
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                destfile = "data.zip")
  unzip("data.zip")
  
  # Reading the training dataset
  
  training_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  training_values <- read.table("./UCI HAR Dataset/train/X_train.txt")
  training_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
  
  colnames(training_subjects) <- "Subject"
  colnames(training_labels) <- "Activity"
  
  
  training_data <- cbind(training_subjects, training_labels, training_values)
  
  #Reading the test dataset
  
  test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  test_values <- read.table("./UCI HAR Dataset/test/X_test.txt")
  test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
  
  colnames(test_subjects) <- "Subject"
  colnames(test_labels) <- "Activity"
  
  
  test_data <- cbind(test_subjects, test_labels, test_values)
  
  #Reading the feature and activity names
  
  features <- read.table("./UCI HAR Dataset/features.txt")
  features_char <- lapply(features$V2, as.character)
  
  activity_names <- read.table("./UCI HAR Dataset/activity_labels.txt")
  
  #Binding the two datasets into the master dataset
  
  data_set <- rbind(test_data, training_data)
  colnames(data_set) <- c("Subject", "Activity", features_char)
  
  
  #Subsetting the columns containing the means and standard deviations
  
  data_mean<- data.frame(c(data_set[,1:2], data_set[,grepl(c("mean\\(\\)"), colnames(data_set)) == TRUE]))
  data_std <- data.frame(data_set[,grepl("std\\(\\)", colnames(data_set)) == TRUE])
  data_mean_std <- cbind(data_mean, data_std)
  
  #Transforming activity and variable names
  
  for (i in 1:nrow(activity_names)) {data_mean_std$Activity <- sub(i, activity_names[i,2], data_mean_std$Activity)}
  
  colnames(data_mean_std) <- sub(".mean..", "Mean", colnames(data_mean_std))
  colnames(data_mean_std) <- sub(".std..", "STD", colnames(data_mean_std))
  
  
  #### Here ends the first part of the assignment
  
  
  #Creating the tidy dataset
  
  data_tidy <- aggregate(data_mean_std, by = list(data_mean_std$Activity ,data_mean_std$Subject), 
                         FUN = mean)
  
  data_tidy$Activity <- data_tidy$Group.1
  
  data_tidy$Group.1 <- NULL
  data_tidy$Group.2 <- NULL
  
  #Writing the tidy data to a .txt file

  write.table(data_tidy, file = "data_tidy.txt")
  
