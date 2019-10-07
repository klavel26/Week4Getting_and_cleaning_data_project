library(plyr)

#### Download data
if(!file.exists("./tidy_data_project")){dir.create("./tidy_data_project")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./tidy_data_project/tidy_project_data.zip")

## unzip dataset
unzip(zipfile = "./tidy_data_project/tidy_project_data.zip", exdir = "./tidy_data_project")

## Project Outline
	#1) Merges the training and the test sets to create one data set.
	#2) Extracts only the measurements on the mean and standard deviation for each measurement.
	#3) Uses descriptive activity names to name the activities in the data set
	#4) Appropriately labels the data set with descriptive variable names.
	#5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
	
	
	#### #1 Merging training and test sets to create one data set
	
	# 1.1 set up pathway
	path <- "./tidy_data_project/UCI HAR Dataset"
	list.files(path)
	
	## 1.2 double check to make sure everything is there
	files <- list.files(path, recursive = TRUE)
	files
# [1] "activity_labels.txt"                          "features_info.txt"                           
# [3] "features.txt"                                 "README.txt"                                  
# [5] "test/Inertial Signals/body_acc_x_test.txt"    "test/Inertial Signals/body_acc_y_test.txt"   
#[7] "test/Inertial Signals/body_acc_z_test.txt"    "test/Inertial Signals/body_gyro_x_test.txt"  
# [9] "test/Inertial Signals/body_gyro_y_test.txt"   "test/Inertial Signals/body_gyro_z_test.txt"  
#[11] "test/Inertial Signals/total_acc_x_test.txt"   "test/Inertial Signals/total_acc_y_test.txt"  
#[13] "test/Inertial Signals/total_acc_z_test.txt"   "test/subject_test.txt"                       
#[15] "test/X_test.txt"                              "test/y_test.txt"                             
#[17] "train/Inertial Signals/body_acc_x_train.txt"  "train/Inertial Signals/body_acc_y_train.txt" 
#[19] "train/Inertial Signals/body_acc_z_train.txt"  "train/Inertial Signals/body_gyro_x_train.txt"
#[21] "train/Inertial Signals/body_gyro_y_train.txt" "train/Inertial Signals/body_gyro_z_train.txt"
#[23] "train/Inertial Signals/total_acc_x_train.txt" "train/Inertial Signals/total_acc_y_train.txt"
#[25] "train/Inertial Signals/total_acc_z_train.txt" "train/subject_train.txt"                     
# [27] "train/X_train.txt"     "train/y_train.txt"      
       
       
       ### 1.3 Read in training datasets               
	xtrain <- read.table(file.path(path, "train", "X_train.txt"), header = FALSE)
	ytrain <- read.table(file.path(path, "train", "Y_train.txt"), header = FALSE)
	ytrain <- read.table(file.path(path, "train", "y_train.txt"), header = FALSE)
	subject_train <- read.table(file.path(path, "train", "subject_train.txt"), header = FALSE)

	### 1.4 Read in testing datasets
	xtest = read.table(file.path(path, "test", "X_test.txt"), header = FALSE)
	ytest = read.table(file.path(path, "test", "y_test.txt"), header = FALSE)
	subject_test = read.table(file.path(path, "test", "subject_test.txt"), header = FALSE)                       

	### 1.5 Read in Features 
	features <- read.table(file.path(path, "features.txt"), header = FALSE)
	
	### 1.6 Read in activity labels
	activity_labels <- read.table(file.path(path, "activity_labels.txt"), header = FALSE)

	### 1.7 Looking at activity/features + description - easier to assign names now
	colnames(xtrain) = features[,2]
	colnames(ytrain) = "activity_id"
	colnames(subject_train) = "subject_id"
	
	colnames(xtest) <- features[,2]
	colnames(ytest) <- "activity_id"
	colnames(subject_test) = "subject_id"
	
	colnames(activity_labels) <- c('activity_id', 'activity_type')
	
	## Double check to make sure looks like what I want
	head(activity_labels)
# activity_id      activity_type
#1           1            WALKING
#2           2   WALKING_UPSTAIRS
#3           3 WALKING_DOWNSTAIRS
#4           4            SITTING
#5           5           STANDING
#6           6             LAYING

	#### 1.8 Actually Merge
	training_merge <- cbind(ytrain, subject_train, xtrain)
	testing_merge <- cbind(ytest, subject_test, xtest)
	full_merge <- rbind(training_merge, testing_merge)

	#2) Extracts only the measurements on the mean and standard deviation for each measurement
	
	### 2.1 Check column names
	column_names = colnames(full_merge)
	
	### 2.2 Make our vector for Defining activity_id, subject_id, mean, std
	mean_std <- (grepl("activity_id", column_names) | grepl("subject_id", column_names) | grepl("mean..", column_names) | grepl("std..", column_names))
	
	### 2.3 Subsetting data
	set_mean_std <- full_merge[ , mean_std == TRUE]
	
	#3) Uses descriptive activity names to name the activities in the data set	
	set_with_names <- merge(set_mean_std, activity_labels, by = 'activity_id', all.x = TRUE)
	
	#4) Appropriately labels the data set with descriptive variable names.
		### look in 1.7 and 2.2/2.3
	
	#5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
	# 5.1 creating a second tidy set of data
	 new_tidy_set <- aggregate(. ~subject_id + activity_id, set_with_names, mean)
	 new_tidy_set_final <- new_tidy_set[order(new_tidy_set$subject_id, new_tidy_set$activity_id),]
 
	#5.2 Writing the output to txt file
	write.table(new_tidy_set_final, "new_tidy_set_final.txt", row.name = FALSE)
