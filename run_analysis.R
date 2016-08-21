##Run_Analysis.R is the script for the Peer Graded Assigment of Geetting and Clearning Data

##This will work in your current working directory

##Review criteria:
###1.The submitted data set is tidy.
###2.The Github repo contains the required scripts.
###3.GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
###4.The README that explains the analysis files is clear and understandable.
###5.The work submitted for this project is the work of the student who submitted it.

##Requirments of the code:
###1.Merges the training and the test sets to create one data set.
###2.Extracts only the measurements on the mean and standard deviation for each measurement.
###3.Uses descriptive activity names to name the activities in the data set
###4.Appropriately labels the data set with descriptive variable names.
###5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
################################################################################################

#load packages
library(dplyr)
library(data.table)
library(tidyr)


#check if the folder data exists in the current working directory, if not create it
if(!file.exists("./data")){dir.create("./data")}
#set url path where the data is located
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download the data
download.file(fileUrl,destfile="Dataset.zip")
#Unzip Dataset
unzip(zipfile="Dataset.zip",exdir="data")

#set file path
filesPath <- "data/UCI HAR Dataset"

#read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

#read activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#read data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

##Req 1: Merges the training and the test sets to create one data set.

#for both Activity and Subject files this will merge the training and the test sets by row binding 
#and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#combine the DATA training and test files
dataTable <- rbind(dataTrain, dataTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

##Req 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

#taking only measurements for the mean and standard deviation and add "subject","activityNum"
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##Req 3: Uses descriptive activity names to name the activities in the data set
##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

##Req 4: Appropriately labels the data set with descriptive variable names.
#Names before
head(str(dataTable),2)
##leading t = time
##leading f = frequency
##Body = BodyBody(related to body movement.)
##Accelerometer = Acc(accelerometer measurement)
##Gyroscopic =Gyro(gyroscopic measurements)
##Magnitude = Mag(magnitude of movement)
##mean = MEAN
##SD = std

##Gravity left as Gravity(acceleration of gravity)
##Jerk left as Jerk(sudden movement acceleration)

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# Names after
head(str(dataTable),6)

##Req 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##write to text file on disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)
