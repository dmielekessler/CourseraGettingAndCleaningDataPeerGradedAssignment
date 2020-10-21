#This is a script for the the Coursera Getting and Cleaning Data course project.

#check if the tidyverse package is installed and install if it isn't
invisible(if(!is.element("tidyverse", installed.packages()[,1])){install.packages("tidyverse")})

# load the tidyverse library  
library(tidyverse)

# Establish the location of the initial data zip folder 
zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Store the current working directory as a text string for use in navigating to various folders 
startingwd <- getwd()

# Store the future zip file name 
zipfile <- "getdata_projectfiles_UCI HAR Dataset.zip"

# Store the location of the zip file 
zipcombine <- as.character(paste(startingwd, zipfile, sep = "/"))

# If the file exists, remove it 
invisible(if(file.exists(zipcombine)) {file.remove(zipcombine)})

# Store the directory location for the unzipped folder
datadir <- paste0(startingwd, "/UCI HAR Dataset")

# If the folder exists, remove it
invisible(if(dir.exists(datadir)) {unlink(datadir, recursive = TRUE)})

# Download and store the zip file 
download.file(zipurl, destfile = zipcombine)
# (It appears one does not have to be logged in to coursera.org to complete this step)

# Unzip the file 
unzip(zipfile)

# store the measurment names 
measurementnames <- read_table(paste0(datadir, "/features.txt"), col_names = FALSE)

# transpose the measurement names so that it is 1x561 instead of 561x1 
measurementnames <- t(measurementnames)

# read in the test set measurements  
xtest <- read_delim(paste0(datadir, "/test/X_test.txt"), delim = " ", col_names = FALSE)

# update the measurements in the test set from character to numeric 
xtest <- xtest %>% mutate_if(is.character,as.numeric)

# name the xtest columns 
colnames(xtest) <- measurementnames

# read in the test set activity codes 
testactivities <- read_table(paste0(datadir, "/test/y_test.txt"), col_names = FALSE)

# read in the test set subjects 
testsubjects <- read_table(paste0(datadir, "/test/subject_test.txt"), col_names = FALSE)

# Read in the activity labels 
activitylabels <- read_table(paste0(datadir, "/activity_labels.txt"), col_names = FALSE)

# Name the activity labels column as activity 
colnames(activitylabels) <- c("activityindicator", "activityname")

# Name the test set activities column 
colnames(testactivities) <- c("activityindicator")

#21. merge the activity labels into the test set activities 
testactivities <- merge(testactivities, activitylabels)

#22. Column bind the test set subjects and activities to the test set measurements 
xtest <- cbind(testsubjects, testactivities, xtest)

## Now we're going to do the similar steps for the training data set.

# read in the training set measurements 
xtrain <- read_delim(paste0(datadir, "/train/X_train.txt"), delim = " ", col_names = FALSE)

# update the measurements in the training set from character to numeric 
xtrain <- xtrain %>% mutate_if(is.character,as.numeric)

# name the xtrain columns 
colnames(xtrain) <- measurementnames

# read in the training set activity codes 
trainactivities <- read_table(paste0(datadir, "/train/y_train.txt"), col_names = FALSE)

# read in the test set subjects 
trainsubjects <- read_table(paste0(datadir, "/train/subject_train.txt"), col_names = FALSE)

# Name the training set activities column 
colnames(trainactivities) <- c("activityindicator")

# merge the activity labels into the training set activities 
trainactivities <- merge(trainactivities, activitylabels)

# Column bind the training set subjects and activities to the training set measurements 
xtrain <- cbind(trainsubjects, trainactivities, xtrain)

# Combine the test and train data sets into one table 
alldata <- rbind(xtrain, xtest)

# Determine the locations of the mean and standard deviation variables 
meanandstdlocations <- intersect(grep("mean|std", measurementnames), grep("meanFreq", measurementnames, invert = TRUE))
#The above intentionally excludes the meanFreq measurements, as those are "Weighted average of the frequency components to obtain a mean frequency", which is distinct from the mean, which is Mean value, and is a member of the set for the same measurements such as Body Acceleration Jerk and Body Gyro.

# Create a reduced data out the subject and activity columns in preparation to reduce to only the mean and standard deviation measurements 
meanandstddata <- alldata[,-c(1:3)]

# Create a reduced data set containing only the mean and standard deviation measurements 
meanandstddata <- meanandstddata[,meanandstdlocations]

# Column bind the subjects and activities to the mean and stadard deviation measurements 
meanandstddata <- cbind(alldata[,c(1,3)], meanandstddata)

# Rename the first column in the mean and stadard deviation data set to a descriptive name 
colnames(meanandstddata)[1] <- "subject"

# Store the mean and stadard deviation data set column names to a vector 
thenames <- colnames(meanandstddata)

# Remove the leading digits and spaces from the names vector 
thenames <- gsub("\\d+ ","", thenames)

# Remove the opening parentheses from the names vector 
thenames <- gsub("\\(+","", thenames)

# Remove the closing parentheses from the names vector 
thenames <- gsub("\\)+","", thenames)

# Remove the dashes from in the from the names vector 
thenames <- gsub("-","", thenames)

# Substitute "acceleration" for "Acc: in the names vector 
thenames <- gsub("Acc","acceleration", thenames)

# Change the names vector to all lowercase (a preference listed durign the lecture by Dr. Leek) 
tolower(thenames)

# Rename the columns in the mean and stadard deviation data set 
colnames(meanandstddata) <- thenames

# create a tidy data set containing the average of each variable for each activity and each subject 
averagesdata <- meanandstddata %>% group_by(activityname, subject) %>% summarise_all("mean")

# Extract the variable names in the averages data set 
averagesnames <- colnames(averagesdata)

# Append "average" to each of the averages data names items 
averagesnames <- paste0(averagesnames,"average")

# Remove the "average" from activity name and subject, as these are not averages 
averagesnames[1:2] <- c("activityname", "subject")

# Update the variable names in the averages data set 
colnames(averagesdata) <- averagesnames

# check if the final step output file already exists, if so remove it
finalfile <- as.character(paste(startingwd, "measurementaveragesbysubjectandactivity.txt", sep = "/"))
invisible(if(file.exists(finalfile)) {file.remove(finalfile)})

# Write the final tidy data set to a txt and also View
write.table(averagesdata, file = "measurementaveragesbysubjectandactivity.txt", row.names = FALSE)
View(averagesdata)