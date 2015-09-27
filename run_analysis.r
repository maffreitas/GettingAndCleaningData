# Preparing the work directory
WD <- getwd()
dirGCT <- "../GettingAndCleaningData"
if ( WD != dirGCT){
setwd(dirGCT)
} 
getwd()

# Preparing, downloading and unziping the necessary data
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if ( !is.installed('httr') ) {
	install.packages('httr')
}
library(httr)

if ( !is.installed('reshape2') ) {
	install.packages('reshape2')
}
library(reshape2)

if ( !is.installed('RCurl') ) {
	install.packages('RCurl')
}
library(RCurl)

if ( !is.installed('data.table') ) {
	install.packages('data.table')
}
library('data.table')


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "Dataset.zip"
download.file(url, file)

dirdata <- "UCI HAR Dataset"
unzip(file, list = FALSE, overwrite = TRUE)

gettables <- function (file,column = NULL){
	f <- paste(dirdata,file,sep="/")
	data <- data.frame()
	if(is.null(column)){
		data <- read.table(f,sep="",stringsAsFactors=F)
	} else {
		data <- read.table(f,sep="",stringsAsFactors=F, col.names= column)
	}
	data
}

features <- gettables("features.txt")

getdata <- function(type, features){
	subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
	y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
	x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
	return (cbind(subject_data,y_data,x_data))
}

test <- getdata("test", features)
train <- getdata("train", features)

saveresults <- function (data,name){
	file <- paste(dirdata, "/", name,".txt" ,sep="")
	write.table(data,file, row.name=FALSE)
}

#You should create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
library(plyr)
step_1 <- rbind(train, test)
step_1 <- arrange(step_1, id)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
step_2 <- step_1[,c(1,2,grep("std", colnames(step_1)), grep("mean", colnames(step_1)))]
saveresults(step_2,"MeanSTD")

#3. Uses descriptive activity names to name the activities in the data set
step_3 <- gettables("activity_labels.txt")

#4. Appropriately labels the data set with descriptive variable names. 
step_1$activity <- factor(step_1$activity, levels=step_3$V1, labels=step_3$V2)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
step_5 <- ddply(step_2, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(step_5)[-c(1:2)] <- paste(colnames(step_5)[-c(1:2)], "_mean", sep="")
saveresults(step_5,"Activity")