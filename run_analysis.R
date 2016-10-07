## Program:	run_analysis.R
## Author:	Alan Carregal
## Description: 

# load libraries
library(dplyr)
library(reshape2)

# remove variables from R environment (Debugging purposes)
# rm(list = setdiff(ls(), lsf.str()))

# Define input/output files
URL <- c("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ");
ZIP <- c("UCI_HAR_Dataset.zip");
OUT <- c("tidy.txt");

# Get file names needed for the data manipulation
#
getFileNames <- function(ZipFile)
{
	if (file.exists(ZipFile))
	{
		tryCatch({ 
			zippedFiles <- unzip(ZipFile,list=TRUE); # Get filenames in zip file
			zipFileNames<-zippedFiles[,"Name"];

			zipFilesCnt<-length(unlist(zipFileNames));
			zipExistCnt<-sum(unlist(lapply(zipFileNames,file.exists)));

			if(zipFilesCnt == zipExistCnt) # if all zip files exist then continue
			{
				cFileNames<-function(mText) # add filenames based on search to vector
				{
					x<-NULL;
					for(txt in mText)
					{
						if (!is.na(txt)) 
						{ 
							x<-c(x,zipFileNames[grep(txt,zipFileNames)]);
						} else
						{
							x<-c(x,NA);
						}
					}
					return(x);	
				}
				
				# files to be used for manipulation
				activityLabels<-cFileNames(c("/activity_labels",NA,NA));
				features<-cFileNames(c("/features.txt",NA,NA));
				train<-cFileNames(c("/X_train","/y_train","subject_train"));
				test<-cFileNames(c("/X_test","/y_test","subject_test"));

				return(data.frame(activityLabels,features,train,test,stringsAsFactors=FALSE))
			}
			}, warning=function(w)
			{
				cat(paste("Data Collection Warning:\n",w));
				stop("");
			}, error=function(e)
			{
				cat(paste("Data Collection Error:\n",e));
				stop("Possible ZIP File corruption");
			})
	}
}

# Get data from its source and expand zip file
#
getSourceData <- function(dlFileName,fileURL,overwrite=FALSE,verbose=FALSE)
{
	if (!file.exists(dlFileName) | overwrite==TRUE) download.file(fileURL, dest=dlFileName, method="curl",quiet=!verbose)  
	if (!file.exists("UCI HAR Dataset") | overwrite==TRUE) unzip(dlFileName) 
}

# Extract name column from Labels text file.
#
loadLabels <- function(xfilename)
{
	if (file.exists(xfilename))
	{
		lbls <- read.table(xfilename)
		lbls[,2] <- as.character(lbls[,2])
	}
	return(lbls);
}

# read data from three files and combine with only the features needed.
#
loadData <- function(xfileSet,neededFeatures)
{
	x1 <- read.table(as.character(xfileSet[1,1]))[neededFeatures]
	x2 <- read.table(as.character(xfileSet[2,1]))
	x3 <- read.table(as.character(xfileSet[3,1]))
	x1 <- cbind(x3, x2, x1)	
}

# Begin actual execution
#
getSourceData(ZIP,URL,overwrite=FALSE,verbose=TRUE);
file <- getFileNames(ZIP);
activityLabel <- loadLabels(file[1,"activityLabels"])
features <- loadLabels(file[1,"features"])

# create needed features based on what is asked
#
nFeatures <- grep(".*mean.*|.*std.*", features[,2]) # needed Features
nFnames <- features[nFeatures,2]
nFnames <- gsub("-mean","Mean",nFnames)
nFnames <- gsub("-std","Std",nFnames)
nFnames <- gsub('[()-]',"",nFnames)  # needed Features Names

# combine train and test data using only the needed features
mergedData <-rbind(loadData(file["train"],nFeatures),loadData(file["test"], nFeatures))

# add column names to merged data (nFrames is not all lowercased, but are multiple words for
# description)
colnames(mergedData)<- c("subject","activity", nFnames)

#convert actvity & subject to factors
mergedData$activity<-factor(mergedData$activity,levels=activityLabel[,1],labels=activityLabel[,2])
mergedData$subject<-as.factor(mergedData$subject)

# melt the data using subject and activity
meltedData<-melt(mergedData, id=c("subject","activity"))

# create dataset of mean on each variable
meanData<-dcast(meltedData, subject + activity ~ variable, mean)

# create file with mean data
write.table(meanData,OUT,row.names=FALSE,quote=FALSE)

