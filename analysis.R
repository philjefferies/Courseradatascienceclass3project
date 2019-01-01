# libraries

library(RCurl)
library(reshape2)


#source file location

log <- function(...) {
        cat("[run_analysis.R] ", ..., "\n", sep="")
}

codebook <- function(...){
        cat(..., "\n",file=targetCodebookFilePath,append=TRUE, sep="")
}

vars <- ls()

vars <- vars[which(vars!="mergedData")]

debug <- FALSE

log("DEBUGGING: ",debug)

log("workingDir: `",getwd(),"`")

if(debug && exists("mergedData")){
        rm(mergedData)
}


# data

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

zipDir <- "Human Activity Recognition Dataset"

targetZipFile <- "data.zip"

targetResultFile <- "tidydata.txt"

dataPath <- "./data"

targetZipFilePath <- file.path(dataPath,targetZipFile)

targetResultFilePath <- file.path(dataPath,targetResultFile)


#codebook

targetCodebookFilePath <- "./Codebook.md"

file.remove(targetCodebookFilePath)

codebook("# Codebook")


# create the path

if(!file.exists(dataPath)){
        log("create path: `",dataPath,"`")
        dir.create(dataPath)
}


# download the data file

if(debug){file.remove(filePath)}

if(!file.exists(targetZipFilePath)){
        
        log("downloading `",fileUrl,"`")

        binaryData <- getBinaryURL(fileUrl, ssl.verifypeer=FALSE, followlocation=TRUE)

        log("writing `",targetZipFilePath,"`")
        
        fileHandle <- file(targetZipFilePath, open="wb")
        
        writeBin(binaryData, fileHandle)

        close(fileHandle)
        
        rm(binaryData)
}else{
        log("file already exists here: `",targetZipFilePath,"`")
}


# unzip if it does not already exist

extractedZipPath <- file.path(dataPath, zipDir)

if(!file.exists(extractedZipPath) || debug){
        log("unzip: `",targetZipFilePath, "` to `",dataPath,"`")
        unzip(targetZipFilePath, exdir=dataPath, overwrite=TRUE)
}else{
        log("zip file already extracted here: `",extractedZipPath,"`")
}

dirList <- list.files(extractedZipPath, recursive=TRUE)


# load all test and train text files

sanitizedDirList <- dirList[!grepl("Inertial", dirList) & grepl("test|train", dirList)]

if(!exists("mergedData") || debug){
    
            log("load text files:")
        
        for(dataFile in sanitizedDirList){
                
                # generate parameters based on filesc
                
                paramName <- paste0("data_", tolower(sub("^.*/([^\\.]+).*$","\\1",dataFile, perl=TRUE)))
                
                txtFile <- file.path(extractedZipPath, dataFile)
                
                log("\t- `",txtFile, "` into var `", paramName,"`")
                
                tableData <- read.table(txtFile)  
                
                assign(paramName, tableData)
                
                rm(tableData)
        }
        

# Assignment task:
        
# 1. Merges the training and the test sets to create one data set.
        
log("[#1] Merges the training and the test sets to create one data set.")

# combine test & training data as rows into 3 data sets

        log("\t- combine subject test & train")

                data_subject <- rbind(data_subject_test, data_subject_train)
      
                  names(data_subject) <- c("subject")
        
                  keyColumns <- union(keyColumns, names(data_subject))
      
                  rm(data_subject_test)
        
                  rm(data_subject_train)
        
        log("\t- combine activity test & train")
        
                data_y <- rbind(data_y_test, data_y_train)
        
                names(data_y) <- c("activity_num")
        
                keyColumns <- union(keyColumns, names(data_y))
        
                rm(data_y_test)
        
                rm(data_y_train)
        
        log("\t- combine feature data test & train")
        
                data_x <- rbind(data_x_test, data_x_train)
        
                featuresFile <- file.path(extractedZipPath,"features.txt")
        
                featureData <- read.table(featuresFile)
        
                featureColumns <- featureData$V2
        
                names(data_x) <- featureColumns
        
                rm(data_x_test)
        
                rm(data_x_train)
        
                rm(featureData)
        
        
# combine the 3 data sets as colums into mergedData

        log("\t- combine subject, activity & feature data")
        
                mergedData <- cbind(data_subject, data_y)
        
                mergedData <- cbind(mergedData, data_x)
        
                rm(data_subject)
        
                rm(data_x)
        
                rm(data_y)
        
        log("\t - `mergedData` loaded in memory: ", nrow(mergedData)," x ",ncol(mergedData))
}else{
        
        log("[#1] Merges the training and the test sets to create one data set.")
        
        log("\t - `mergedData` already loaded in memory: ", nrow(mergedData)," x ",ncol(mergedData))
}


# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

log("[#2] Extracts only the measurements on the mean and standard deviation for each measurement. ")

meanStdFeatureColumns <- featureColumns[grepl("(mean|std)\\(\\)",featureColumns)]

subSetColumns <- union(keyColumns, meanStdFeatureColumns)

subSetMergedData <- mergedData[,subSetColumns]


# 3. Uses descriptive activity names to name the activities in the data set

log("[#3] Uses descriptive activity names to name the activities in the data set")

activitiesFile <- file.path(extractedZipPath,"activity_labels.txt")

activitiesData <- read.table(activitiesFile)

names(activitiesData) <- c("activity_num", "activity_name")

subSetMergedData <- merge(subSetMergedData, activitiesData, by="activity_num", all.x=TRUE)

subSetKeyColumns <- union(keyColumns, c("activity_name"))



# 4. Appropriately labels the data set with descriptive variable names. 

log("[#4] Appropriately labels the data set with descriptive variable names. ")

reshapedData <- melt(subSetMergedData, subSetKeyColumns)


# split the variable into parts and reshape into data frame and add to reshapedData

variableList <- strsplit(gsub("^((f|t)(Body|BodyBody|Gravity)(Gyro|Acc|Body)[\\-]*(Jerk)?(Mag)?[\\-]*(mean|std)[\\(\\)\\-]*(X|Y|Z)?)", "\\2|\\3|\\4|\\5|\\6|\\7|\\8|\\1", reshapedData$variable), "\\|")

        nrows <- length(variableList)
        
        ncols <- length(unlist(variableList[1]))
        
        variableUnlist <- unlist(variableList)
        
        variableMatrix <- matrix(variableUnlist, nrow=nrows, ncol=ncols, byrow=TRUE)
        
        variableData <- as.data.frame(variableMatrix)
        
        variableData$V8 <- NULL
        
        names(variableData) <- c("dimension", "source","type","jerk", "magnitude","method","axis")
        
        reshapedData <- cbind(reshapedData, variableData)
        
        rm(variableList)
        
        rm(variableUnlist)
        
        rm(variableMatrix)
        
        rm(variableData)


resultData <- reshapedData

rm(reshapedData)

log("variable `resultData` available for use : ", nrow(resultData)," x ",ncol(resultData))


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

log("[#5] Appropriately labels the data set with descriptive variable names. ")

tidyData <- dcast(resultData, activity_name + subject ~ variable, mean)

log("variable `tidyData` available for use ", nrow(tidyData)," x ",ncol(tidyData))

log("Writing `tidyData` to `",targetResultFilePath,"`")

write.table(tidyData, targetResultFilePath, row.names = FALSE, quote = FALSE,col.names = TRUE)


# write details of variables

codebook("") 

codebook("## `resultData` variable\n")

codebook("### key columns\n")

codebook("Variable name - description of var")

codebook("--------------------------------------")

codebook("`subject` participant ID")

codebook("`activity_num` activity ID")

codebook("`activity_name` name of activity")

codebook("### non-key columns\n")

codebook("Variable name - description of var")

codebook("-------------------------------------")

codebook("`variable` full name")

codebook("`value` actual value")

codebook("`dimension` dimension of measure (time or frequency)")

codebook("`source` source of measure (Body, BodyBody, or Gravity)")

codebook("`type` type of measure (accelerometer or gyro)")

codebook("`jerk` 'Jerk' signal (jerk or non-jerk)")

codebook("`magnitude` magnitude value (mag or non-mag)")

codebook("`method` method result (mean or sd)")

codebook("`axis` no axis or X/Y/Z")

codebook("") 

codebook("## `tidyData` variable\n")

codebook("### key columns\n")

codebook("Variable name - description of var")

codebook("------------------------------------")

codebook("`activity_name` name of activity")

codebook("`subject` participant ID")


codebook("### non-key columns\n")

codebook("Variable name - description of var")

codebook("------------------------------------")

tidyDataCols <- names(tidyData)[3:68]

for(tdc in tidyDataCols){
        codebook("`",tdc,"`   average value")
}