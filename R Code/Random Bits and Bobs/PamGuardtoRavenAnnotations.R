rm(list = ls())
library(lubridate)
library(stringr)
library(tuneR)
library(RSQLite)

#Define path to functions that we will use
source('C:\\Users\\kaitlin.palmer\\Documents\\GitHub\\YAWN_functions\\NoiseProcessingFxs.R')

# Scratch directory to set up noise analysis metrics following merchant 2012 (I think)


################################################################
# Load list of sound files and start times, assumes all audio files are the 
# same duration
################################################################

# Assumes 
# 1) file names in UTC
# 2) All sample rates are the same
fileLoc ='D:\\RECORDINGS\\12kHz\\DRIFT_10_12kHz'


# Soundtrap Pattern
nameStringPattern = '\\d{12}'
nameStringFormat ='%y%m%d%H%M%OS'
lubradiateFormat ="ymdHMS"

# Set up the audiodataframe and iniitalize parameters
dataOut = createAudioDataframe(fileLoc,nameStringPattern = nameStringPattern,
                               lubradiateFormat = lubradiateFormat)
audioData= dataOut$audioData
audioData$StartSec =  cumsum(audioData$Duration)-audioData$Duration[1]

# Load the databse
con <- dbConnect(SQLite(), 
                 'C:/Users/kaitlin.palmer/Desktop/DATA/CompletedDatabases/CCES/GPL_HB_Gray_PG2_02_02_CCES2018_Drift10_completemissingvalues.sqlite3')

annotations <- dbReadTable(con, 'Spectrogram_Annotation')

# Convert annotation times to datetimes
local_time <- ymd_hms(annotations$UTC, tz='UTC')

# Figure out how many seconds offset each annotation is. 

# Figure out which file the data starts in
filestartIdx = max(which(audioData$StartTime<local_time[36]))
timeDiff=local_time[36]-audioData$StartTime[filestartIdx]


filestartIdx <- sapply(local_time, function(time) {
  max(which(audioData$StartTime < time))
})

timeDiff=local_time-audioData$StartTime[filestartIdx]


annotations$'Begin Time (s)'=audioData$StartSec[filestartIdx]+
  as.numeric(timeDiff)
annotations$'End Time (s)'=annotations$'Begin Time (s)'+annotations$Duration

# Select the Data to to be included
extraCols = c('Species', 'Note')
RavenTable=annotations[,c('Begin Time (s)', 'End Time (s)', 'f1', 'f2', extraCols )]

colnames(RavenTable)[c(3,4)]<- c('Low Freq (Hz)', 'High Freq (Hz)')
RavenTable$Channel =1
RavenTable$Selection = 1:nrow(RavenTable)
RavenTable$View=1

RavenTable$`Low Freq (Hz)`[RavenTable$`Low Freq (Hz)`<1]=1



# Modify column names by removing backticks and quotation marks
col_names <-(names(RavenTable))
col_names <- gsub("\"", "", col_names)


# Export the dataframe as a tab-delimited file without quotation marks in column names
write.table(RavenTable, file = file_path, sep = "\t", row.names = FALSE, col.names = col_names, quote = FALSE)



# Set up the file names
file_path=(paste0('C:/Users/kaitlin.palmer/Desktop/DATA/SelectonTables/', 'TestRavenTable.txt'))

# Export the dataframe as a tab-delimited file without quotation marks in column names
write.table(RavenTable, file = file_path, 
            sep = "\t", row.names = FALSE, col.names = col_names)