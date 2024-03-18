# PAMpal simple example 
# Its on CRAN. Yay!
# install.packages('PAMpal')
# Sometimes I fix things and theyre only available on the GitHub version
# Right now there are some things that run a lot faster on teh GitHub version
# so I recommend installing that.
# updated 22-12-6 to include loop for PG event adding w/PAMmisc

rm(list=ls())
#devtools::install_github('TaikiSan21/PAMpal')

library(PAMpal)
library(lubridate)
library(PAMmisc)
library(stringr)
source('timeEventFunctions.R')


# Start by creating a "PAMpalSettings" object. This keeps track of what data
# you want to process and what processing you want to apply to it.

# Change paths below to your DB and binary folder. Can just be the 
# highest level binary folder for that drift - it will add all files
# within that folder recursively through subfolders.

# This will also ask you to type in some parameters for calculations
# in your console. You can just hit ENTER to accept defaults for all
# of these, they aren't relevant to the GPL calculations only for clicks.

dbLoc = 'E:/DATA/GPL_Blue_wHALE/DATABASES/GPL_BlueAB_PG2_02_02_CCES2018_Drift7.sqlite3'
binLoc = 'E:/DATA/GPL_Blue_wHALE/BINARY/Drift_7/'
recLoc = 'E:/RECORDINGS/500Hz/DRIFT_7_500Hz/'


pps <- PAMpalSettings(db = dbLoc,
                      binaries = binLoc,
                      # these parameters are only for the click detector - can ignroe
                      sr_hz='auto',
                      filterfrom_khz=0,
                      filterto_khz=NULL,
                      winLen_sec=.0025)

# Now tell it to process your data. Id is optional and serves no function,
# but can be useful to tell data apart at a later point in time. Here
# mode = 'recording' tells it how to organize your data. Most of the time
# we are working with data that have been marked manually into events, 
# so PAMpal wants to organize things into events. mode='db' uses the events
# in the database, and only processes the detectoins you've marked out.
# In this case we just want to process everything, which is what 
# mode='recording' does. It will group them into events by recording file. 


# Soundtrap Pattern
nameStringPattern = '\\d{12}'
nameStringFormat ='%y%m%d%H%M%OS'
lubradiateFormat ="ymdHMS"


data <- processPgDetections(pps, mode='recording', id='Humpback007')
# And here's how you can get the detections information out of "data"
# as a dataframe. Time column is "UTC", other columns are stuff it
# measured.
# Now we can add the wav files to this data. You might get a warning about
# "startSample", its safe to ignore that.
data <- addRecordings(data, folder= recLoc)

gplDf <- getGPLData(data)

# that data is stored here as a dataframe. Has "start" & "end" as POSIXct and
# the fulle path to the file as "file"
wavDf <- files(data)$recordings
# add number of detections to this
nDets <- sapply(events(data), nDetections)
nDets <- data.frame(join=names(nDets), nDets=nDets)
wavDf$join <- basename(wavDf$file)
wavDf <- left_join(wavDf, nDets)
wavDf$join <- NULL
wavDf$nDets[is.na(wavDf$nDets)] <- 0
nfiles =round(nrow(wavDf)*.2)



# Load the full log of 500hz files
loggedFiles = read.csv('C:/Users/kaitlin.palmer/Downloads/CCES_2018_Drift_07_BlueWhale_Log.csv')
loggedFiles$Call = as.factor(loggedFiles$Call)
loggedFiles$fname = basename(loggedFiles$Input.file)

# Get all file names
allAudio = list.files(path = 'E:/RECORDINGS/500Hz/DRIFT_7_500Hz/',pattern = '.wav')

# Files that do not have blue whale A calls
loggedFilesKeep = loggedFiles[loggedFiles$Call != "A NE Pacific",]
keepAudio = allAudio[!(allAudio %in% loggedFiles$fname)]

lessData <- data[keepAudio]

###########
writeEventClips(lessData, buffer = c(8,8), 
                outDir = 'E:/BlueClips/NegExamples/',mode = 'detection')

clipNames = list.files(path = 'E:/BlueClips/NegExamples/',pattern = '.wav')

aa = parseEventClipName(clipNames)
#############################################
# Modify the raven log for consistant clip sizes
# Load file list and annotation log
Ravenlog = read.table('E:/BluewhaleDetector/SelectionTableAcalls/CCES_2018_Drift_07_BlueACalls.txt',
                      sep="\t", header=TRUE, check.names = FALSE)
centerTime = (Ravenlog$`End Time (s)`+Ravenlog$`Begin Time (s)`)/2

# Create 10 second clips

fs =500
startSec=c()

for(ii in 1:nrow(Ravenlog)){
  startSec =c(startSec, centerTime[ii]-5)
  
  if(Ravenlog$`Beg File Samp (samples)`[ii]-(5*fs)>0){
    startSec = c(startSec, Ravenlog$`Begin Time (s)`[ii]-5)
  }
  
  if(Ravenlog$`End File Samp (samples)`[ii]+(fs*5)< fs*120){
    startSec = c(startSec, Ravenlog$`End Time (s)`[ii]-5)
  }
  
}
endSec = startSec+10

selectionTableOut = data.frame(Selection = 1:length(endSec),
                               View= 'Spectrogram 1',
                               Channel = 1,
                               beginTime = startSec,
                               endTime = endSec,
                               lowf= 40, 
                               highf=120)
colnames(selectionTableOut)<-colnames(Ravenlog)[1:7]

write.table(x = selectionTableOut, 
            file = 'E:/BluewhaleDetector/SelectionTableAcalls/NNSamples.txt',
              sep="\t", quote = FALSE,row.names = FALSE)


