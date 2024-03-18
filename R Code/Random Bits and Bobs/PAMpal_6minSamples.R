# PAMpal simple example 
# Its on CRAN. Yay!
# install.packages('PAMpal')
# Sometimes I fix things and theyre only available on the GitHub version
# Right now there are some things that run a lot faster on teh GitHub version
# so I recommend installing that.
# updated 22-12-6 to include loop for PG event adding w/PAMmisc

#devtools::install_github('TaikiSan21/PAMpal')

library(PAMpal)
library(lubridate)
library(PAMmisc)
library(stringr)
source('C:/Users/kaitlin.palmer/Documents/GitHub/SWFSC Processing/timeEventFunctions.R')


# Start by creating a "PAMpalSettings" object. This keeps track of what data
# you want to process and what processing you want to apply to it.

# Change paths below to your DB and binary folder. Can just be the 
# highest level binary folder for that drift - it will add all files
# within that folder recursively through subfolders.

# This will also ask you to type in some parameters for calculations
# in your console. You can just hit ENTER to accept defaults for all
# of these, they aren't relevant to the GPL calculations only for clicks.

dbLoc = 'D:/DATA/2022-12-05_GPL_HB_Gray_Run_CorrectTimes/DATABASES/GPL_HB_Gray_PG2_02_02_ADRIFT_012.sqlite3'
binLoc = 'D:/DATA/2022-12-05_GPL_HB_Gray_Run_CorrectTimes/BINARY/ADRIFT_012/'
recLoc = 'D:/RECORDINGS/ADRIFT_012_CENSOR_12kHz/'


dbLoc = 'E:/DATA/GPL_HB_GRAY/DATABASES/GPL_HB_Gray_PG2_02_02_CCES2018_Drift14_rerun.sqlite3'
binLoc = 'E:/DATA/GPL_HB_GRAY/BINARY/Drift_014/'
recLoc = 'E:/RECORDINGS/12kHz/DRIFT_14_12kHz/'

dbLoc = 'E:/DATA/GPL_HB_GRAY/DATABASES/GPL_HB_Gray_PG2_02_02_CCES2018_Drift16.sqlite3'
binLoc = 'E:/DATA/GPL_HB_GRAY/BINARY/Drift_016/'
recLoc = 'E:/RECORDINGS/12kHz/DRIFT_16_12kHz/'


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
randStart =sample(1:5,1)
wavDf=wavDf[round(seq(randStart, nrow(wavDf), length.out = nfiles)),]


# If we care about assigning some kind of initial label to these
# detections. Otherwise ignore.
data <- setSpecies(data, method='manual', value='InitialGPL')
# Add events from wavDf loop

for(e in 1:nrow(wavDf)) {
  thisEv <- data[[basename(wavDf$file[e])]]
  # this will get all detector types, if just one type is wanted can
  # be simplified to ex. uids <- unique(getGPLData(thisEv)$UID)
  uids <- unique(unlist(lapply(getDetectorData(thisEv), function(x) {
    if(is.null(x)) return(NULL)
    x$UID
  })))
  addPgEvent(db = files(thisEv)$db,
             binary = files(thisEv)$binaries,
             eventType = species(thisEv)$id,
             UIDs = uids,
             type = 'dg',
             start = wavDf$start[e],
             end = wavDf$end[e],
             comment = paste0('Added by PAMpal, event ID: ', id(thisEv)))
}



# 
# # create df with start and end times of all files
# files <- (list.files(recLoc[1], pattern = "\\.wav$"))
# 
# audioData =data.frame(files = file.path(recLoc, files))
# audioData$FileName = files
# audioData$TimeString=lapply(audioData$FileName, 
#                             str_extract, 
#                             pattern =nameStringPattern)
# audioData$StartTime =parse_date_time(audioData$TimeString,
#                                      lubradiateFormat,
#                                      tz='UTC')
# 
# # For subsampling along two six minute periods
# # Create the time dataframe
# 
# maxDur = minutes(22)+seconds(30)
# dataStart = min(audioData$StartTime)
# dataStop = max(audioData$StartTime)+maxDur
# 
# timeDf = data.frame(
#   start = seq(dataStart,dataStop, by = '6 min'))
# timeDf$End = timeDf$start+minutes(6)
# timeDf$id=1:nrow(timeDf)
# 
# # Create the events
# timeGroupedData <- processPgDetections(pps, mode='time', grouping=timeDf)
# ancillary(timeGroupedData)$grouping
# 
#   
# # If we care about assigning some kind of initial label to these
# # detections. Otherwise ignore. 
# timeGroupedData <- setSpecies(timeGroupedData, method='manual', value='InitialGPL')
# 
# 
# # 1/5th of the events
# 
# # get the event IDs 
# keepEvents = as.character(seq(2, length(events(timeGroupedData)), by =5))
# 
# 
# # Add events from wavDf loop
# for(e in keepEvents) {
#     thisEv <- timeGroupedData[e]
#     #thisEv <- data[[basename(wavDf$file[e])]]
#     # this will get all detector types, if just one type is wanted can
#     # be simplified to ex. uids <- unique(getGPLData(thisEv)$UID)
#     uids <- unique(unlist(lapply(getDetectorData(thisEv), function(x) {
#         if(is.null(x)) return(NULL)
#         x$UID
#     })))
#     
#     addPgEvent(db = files(thisEv)$db,
#                binary = files(thisEv)$binaries,
#                eventType = species(thisEv)$id,
#                UIDs = uids,
#                type = 'dg',
#                start = wavDf$start[e],
#                end = wavDf$end[e],
#                comment = paste0('Added by PAMpal, event ID: ', id(thisEv)))
# }