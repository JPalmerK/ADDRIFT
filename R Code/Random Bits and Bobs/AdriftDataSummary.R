# Create metadata for upload

rm(list=ls())
library(lubridate)


##############################################################
# Adrift Setup
##############################################################

# Set the directory where your CSV files are located
driftDir <- "Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics"

# List of all Adrift folders
driftDir<-list.dirs(driftDir)
driftDir<-driftDir[-1]



# Pre-allocate the dataframe that we will writ to the csv
csvOut = data.frame(driftID = rep('driftId', length(driftDir)),
                    dataStart = rep(Sys.time(), length(driftDir)),
                    dataEnd = rep(Sys.time(), length(driftDir)),
                    LowBand = rep(NaN, length(driftDir)),
                    HighBand = rep(NaN, length(driftDir)),
                    calib = rep(NaN, length(driftDir)))


for(ii in 44:length(driftDir))
  for(ii in 1:43){
  # Get the drift name
  currentDriftDir = driftDir[ii]
  driftID<- basename(driftDir[ii])
  
  # # Adrift specific #
  ## Flat if the data are in PST - use later
  isPST = grep('pst', driftID)
  
  # Cut the folder name if PST has been added
  if(length(isPST) > 0) {driftID = substr(driftID, 1, 10)}
  
  # ## CCES Specific
  # library(stringr)
  # driftNumber <- str_extract(driftID, "(?<=_)\\w+(?=_)")
  # driftID<- paste0('CCES_', driftNumber)
  
  
  csvOut$driftID[ii]<-driftID
  
  # ### Adrift specific
  # # Load up an example file that will have the frequency bounds and time bounds
  # 
  # if(ii %in% c(2)){
  #   
  #   #cces files are all over the place in terms of naming
  #   fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_1s_1Hz_PSD_2min.csv'))
  #   fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_1s_1Hz_TOL_2min.csv'))  
  # }else if(ii %in% c(1,3,4,5,6,7,8,9,10,11)){
  #   
  #   #cces files are all over the place in terms of naming
  #   fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_1Hz_1s_PSD_2min.csv'))
  #   fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_1Hz_1s_TOL_2min.csv'))
  #   print('second')
  # }else if(ii ==12){
  #   #cces files are all over the place in terms of naming
  #   fnamePSD<- file.path(currentDriftDir, 'CCES_07_1Hz_1s_PSD_2min.csv')
  #   fnameTrdOct<- file.path(currentDriftDir, 'CCES_07_1Hz_1s_TOL_2min.csv')
  # }else if(ii == 13){
  #   fnamePSD<- file.path(currentDriftDir, 'CCES_08_1Hz_1s_PSD_2min.csv')
  #   fnameTrdOct<- file.path(currentDriftDir, 'CCES_08_1Hz_1s_TOL_2min.csv')
  # }else{
  #   # fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_PSD_2min.csv'))
  #   # fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_TOL_2min.csv'))
  #   print('third')}
  # 

  fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_PSD_2min.csv'))
  fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_TOL_2min.csv'))
  
  
  
  # We only need the first and last column and the first and last row but those 
  # always change so we need to figure out the dimensions beforehand
  
  freqRange = read.csv(fnamePSD, head = TRUE, nrows=1)
  thirdOct= read.csv(fnameTrdOct)
  
  # Frequency range
  freqNames = colnames(freqRange)[c(2,dim(freqRange)[2])]
  freqRange = as.numeric(gsub("PSD_", "", freqNames))
  csvOut$LowBand[ii] = freqRange[1]
  csvOut$HighBand[ii] = freqRange[2]
  
  
  # Get the time range
  tzval = ifelse(length(isPST)>1, "America/Los_Angeles", 'utc')
  Tvals = as.POSIXct(thirdOct[c(1,dim(thirdOct)[1]),1], 
                     format ="%Y-%m-%dT%H:%M:%OSZ", tz= tzval)
  csvOut$dataStart[ii] = Tvals[1]
  csvOut$dataEnd[ii] = Tvals[2]
  
  # read in the text file and pull out the offset 
  logfile =  file.path(currentDriftDir, 'logfile.txt')
  text<-readLines(logfile, warn = FALSE)
  
  # Define the regular expression pattern to match the phrase
  pattern <- "Single value full system calibration: ([0-9.]+) dB"
  
  # Find the match in the text
  matches <- grep(pattern, text, value = TRUE)
  
  # Extract the numerical value
  if (length(matches) > 0) {
    match_result <- regmatches(matches, regexec(pattern, matches))[[1]]
    extracted_value <- as.numeric(match_result[2])
    print(extracted_value)
  } else {
    print("Pattern not found in the text.")
  }
  
  csvOut$calib[ii]<-extracted_value
  print(ii)
}


write.csv(csvOut, 'ADRIFT Summary.csv')

###############################################################
# CCES setup Sure name them whatever you want and change it every time
##############################################################

driftDir = 'Z:\\ANALYSIS\\CCES_2018\\Soundscapes\\METRICS'
driftDir<-list.dirs(driftDir, recursive = FALSE)
driftDir<-driftDir[c(-1,-6)]


# Pre-allocate the dataframe that we will writ to the csv
csvOut = data.frame(driftID = rep('driftId', length(driftDir)),
                    dataStart = rep(Sys.time(), length(driftDir)),
                    dataEnd = rep(Sys.time(), length(driftDir)),
                    LowBand = rep(NaN, length(driftDir)),
                    HighBand = rep(NaN, length(driftDir)),
                    calib = rep(NaN, length(driftDir)))




for(ii in 1:length(driftDir)){
  # Get the drift name
  currentDriftDir = driftDir[ii]
  driftID<- basename(driftDir[ii])
  
  # # Adrift specific #
  ## Flat if the data are in PST - use later
  isPST = grep('pst', driftID)

  # # Cut the folder name if PST has been added
  # if(length(isPST) > 0) {driftID = substr(driftID, 1, 10)}
  
  ## CCES Specific
  library(stringr)
  driftNumber <- str_extract(driftID, "(?<=_)\\w+(?=_)")
  driftID<- paste0('CCES_', driftNumber)

  
  csvOut$driftID[ii]<-driftID
  
  ### Adrift specific
  # Load up an example file that will have the frequency bounds and time bounds
  
  if(ii %in% c(2)){
    
    #cces files are all over the place in terms of naming
    fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_1s_1Hz_PSD_2min.csv'))
    fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_1s_1Hz_TOL_2min.csv'))  
  }else if(ii %in% c(1,3,4,5,6,7,8,9,10,11)){
    
    #cces files are all over the place in terms of naming
    fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_1Hz_1s_PSD_2min.csv'))
    fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_1Hz_1s_TOL_2min.csv'))
    print('second')
  }else if(ii ==12){
    #cces files are all over the place in terms of naming
    fnamePSD<- file.path(currentDriftDir, 'CCES_07_1Hz_1s_PSD_2min.csv')
    fnameTrdOct<- file.path(currentDriftDir, 'CCES_07_1Hz_1s_TOL_2min.csv')
  }else if(ii == 13){
    fnamePSD<- file.path(currentDriftDir, 'CCES_08_1Hz_1s_PSD_2min.csv')
    fnameTrdOct<- file.path(currentDriftDir, 'CCES_08_1Hz_1s_TOL_2min.csv')
  }else{
  fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_PSD_2min.csv'))
  fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_TOL_2min.csv'))
  print('third')}
  
  # We only need the first and last column and the first and last row but those 
  # always change so we need to figure out the dimensions beforehand
  
  freqRange = read.csv(fnamePSD, head = TRUE, nrows=1)
  thirdOct= read.csv(fnameTrdOct)
  
  # Frequency range
  freqNames = colnames(freqRange)[c(2,dim(freqRange)[2])]
  freqRange = as.numeric(gsub("PSD_", "", freqNames))
  csvOut$LowBand[ii] = freqRange[1]
  csvOut$HighBand[ii] = freqRange[2]
  
  
  # Get the time range
  tzval = ifelse(length(isPST)>1, "America/Los_Angeles", 'utc')
  Tvals = as.POSIXct(thirdOct[c(1,dim(thirdOct)[1]),1], 
                     format ="%Y-%m-%dT%H:%M:%OSZ", tz= tzval)
  csvOut$dataStart[ii] = Tvals[1]
  csvOut$dataEnd[ii] = Tvals[2]
  
  # read in the text file and pull out the offset 
  logfile =  file.path(currentDriftDir, 'logfile.txt')
  text<-readLines(logfile, warn = FALSE)
  
  # Define the regular expression pattern to match the phrase
  pattern <- "Single value full system calibration: ([0-9.]+) dB"
  
  # Find the match in the text
  matches <- grep(pattern, text, value = TRUE)
  
  # Extract the numerical value
  if (length(matches) > 0) {
    match_result <- regmatches(matches, regexec(pattern, matches))[[1]]
    extracted_value <- as.numeric(match_result[2])
    print(extracted_value)
  } else {
    print("Pattern not found in the text.")
  }
  
  csvOut$calib[ii]<-extracted_value
  print(ii)
}


write.csv(csvOut, 'CCES Summary.csv')


##########################################################################
# Pascal
##########################################################################


###############################################################
# CCES setup Sure name them whatever you want and change it every time
##############################################################

driftDir = 'Z:\\ANALYSIS\\PASCAL_2016\\Soundscape\\metrics'
driftDir<-list.dirs(driftDir, recursive = FALSE)



# Pre-allocate the dataframe that we will writ to the csv
csvOut = data.frame(driftID = rep('driftId', length(driftDir)),
                    dataStart = rep(Sys.time(), length(driftDir)),
                    dataEnd = rep(Sys.time(), length(driftDir)),
                    LowBand = rep(NaN, length(driftDir)),
                    HighBand = rep(NaN, length(driftDir)),
                    calib = rep(NaN, length(driftDir)))




for(ii in 3:length(driftDir)){
  # Get the drift name
  currentDriftDir = driftDir[ii]
  driftID<- basename(driftDir[ii])
  
  # # Adrift specific #
  ## Flat if the data are in PST - use later
  isPST = grep('pst', driftID)
  
  # # Cut the folder name if PST has been added
  # if(length(isPST) > 0) {driftID = substr(driftID, 1, 10)}
  
  csvOut$driftID[ii]<-driftID
  
  ### Adrift specific
  # Load up an example file that will have the frequency bounds and time bounds
  
  if(ii %in% c(1,2,13,14,16,17)){
    fnamePSD<- file.path(currentDriftDir, 'PASCAL__PSD_2min.csv')
    fnameTrdOct<- file.path(currentDriftDir, 'PASCAL__TOL_2min.csv')
  }else{
    fnamePSD<- file.path(currentDriftDir, paste0(driftID,'_PSD_2min.csv'))
    fnameTrdOct<- file.path(currentDriftDir, paste0(driftID,'_TOL_2min.csv'))  
  }
  
  
  
  # We only need the first and last column and the first and last row but those 
  # always change so we need to figure out the dimensions beforehand
  
  freqRange = read.csv(fnamePSD, head = TRUE, nrows=1)
  thirdOct= read.csv(fnameTrdOct)
  
  # Frequency range
  freqNames = colnames(freqRange)[c(2,dim(freqRange)[2])]
  freqRange = as.numeric(gsub("PSD_", "", freqNames))
  csvOut$LowBand[ii] = freqRange[1]
  csvOut$HighBand[ii] = freqRange[2]
  
  
  # Get the time range
  tzval = ifelse(length(isPST)>1, "America/Los_Angeles", 'utc')
  Tvals = as.POSIXct(thirdOct[c(1,dim(thirdOct)[1]),1], 
                     format ="%Y-%m-%dT%H:%M:%OSZ", tz= tzval)
  csvOut$dataStart[ii] = Tvals[1]
  csvOut$dataEnd[ii] = Tvals[2]
  
  # read in the text file and pull out the offset 
  logfile =  file.path(currentDriftDir, 'logfile.txt')
  text<-readLines(logfile, warn = FALSE)
  
  # Define the regular expression pattern to match the phrase
  pattern <- "Single value full system calibration: ([0-9.]+) dB"
  
  # Find the match in the text
  matches <- grep(pattern, text, value = TRUE)
  
  # Extract the numerical value
  if (length(matches) > 0) {
    match_result <- regmatches(matches, regexec(pattern, matches))[[1]]
    extracted_value <- as.numeric(match_result[2])
    print(extracted_value)
  } else {
    print("Pattern not found in the text.")
  }
  
  csvOut$calib[ii]<-extracted_value
  print(ii)
}


write.csv(csvOut, 'CCES Summary.csv')
