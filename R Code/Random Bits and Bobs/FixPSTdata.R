#Script to fix  noise summaries downloaded/processed in PST rather than UTM
rm(list=ls())
library(lubridate)


# Set the directory where your CSV files are located

driftDir <- "Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics\\ADRIFT_012_pst"
driftDir <- "Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics\\ADRIFT_001_pst"

outDir ='Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics\\ADRIFT_001'

#List the csv files
driftFiles<-list.files(driftDir, pattern = '*.csv',full.names = TRUE)


for(ii in 1:length(driftFiles)){
  drift= read.csv(driftFiles[ii],header = TRUE)
  times= as.POSIXct(drift$yyyy.mm.ddTHH.MM.SSZ, tz = "US/Pacific", 
                    format ="%Y-%m-%dT%H:%M:%OSZ")
  timesUTC= times
  timesUTC= with_tz(times, tzone = "UTC")  
  timesOut = as.character(timesUTC, format ="%Y-%m-%dT%H:%M:%OS3Z")
  
  drift$yyyy.mm.ddTHH.MM.SSZ= timesOut
  
  write.csv(file.path(outDir, basename(driftFiles[ii])))
  
}
