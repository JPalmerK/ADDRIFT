#Script to fix  noise summaries downloaded/processed in PST rather than UTM
rm(list=ls())
library(lubridate)
library(readr)
library(data.table)


# Set the directory where your CSV files are located

driftDir <- "Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics\\ADRIFT_012"
driftDir <- "Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics\\ADRIFT_001_pst"


outputDir <- "Z:\\ANALYSIS\\ADRIFT\\Soundscape\\metrics\\ADRIFT_001"

#List the csv files
driftFiles<-list.files(driftDir, pattern = '*.csv',full.names = TRUE)

# Load the first drift, convert to UTC and save
drift= read.csv(driftFiles[1],header = TRUE)
times= as.POSIXct(drift$yyyy.mm.ddTHH.MM.SSZ, tz = "US/Pacific", 
                  format ="%Y-%m-%dT%H:%M:%OSZ")
timesUTC= times
timesUTC= with_tz(times, tzone = "UTC")  
timesOut = as.character(timesUTC, format ="%Y-%m-%dT%H:%M:%OS3Z")

for(ii in 1:length(driftFiles)){
  
  input_file= driftFiles[ii]
  output_file = file.path(outputDir, basename(input_file))
  header <- read_lines(input_file, n_max = 1)
  write_lines(header, output_file)
  
  
  for(jj in 2:length(timesOut)){
    
    # Read the current row
    line <- read_lines(input_file, skip = jj - 1, n_max = 1)
    
    # Split the line by commas
    values <- unlist(strsplit(line, ",", fixed = TRUE))
    
    # Modify the first element (assuming the first element is the first column)
    values[1] <-timesOut[jj-1]
    
    # Combine the modified values back into a line
    modified_line <- paste(values, collapse = ",")
    
    
    
    # Write the modified line to the output file
    write_lines(modified_line, output_file, append = TRUE)
    print(jj)
  }
  # 
  # dt <- fread(driftFiles[ii], header = TRUE, colClasses=c("character"))
  # dt[1:length(timesOut),colnames(dt)[1] := timesOut]
  # fwrite(dt, driftFiles[ii], col.names = TRUE)
  # 
  # 
  print(ii)
  
}
