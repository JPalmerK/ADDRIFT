rm(list=ls())


library(lubridate)
library(stringr)
library(dplyr)


# Set the directory where your CSV files are located
csv_directory <- "C:\\Data\\ADRIFT Blue Whale Logs"

# Get a list of CSV files in the directory (adjust the pattern if needed)
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)

# Use lapply to read each CSV file into a list of dataframes
list_of_dataframes <- lapply(csv_files, read.csv)

# Combine the list of dataframes into a single dataframe
combined_dataframe <- do.call(rbind, list_of_dataframes)


# Assuming df is your DataFrame with an 'inputfile' column
combined_dataframe$Adrift_id <- str_extract(combined_dataframe$Input.file, "ADRIFT_\\d{3}")

# Convert to datetime
combined_dataframe <- combined_dataframe %>%
  mutate(Start.timeNew = gsub("\\s\\d{2}:\\d{2}(:\\d{2})?$", "", Start.time),
         Start.timeNew = as.POSIXct(Start.time, format = "%m/%d/%Y %H:%M"))

combined_dataframe$Date_and_Hour=  format(combined_dataframe$Start.timeNew,
                                          "%Y-%m-%d %H")


## Process Meta data

# Read in the meta
meta= read.csv('C:\\Data/DepDetailsMod.csv')

meta= meta[meta$Project== 'ADRIFT', c("Project", "DeploymentID", "Site",
                                      "Instrument_ID", "Deployment_Date", 
                                      "Deployment_Latitude", 
                                      "Deployment_Longitude", "Data_Start", "Data_End",
                                      "Data_ID")]
# Only keep adrift ID 
colnames(meta)[10]<-'Adrift_id'
meta= meta[meta$Adrift_id %in% unique(combined_dataframe$Adrift_id),]


# Calculate data duration
meta$start_time=as.POSIXct(meta$Data_Start, format = "%m/%d/%Y %H:%M")
meta$end_time = as.POSIXct(meta$Data_End,   format = "%m/%d/%Y %H:%M")
meta$date = as.POSIXct(meta$Data_Start,    format = "%m/%d/%Y")




# Deployment Duration
meta <- meta %>%
  mutate(duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")))
meta$duration_days= round(meta$duration_hours/24)
meta$loc_date = paste0(meta$Site,'_', meta$date)

# Add the meta data to the observations dataframe
bwObs = merge(meta, combined_dataframe, by= "Adrift_id", all.x = TRUE, all.y = TRUE)
bwObs$Date = as.Date(floor_date(bwObs$start_time))
bwObs = bwObs[!is.na(bwObs$Project),]


##################################################
## Create a time series for each deployment time Location
###################################################


bwSub = bwObs[bwObs$loc_date=='HUM_2022-11-16',]
metaSub = meta[meta$loc_date=='HUM_2022-11-16',]

dataStart = as.Date((floor_date(metaSub$start_time)))-1
dataStop = as.Date((ceiling_date(metaSub$end_time)))+1


# The result will be a new DataFrame with a column 'Date' containing the time series for each DriftId
depTimesSeries = seq.POSIXt(min(as.POSIXlt(dataStart)), 
                            max(as.POSIXlt(dataStop)), 
                            by = 60)













