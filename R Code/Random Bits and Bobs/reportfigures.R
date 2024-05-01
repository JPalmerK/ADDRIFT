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

meta$DeploymentId = paste0(meta$Site,'_', meta$date)


# Deployment Duration
meta <- meta %>%
  mutate(duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")))
meta$duration_days= round(meta$duration_hours/24)

# Add the meta data to the observations dataframe
bwObs = merge(meta, combined_dataframe, by= "Adrift_id", all.x = TRUE, all.y = TRUE)
bwObs$Date = as.Date(floor_date(bwObs$start_time))
bwObs = bwObs[!is.na(bwObs$Project),]

##
#Create a time series to populate with detection positive hours per day
#

dataStart = as.Date((floor_date(meta$start_time)))-1
dataStop = as.Date((ceiling_date(meta$end_time)))+1

meta$DateStart = dataStart
meta$DateStop = dataStop



# Create sequence for each drift. This will be aggregated in the next step
nBuoysdf <- meta %>%
  group_by(Adrift_id) %>%
  do(data.frame(Date = seq.Date(.$DateStart, .$DateStop, by = "1 day")))


# Number of Buoys in the water per date
nBuoysdf =aggregate(data=nBuoysdf, Adrift_id~Date, FUN = length)


# The result will be a new DataFrame with a column 'Date' containing the time series for each DriftId
timeSeriesDay = data.frame(Date = seq.Date(min(dataStart), max(dataStop), by = "1 day"))

# merge the time series and the effort data
timeSeriesDay= merge(timeSeriesDay, nBuoysdf, all.x = TRUE)

# Daily Effort
timeSeriesDay$Effort = timeSeriesDay$Adrift_id*24
timeSeriesDay$DateFactor = as.factor(timeSeriesDay$Date)

# Count detection positive hours for each date
DateStart = min(dataStart)
DateStop =  max(dataStop)

hour = seq.POSIXt(from = as.POSIXct(DateStart)-hours(1), 
                            to = as.POSIXct(DateStop)+hours(1), by = "hour")

adrifIDs= as.factor(unique(meta$Adrift_id))
callTypes = as.factor(unique(bwObs$Call))
hourlyPresence = expand.grid(hour=hour, callTypes = callTypes)


for(ii in 1:length(callTypes)){
  dataSub = bwObs[bwObs$Call==callTypes[ii],]
  aa = aggregate(data= dataSub, 
                 Species.Code~Date_and_Hour+Adrift_id+Site, FUN= length)
  aa$Species.Code[aa$Species.Code>1] =1 #there shouldn't be duplicates here
  
  aa$date = as.Date(aa$Date_and_Hour)
  
  # Figure out detection positive hours
  hourlyPrese= aggregate(aa$Species.Code~aa$date, FUN=sum)
  colnames(hourlyPrese)<-c('Date',as.character(callTypes[ii]))
  
  hourlyPrese$DateFactor = as.factor(hourlyPrese$Date)
  hourlyPrese$Date <- NULL
  
  # Merge 
  timeSeriesDay= merge(timeSeriesDay, hourlyPrese, 
                        by= 'DateFactor', all.x = TRUE)
  
}

# Remove data with no effort and fill in zeros
#timeSeriesDay=timeSeriesDay[!is.na(timeSeriesDay$Effort),]# remove line to keep spaces
timeSeriesDay[is.na(timeSeriesDay)]=0
timeSeriesDay$Month= month(timeSeriesDay$Date)
timeSeriesDay$Year = year(timeSeriesDay$Date)

##############################################
# Plotting
###############################################
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(viridis)

# create a new dataframe that has the proportion of detection positive
# hours for each calltype

dphData = timeSeriesDay[,c('Date', 'Year', 'Effort')]
dphData$Calltype='`B NE Pacific`'
dphData$dph = timeSeriesDay$`B NE Pacific`


dphData1 = timeSeriesDay[,c('Date', 'Year', 'Effort')]
dphData1$Calltype='`A NE Pacific`'
dphData1$dph = timeSeriesDay$`A NE Pacific`


dphData2 = timeSeriesDay[,c('Date', 'Year', 'Effort')]
dphData2$Calltype='D'
dphData2$dph = timeSeriesDay$D

dphData = rbind(dphData,dphData1,dphData2)
dphData$Proportion = dphData$dph/dphData$Effort
dphData$Month=month(dphData$Date)
dphData$Season='Spring'
dphData$Season[dphData$Month>=6 &dphData$Month <= 8]='Summer'
dphData$Season[dphData$Month>8]='Autumn'
dphData$MonthDay <- format(dphData$Date, "%m-%d")
dphData$Year= as.factor(dphData$Year)

coef= 1

ggplot(data = dphData, aes(x=Date, y=Effort))+
  facet_grid(~Year, scales = 'free_x')+
  geom_bar(data = unique(dphData[,c(1:3)]), stat="identity")+
  geom_point(aes(y=dph, color =Calltype))+
  scale_color_viridis(discrete = TRUE)+
  ylab('Survey Effort (hrs)')+
  scale_y_continuous(sec.axis = sec_axis(~.*coef, name = "Calltype"))
# Issues, effort is more than 300, max should be 100. Weird summing going on


coef=1
num_ticks <- 10
uniqueDays = unique(dphData$MonthDay)
tick_positions <- seq(1, length(uniqueDays),
                      length.out = num_ticks)

breaks <- uniqueDays[tick_positions]
labels <- uniqueDays[tick_positions]

# get rid of spring
dphData = dphData[dphData$Season != 'Spring',]
dphData$dph[dphData$Effort==0]=NaN

ggplot(data = dphData, aes(x = MonthDay, y = Effort)) +
  facet_grid(~Season, scales = 'free_x') +
  geom_bar(
    data = dphData[which(!duplicated(dphData[, c(1:3)])), ],
    aes(fill = Year), position = position_dodge(), stat = "identity") +
  scale_fill_manual(values = c("gray80", "gray40")) +
  geom_point(
    data = dphData,
    aes(y = dph, x = MonthDay, color = Calltype, shape=Year)) +
  scale_color_viridis(discrete = TRUE)+
  ylab('Survey Effort (hrs)') +
  scale_x_discrete(
    name = "Month and Day",
    breaks = breaks,
    labels = labels) +
  theme_bw()+
  scale_y_continuous(sec.axis = sec_axis(~ .* coef, name = "Det Pos Hours"))

#################################################################
# Three d plot

p <- (ggplot(dphData[dphData$Season=='Spring',],
             aes(x=MonthDay, y=Effort, color=measure, group=Year)) +
        scale_color_discrete(guide=FALSE) +
        geom_point() + facet_wrap(~ Calltype) +
        geom_line(color="grey", alpha=0.5) +
        xlab(""))
p

#########################################################

# 3d plot for each deployment

metaSub = meta[meta$DeploymentId=='HUM_2022-11-16',]

 hour = seq.POSIXt(from = as.POSIXct(metaSub$start_time[1])-hours(1), 
                                    to = as.POSIXct(metaSub$end_time[1])-hours(1), 
                   by = "hour")
 tSeriesSub=data.frame(hour =hour, DriftNumber = metaSub$Adrift_id[1])
 
 
 for (ii in 2:nrow(metaSub)){
   
   hour = seq.POSIXt(from = as.POSIXct(metaSub$start_time[ii])-hours(1), 
                     to = as.POSIXct(metaSub$end_time[ii])-hours(1), 
                     by = "hour")
   
   tSeriesTemp=data.frame(hour =hour, DriftNumber = metaSub$Adrift_id[1])
   
   tSeriesSub= rbind(tSeriesSub, tSeriesTemp)
   
 }
 
 hourlyPresenceDep = expand.grid(hour=tSeriesSub$hour,
                              DriftNumber = tSeriesSub$DriftNumber,
                              callTypes = callTypes)
 minute(hourlyPresenceDep$hour)=0
 

 



library(rayshader)
library(ggplot2)
d <- read.table(text=' x   y     z
t1   5   high
t1   2   low
t1   4   med
t2   8   high
t2   1   low
t2   3   med
t3  50   high
t3  12   med
t3  35   low', header=TRUE)

p <- ggplot(d, aes(x, z, fill = y)) +
  geom_tile() +
  scale_fill_fermenter(type = "div", palette = "RdYlBu")

plot_gg(p)


datasub = dphData[dphData$Adrift_id==1,]

ggplot()


###################################################
# See if we can make Taiki's outline work

dataSub = dphData[dphData$Adrift_id==1,]

ggplot(dphData)+
 facet_grid(~Season) +
  geom_step(data = dataSub, aes(x=MonthDay, y=Effort))



###############################################


ggplot(data = dphData,) +
  facet_grid(~Season, scales = 'free_x') +
  geom_step(aes(x=MonthDay, y= Effort, color = Year))
  geom_point(
    data = dphData,
    aes(y = dph, x = MonthDay, color = Calltype, shape=Year)) +
  scale_color_viridis(discrete = TRUE)+
  ylab('Survey Effort (hrs)') +
  scale_x_discrete(
    name = "Month and Day",
    breaks = breaks,
    labels = labels) +
  theme_bw()+
  scale_y_continuous(sec.axis = sec_axis(~ .* coef, name = "Det Pos Hours"))










ggplot(data = dphData, aes(x=MonthDay, y=Effort))+
  facet_grid(~Season, scales = 'free_x')+
  geom_bar(data = dphData[which(!duplicated(dphData[,c(1:3)])),],
           aes(fill=Year),  position = position_dodge(), stat="identity")+
  scale_fill_manual(values= wes_palette("Darjeeling2", n = 2))+
  geom_point(data = dphData, 
             aes(y=dph, 
                 x= MonthDay,
                 color =Calltype))+
  scale_color_viridis(discrete = TRUE)+
  ylab('Survey Effort (hrs)')+
  theme_bw()+
  scale_x_discrete(name = "Month and Day", 
                   breaks = tick_positions, 
                   labels = uniqueDays[tick_positions]) +
  scale_y_continuous(sec.axis = sec_axis(~.*coef,name = "Prop. Det Pos Hours"))
# Issues, effort is more than 300, max should be 100. Weird summing going on




ggplot(data = timeSeriesDay, aes(Date, Adrift_id))+
  facet_grid(~Year, scales = 'free_x')+
  geom_bar(stat='identity', alpha=0.5)+
  geom_line(data= dphData, aes(x=Date, y=Proportion, 
                               color =Calltype))+
  




# Number of Hours blue whale calls were present
BW_counts =  aggregate(data=bwObs, Adrift_id~Call+Date, FUN=length)

TimeSeries

