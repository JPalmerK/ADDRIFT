rm(list =ls())
library(dplyr)
library(stats)

# 
# # Estimating GPS positio error
# We estimated the GPS position error by selecting selecting every second
# GPS location from each of the drifting buoys. Using those data estimated
# GPS position for the remaining second half of the dataset using liner interpolation.
# Results from this analysis indicated that GPS error ranged from 5m to 6km with 
# 25th, 50th, and 75th quartiles at  147m, 425m, and 966m respecively. Larger
# error in GPS location was associated with missed GPS signals. For the simulation
# we used a maximum error of 1151m or the 80th percentile. As the units do not
# travel lineraly between GPS points, This is likely an overestimation of the
# expected positional error. 

# Evaluating propagation loss can be complicated and propagaion models were outside
# the avilable resources for this anlysis. Thus to compensate for uncertain soundspeed
# profiles we assumed that a 20% variation between the simulated soundspeed (1500m/s)
# and the potential real world, or observed propagation times.Thus the total TDOA
# error was the sum of the position error (1151m) and the soundspeed error (300m)
# divided by the estimated soundspped(1500m/s) resuting in a TDOA error of 0.86sec 


haversine_dist <- function(long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6378137
  d <- R*c
  return(d)
}

# Set the directory where your CSV files are located
csv_directory <- "F:\\GPS_CSV-20230923T045356Z-001\\MorroBay Mar 2023"

# Get a list of CSV files in the directory (adjust the pattern if needed)
csv_files <- list.files(path = csv_directory, 
                        pattern = "*.csv", full.names = TRUE)

# Combine the list of dataframes into a single dataframe
GPSdf <- do.call(rbind, lapply(csv_files, read.csv))

# Add UTC coords with the sp package
cord.dec = SpatialPoints(cbind(GPSdf$Longitude,GPSdf$Latitude),
                         proj4string=CRS("+proj=longlat"))
cord.dec = spTransform(cord.dec, CRS("+init=epsg:32610"))
GPSdf$UTMx =  coordinates(cord.dec)[,1]
GPSdf$UTMy =  coordinates(cord.dec)[,2]

# Convert to datetime, create date hour column
GPSdf$UTC=as.POSIXct(GPSdf$UTC,tz = 'UTC')
GPSdf$UTCDatehour = GPSdf$UTC
minute(GPSdf$UTCDatehour)<-0
second(GPSdf$UTCDatehour)<-0

# Pull out every other location for modelling
moddelingPts = GPSdf[seq(1, nrow(GPSdf),by=2), ]
predPts =GPSdf[seq(2, nrow(GPSdf),by=2), ]
predPts$EstLat=0
predPts$EstLon=0

predPts$EstLatSpline=0
predPts$EstLonSpline=0


DriftNames = unique(GPSdf$DriftName)


for(drift in DriftNames){
  GPSsub = subset(moddelingPts, DriftName == drift)

  UTMflon <- approxfun(GPSsub$UTC, GPSsub$Longitude)
  UTMflat <- approxfun(GPSsub$UTC, GPSsub$Latitude)
  
  splineLon <- splinefun(GPSsub$UTC, GPSsub$Longitude, method = 'fmm')
  splineLat <- splinefun(GPSsub$UTC, GPSsub$Latitude, method='fmm')
  
  ##############################################################
  # Calculate the range from the whale to the GPS, TDOA and RL
  ############################################################
  # Lat/lon of the drift when the call was produced
  
  predPts$EstLon[predPts$DriftName==drift] = UTMflon(predPts$UTC)
  predPts$EstLat[predPts$DriftName==drift] = UTMflat(predPts$UTC)
  
  
  predPts$EstLatSpline[predPts$DriftName==drift] = splineLat(predPts$UTC)
  predPts$EstLonSpline[predPts$DriftName==drift] = splineLon(predPts$UTC)
  
  
}

predPts$ModellingError = haversine_dist(predPts$Longitude, predPts$Latitude,
                                        predPts$EstLon, predPts$EstLat) 



predPts$ModellingErrorSpline = haversine_dist(predPts$Longitude, predPts$Latitude,
                                        predPts$EstLonSpline, predPts$EstLatSpline) 
predSub = subset(predPts, DriftName !='ADRIFT_048')

aa = quantile(predPts$ModellingError, seq(0,1, by=.1), na.rm=TRUE)
bb =quantile(predPts$ModellingErrorSpline, seq(0,1, by=.1), na.rm=TRUE)


aa = quantile(predSub$ModellingError, seq(0,1, by=.25), na.rm=TRUE)
bb =quantile(predSub$ModellingErrorSpline, seq(0,1, by=.1), na.rm=TRUE)



ggplot(predSub)+
  geom_path(aes(Longitude, Latitude, group=DriftName, color= DriftName))+
  geom_point(aes(Longitude, Latitude, group=DriftName,), color ='black')+
  geom_point(data= predPts, aes(x=EstLon, y= EstLat ), color = 'cadetblue')+
  geom_point(data= predPts, aes(x=EstLonSpline, y= EstLatSpline ), color = 'pink')







