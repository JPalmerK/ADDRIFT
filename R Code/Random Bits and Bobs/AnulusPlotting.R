
rm(list=ls())
library(dplyr)
library(marmap)
library(lubridate)
library(ggplot2)
library(stats)



# Load the CSV files for the GPS data

# Set the directory where your CSV files are located
csv_directory <- "F:\\GPS_CSV-20230923T045356Z-001\\MorroBay Mar 2023"

# Get a list of CSV files in the directory (adjust the pattern if needed)
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)

# Combine the list of dataframes into a single dataframe
GPSdf <- do.call(rbind, lapply(csv_files, read.csv))


# Convert to datetime, create date hour column
GPSdf$UTC=as.POSIXct(GPSdf$UTC,tz = 'UTC')
GPSdf$UTCDatehour = GPSdf$UTC
minute(GPSdf$UTCDatehour)<-0
second(GPSdf$UTCDatehour)<-0

# trim data a bit
GPSdf= GPSdf[GPSdf$Latitude>35.2,]

csv_directory='F:\\GPS_CSV-20230923T045356Z-001\\MorroBay Mar 2023 Noise Files'

# Add noise level data
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store dataframes
dataframes_list <- list()

# Loop through each CSV file
for (csv_file in csv_files) {
  # Read the CSV file
  df <- read.csv(csv_file, header = TRUE)
  
  # Extract the first and eighth columns
  df <- df[, c(1, 8)]
  colnames(df)[1]<-'UTC'
  
  
  # Extract the first 10 characters from the filename
  file_name <- substr(basename(csv_file), 1, 10)
  
  # Create a 'DriftName' column with the extracted filename
  df$DriftName <- file_name
  
  # Append the dataframe to the list
  dataframes_list[[file_name]] <- df
}

# Combine all dataframes into a single dataframe
noiseDf <- bind_rows(dataframes_list)
noiseDf$datetime_posix <- as.POSIXct(noiseDf$UTC, 
                                     format = "%Y-%m-%dT%H:%M:%OSZ",
                                      tz='UTC')

rm(list = ls(all.names = TRUE)[!ls() %in% c("noiseDf", "GPSdf")])

###########################################################
# Get median noise in each deployment hour
###########################################################

# load the windfarm area and convert to UTC
library(sp)

# Wind call boundary from Taiki
windCall <- readRDS(file.path('F:\\GPS_CSV-20230923T045356Z-001',
                              'WindCallBoundary.RData'))
WLAdf = as.data.frame(windCall[[2]][[3]][[1]])
colnames(WLAdf)<-c('Long', 'Lat')


WLAdf = as.data.frame(windCall[[2]][[3]][[1]])

# Make them spatial points for plotting
cord.dec = SpatialPoints(cbind(WLAdf$V1,WLAdf$V2),
                         proj4string=CRS("+proj=longlat"))
cord.dec = spTransform(cord.dec, CRS("+init=epsg:32610"))
head(coordinates(cord.dec))

WLAdf$UTMx =  coordinates(cord.dec)[,2]
WLAdf$UTMy =  coordinates(cord.dec)[,1]

rm(list=c('cord.dec', 'windCall'))

##################################################################
# Create evenly spaced time dataframe
#####################################################################

MaxTime =  as.POSIXct('2023-03-15 16:00:00', tz = 'UTC')
MinTime =  as.POSIXct('2023-03-11 22:00:00', tz = 'UTC')

dataOut = expand.grid(
  DriftName =unique(GPSdf$DriftName),
  UTC = seq(MinTime, MaxTime, by= '1 hour')
  
)
dataOut$Latitude =0
dataOut$Longitude =0
dataOut$NoiseLevel =0



for(ii in 1:8){
  # Pull out each drift
  GPSsub =  GPSdf[GPSdf$DriftName==unique(GPSdf$DriftName)[ii],]
  NLsub = noiseDf[GPSdf$DriftName==unique(GPSdf$DriftName)[ii],]
  
  # Functions to estimate lat, long, and NL
  lat_fun <- approxfun(GPSsub$UTC, GPSsub$Latitude)
  lon_fun <- approxfun(GPSsub$UTC, GPSsub$Longitude)
  NL_FUN <-  approxfun(NLsub$datetime_posix, NLsub$TOL_500)
  
  # Index values for the final table
  tableIdx =which(dataOut$DriftName==unique(GPSdf$DriftName)[ii])
  
  # Fill in the data for each drift
  dataOut$Latitude[tableIdx] =lat_fun(dataOut$UTC[tableIdx])
  dataOut$Longitude[tableIdx] = lon_fun(dataOut$UTC[tableIdx])
  dataOut$NoiseLevel[tableIdx] = NL_FUN(dataOut$UTC[tableIdx])
  
  
}



library(ggplot2)
library(viridis)
# Idiot check
ggplot(dataOut)+
  geom_point(aes(x = Longitude, y=Latitude, color=NoiseLevel))+
  scale_color_viridis_b()

ggplot(dataOut)+
  geom_path(aes(x = Longitude, y=Latitude, group=DriftName))+
  scale_color_viridis_b()


dataOut= dataOut[!is.na(dataOut$Latitude),]
################################################################
# Create a function to calculate detection/SNR range
###############################################################


# frequency dependent acoustic absorption
AcousticAbsorption <-function(f, Z=0, Temp=5, S=35, pH=8){
  
  # Following Kinsler, et al "Fundamentals of Acoustics, Fourth Edition" p. 226-228.
  # f = frequency in Hz
  # Z = depth in km
  # Temp = temperature in C
  # S = salinity in ppt
  # pH = pH
  
  
  f_1 = 780*exp(Temp/29)
  f_2 = 42000*exp(Temp/18)
  A = 0.083*(S/35)*exp(Temp/31 - Z/91 + 1.8*(pH-8))
  B = 22*(S/35)*exp(Temp/14-Z/6)
  C = 4.9e-10*exp(-Temp/26 - Z/25)
  boric_acid = A/(f_1^2+f^2) # contribution from boric acid
  MgSO4 = B/(f_2^2+f^2) # contribution from MgSO4
  hydrostatic = C # contribution from hydrostatic pressure
  alpha = (boric_acid + MgSO4 + hydrostatic)*f^2 #db/KM
  
  return(mean(alpha))
}

# Sonar equation over which to optimize r given, SL, NL, depth, frequency
# and SNR threshold
logfun <- function(SL, NL, SNRthresh, h, f) {
  function(r) {
    alpha = AcousticAbsorption(f)
    abs(SL - (15*log10(r)+10*log10(h/2)+(alpha/1000)*r) - NL - SNRthresh)
  }
}


# source, level, noise level, frequen(ies), and SNR threshold
f=500:900
SL_max=180
SL_min = 170
NL=80
SNRthresh=2
h=20

# estimate the maximum range in meters using simplified sonar equations
dataOut$MaxRange_SNR1 =0
dataOut$MinRange_SNR1 =0

dataOut$MaxRange_SNR5 =0
dataOut$MinRange_SNR5 =0


# SNR threshold of one and 10
for(ii in 1:nrow(dataOut)){
  
  maxRange = optim(par=1000, 
                   fn=logfun(SL_max, dataOut$NoiseLevel[ii],
                             SNRthresh, h, f), 
                   method='Brent', lower=1, upper=10e8)$par
  minRange = optim(par=1000, 
                   fn=logfun(SL_min, dataOut$NoiseLevel[ii],
                             1, h, f), 
                   method='Brent', lower=1, upper=10e8)$par
  
  # Annulus for SNR 1 db
  dataOut$MaxRange_SNR1[ii] = maxRange
  dataOut$MinRange_SNR1[ii] = minRange
  
  # Annulus for SNR 10 db
  maxRange = optim(par=1000, 
                   fn=logfun(SL_max, dataOut$NoiseLevel[ii],
                             10, h, f), 
                   method='Brent', lower=1, upper=10e8)$par
  minRange = optim(par=1000, 
                   fn=logfun(SL_min, dataOut$NoiseLevel[ii],
                             10, h, f), 
                   method='Brent', lower=1, upper=10e8)$par
  
  dataOut$MaxRange_SNR5[ii] =maxRange
  dataOut$MinRange_SNR5[ii] =minRange
  
  
}

# Make them spatial points for plotting
cord.dec = SpatialPoints(cbind(dataOut$Longitude,dataOut$Latitude),
                         proj4string=CRS("+proj=longlat"))
cord.dec = spTransform(cord.dec, CRS("+init=epsg:32610"))
head(coordinates(cord.dec))

dataOut$UTMx =  coordinates(cord.dec)[,2]
dataOut$UTMy =  coordinates(cord.dec)[,1]

rm(list=c(cord.dec))

####################################################################
# Create the Plots
###############################################################


# get the unique times
timestep = unique(dataOut$UTC)

circleFun <- function(centerX = 0, centerY = 0, r = 1, npoints = 100){
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- centerX + r * cos(tt)
  yy <- centerY + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}



for(ii in 1:length(timestep)){
  
  dataSub = subset(dataOut, UTC==timestep[ii] & !is.na(NoiseLevel))
  
  #circle data
  outterCirc = data.frame()
  
  for(jj in 1:nrow(dataSub)){
    
    outData =as.data.frame(circleFun(dataSub$UTMx[jj], 
                       dataSub$UTMy[jj],
                       r = dataSub$MaxRange_SNR5[jj]))
    outData$DriftName = dataSub$DriftName[jj]
    outData$subid='Yes'
    outData$UTM = dataSub$UTC[1]
    
    inData =as.data.frame(circleFun(dataSub$UTMx[jj], 
                                     dataSub$UTMy[jj],
                                     r = dataSub$MinRange_SNR5[jj]))
    inData$DriftName = dataSub$DriftName[jj]
    inData$subid='NO'
    inData$UTM = dataSub$UTC[1]
      
    outterCirc = rbind(outterCirc, outData, inData)
    
    rm(list=c('inData', 'outData'))
    
  }

  # Plotting to check things, ii=50 is a good start
    ggplot(outterCirc) +
      geom_polygon(aes(x = x, y = y,
                       group = DriftName,
                       subgroup = subid))+
      geom_path(data= dataOut, aes(x =UTMx,
                                   y = UTMy,
                                   group=DriftName))+
      #geom_path(data=WLAdf, aes(x=UTMx, y=UTMy))+
      facet_wrap(~DriftName, nrow = 2)+
      ggtitle('170<SL<180, SNR= 10')+
      ylab('UTC')+xlab('UTC')
  }
  
  
  
#######################################################
# Simulation
########################################################


# Simulate an animal call from somewhere in the survey area
set.seed(1) #good place
SampleWhale = data.frame(
  locX = rnorm(1, mean =  mean(dataOut$UTMx, na.rm = TRUE),
               sd = sd(dataOut$UTMx,na.rm = TRUE)),
  locY = rnorm(1, mean=mean(dataOut$UTMy, na.rm = TRUE),
                 sd=sd(dataOut$UTMy,na.rm = TRUE)),
  utm= sample(dataOut$UTC, 1)
  )




# Plotting to check things, ii=50 is a good start
ggplot(outterCirc) +
  geom_polygon(aes(x = x, y = y,
                   group = DriftName,
                   subgroup = subid))+
  geom_path(data= dataOut, aes(x =UTMx,
                               y = UTMy,
                               group=DriftName))+
  facet_wrap(~DriftName, nrow = 2)+
  geom_point(data= SampleWhale, aes(x=locX, y = locY), color = 'red')+
  ggtitle('170<SL<180, SNR= 10')+
  ylab('UTC')+xlab('UTC')
  

# get the distance to each of the drifts
distGrid= expand.grid(
  DriftName = unique(dataOut$DriftName),
  Range = 0,
  NL = 0
)


# Drift information for that time
driftSample = dataOut[dataOut$UTC==SampleWhale$utm,]
driftSample$rangekm = sqrt(
  (SampleWhale$locX-driftSample$UTMx)^2+
  (SampleWhale$locY-driftSample$UTMy)^2)/1000




# Transmission loss
alpha = AcousticAbsorption(200)
TL<- function(r, h=100, alpha=0){
  (15*log10(r)+10*log10(h/2)+(alpha/1000)*r)}

# Estimate the range
driftSample$TL = TL(driftSample$range*1000)

# Calls must be within this snr level.
driftSample$SNR_rec = 177- driftSample$NoiseLevel-driftSample$TL



# Create 8 grids to show where the whale might be
whaleGrid = expand.grid(
  x= seq(min(dataOut$UTMx)-5000, max(dataOut$UTMx)+5000, by=500),
  y= seq(min(dataOut$UTMy)-5000, max(dataOut$UTMy)+5000, by=500),
  DriftName = unique(dataOut$DriftName)
)


#ggplot(whaleGrid)+geom_point(aes(x=x, y=y))+facet_grid(~DriftName)

# Now pick a point in time and calculate the SNR each grid
whaleGrid$r =  sqrt(
  (SampleWhale$locX-driftSample$UTMx)^2+
    (SampleWhale$locY-driftSample$UTMy)^2)/1000 


whaleGrid = merge(whaleGrid, driftSample[,c('DriftName', 'NoiseLevel')], by= 'DriftName')

# step through the drifts and calculate the range from the drifter and the SNR
# values
for(drift in unique(dataOut$DriftName)){
  # drift index
  idx = which(whaleGrid$DriftName==drift)
  whaleGrid$r[idx] =sqrt((whaleGrid$x[idx]-driftSample$UTMx[driftSample$DriftName== drift])^2+
                         (whaleGrid$y[idx]-driftSample$UTMy[driftSample$DriftName== drift])^2)/1000 

    }

whaleGrid$SNR_min= SL_min-TL(whaleGrid$r*1000)-whaleGrid$NoiseLevel
whaleGrid$SNR_max= SL_max-TL(whaleGrid$r*1000)-whaleGrid$NoiseLevel

ggplot(sample_n(whaleGrid,20000))+#dplyr
  geom_point(aes(x=x, y=y, color = SNR_min))+
  facet_grid(~DriftName)+
  scale_color_viridis()


# Ok, 'driftsample' has the observed SNR values so subset from the grid
trimmedGrid = data.frame()
for(drift in unique(whaleGrid$DriftName)){
  
  trimmedGrid=rbind(trimmedGrid, subset(whaleGrid, 
                                DriftName == drift &
                                SNR_max>driftSample$SNR_rec[driftSample$DriftName==drift] &
                                SNR_min<driftSample$SNR_rec[driftSample$DriftName==drift]  ))
}

# This plot shows where SNR values matching what was recorded could have occurred
ggplot(sample_n(trimmedGrid,20000))+#dplyr
  geom_point(aes(x=x, y=y, color = SNR_min))+
  facet_wrap(~DriftName,nrow = 2)+
  scale_color_viridis()+
  geom_point(data= SampleWhale, aes(x=locX, y = locY), color = 'red')


# Ok, Now find the unique values here
temp = paste0(as.character(trimmedGrid$x, trimmedGrid$y))
potentialLocs = trimmedGrid[!duplicated(trimmedGrid[,c('x', 'y')]),]

# Ok, this example shows that for a call located within the 'survey area' that
# it was most likely within the array
ggplot()+#dplyr
  geom_point(data = sample_n(potentialLocs,2500), 
             aes(x=x, y=y), color = 'gray')+
  geom_path(data= dataOut, aes(x =UTMx,
                               y = UTMy,
                               group=DriftName), size=1.5, color='blue')+
  geom_point(data = driftSample, aes(x=UTMx, y= UTMy), color = 'green')+
  geom_point(data= SampleWhale, aes(x=locX, y = locY), size =3, color = 'red')










ggplot(outterCirc) +
  geom_polygon(aes(x = x, y = y, 
                   group = DriftName,
                   subgroup = subid), alpha =.15)+
  geom_path(data= dataOut, aes(x =UTMx, 
                               y = UTMy, 
                               group=DriftName))+
  ggtitle('170<SL<180, SNR= 10')+
  ylab('UTC')+xlab('UTC')




library(ggplot2)
library(mapproj)
library(mapdata)
library(ggOceanMaps)
library(ncdf4) # package for netcdf manipulation
library(sp)


# Prep the GPS tracks
# First lat/lon
sputm <- SpatialPoints(NoiseSummary[,c('Longitude', 'Latitude')],
                       proj4string=CRS("+init=epsg:3857"))

# Turn into segments
GPSsegments = NoiseSummary[(1:(nrow(NoiseSummary)-1)), 
                     c('Latitude','Longitude', 'Median_TOL_500','UTC',
                       'DriftName')]

GPSsegments$EndLat = NoiseSummary$Latitude[c(2:nrow(NoiseSummary))]
GPSsegments$EndLon = NoiseSummary$Latitude[c(2:nrow(NoiseSummary))]
GPSsegments$id = 1:nrow(GPSsegments)
GPSsegments$NL = NoiseSummary$ThirdOctave_44_56_median[1:nrow(NoiseSummary)-1]
GPSsegments$NLnorm = (GPSsegments$NL-min(GPSsegments$NL))/
  (max(GPSsegments$NL-min(GPSsegments$NL)))*5


ggplot(GPSsegments) +
  geom_segment(
    mapping = aes(x=Long, y=Lat, 
                  xend=EndLon, yend=EndLat,
                  group=DriftName), 
    inherit.aes = FALSE
  )



# Use the extents to download the GEBCo data from here:https://download.gebco.net/ 
nc_data <- nc_open('F:\\GPS_CSV-20230923T045356Z-001\\-121.62_e-121.43.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
elevation <- ncvar_get(nc_data, "elevation")
bathyRelief = data.frame(Lat=sort(x=rep(lat, length(lon))),
                         Long=rep(lon, length(lat)),
                         depth=c((elevation)))

# Convert to UTC
basemp.dec = SpatialPoints(cbind(bathyRelief$Long, bathyRelief$Lat),
                           proj4string = CRS("+proj=longlat"))
basemapTiles <- as.data.frame(basemp.dec)
basemapTiles$depth = c((elevation))
colnames(basemapTiles)[1:2]<-c('x', 'y')




# ggplot()+
#   geom_raster(data = basemapTiles,
#               aes(x=x, y=y,fill= depth))+
ggplot()+
 geom_line(data = GPSdf, aes(x=Longitude, y=Latitude, 
                group = DriftName))+
  geom_polygon(data= WLAdf, aes(x=Long, y=Lat), fill= NA, color='black')
  








ggplot()+
  geom_raster(data = basemapTiles,
              aes(x=x, y=y,fill= depth))++
  new_scale_fill()+
  geom_ribbon(data = spgeo, 
              aes(x=x, y=y,
                  xmin = Xmin, 
                  xmax = Xmax,
                  fill=Valitated,
                  group=group)) +
  ylim(40.5,41.25)+
  xlab('Longitude')+
  ylab('Latitude')



# Plot 
ggplot()+
  geom_raster(data = bathyRelief,
              aes(x=Long, y=Lat,fill= depth))+  
  geom_polygon(data= WLAdf, aes(x=Long, y=Lat), fill= NA, color='black')+
  geom_line(data = GPSdf, aes(x = Longitude, y=Latitude, group =DriftName))

## Seems the projections are wrong

# Gebco projection
# Wind lease area projection
# GPS projection

library(sf)
WLA <- st_read(
  "F:\\GPS_CSV-20230923T045356Z-001\\MorroBay_WEA_2021_11_12.shp")

GPSsf <- st_as_sf(GPSdf, 
                  coords = c("Longitude", "Latitude"), 
                  crs = 4326)


df_tracks <- GPSsf %>%
  group_by(DriftName) %>%
  summarize(geometry = st_cast(geometry, "LINESTRING"))


ggplot() + 
  geom_sf(data = WLA, size = 1.5, color = "black") + 
  geom_sf(data=df_tracks)+
  coord_sf()









ggplot() +
  geom_sf(data = WLA, size = 1.5, color = "black") + 
  geom_sf(data=df_tracks, size = .2)+
  coord_sf()

ggplot() +
  geom_sf(df_tracks) +
  theme_minimal() +
  scale_color_discrete(name = "Device ID")
In this code, we first group the df_sf sf object by deviceId using group_by. Then, we use summarize to create a new sf object df_tracks that contains linestrings for each GPS track. Finally, we use geom_sf to plot the GPS tracks, with distinct colors based on deviceId. Each track will be represented as a continuous line.













autoplot.bathy(mbBathy, geom=c("tile","contour")) +
  scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen") +
  geom_point(data = ctd, aes(x = Longitude, y = Latitude),
             colour = 'black', size = 3, alpha = 1, shape = 15) +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation") +
  coord_cartesian(expand = 0)+
  ggtitle("A marmap map with ggplot2") 

