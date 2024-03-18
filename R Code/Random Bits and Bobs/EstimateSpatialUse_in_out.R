########################################################
# Simulation of SNR and TDOA
################################################
rm(list =ls())


######################################################################
# Load and prep the data
#################################################################
library(sp)
library(ncdf4)
library(ggplot2)
library(lubridate)
library(dplyr)

# Load data, drift GPS, and noise levels pre-process, and wind lease area

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


# Add noise level data
csv_directory='F:\\GPS_CSV-20230923T045356Z-001\\MorroBay Mar 2023 Noise Files'
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE)
dataframes_list <- list()

# Loop through each CSV file load, change name
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

# Clean out data for drifts that don't have gps or noise levels
GPSdf= subset(GPSdf, DriftName %in% noiseDf$DriftName)
noiseDf= subset(noiseDf, DriftName %in% GPSdf$DriftName)


# Load the wind lease area
library(sf)
WLA <- st_read(
  "F:\\GPS_CSV-20230923T045356Z-001\\MorroBay_WEA_2021_11_12.shp")
WLA = as.data.frame(WLA[[6]][[1]][1])
colnames(WLA)<-c('UTMx','UTMy')

# Add lat/lon
# Add UTC coords with the sp package
cord.dec = SpatialPoints(cbind(WLA$UTMx,WLA$UTMy),
                         proj4string=CRS("+init=epsg:32610"))


cord.dec = spTransform(cord.dec, CRS("+proj=longlat"))
WLA$Lon =  coordinates(cord.dec)[,1]
WLA$Lat =  coordinates(cord.dec)[,2]



# Check
p<-ggplot()+
  geom_path(data = GPSdf, aes(x=UTMx, y=UTMy, group = DriftName), color='gray30')+
  geom_path(data= WLA, aes(x= UTMx, y=UTMy), color = 'gold')+
  theme_bw()
p


# Load the bthymetry


# Use the extents to download the GEBCo data from here:https://download.gebco.net/ 
nc_data <- nc_open('F:\\GPS_CSV-20230923T045356Z-001\\gebco_2023_n36.0791_s34.6025_w-122.8052_e-120.8013.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
elevation <- ncvar_get(nc_data, "elevation")
bathyRelief = data.frame(Lat=sort(x=rep(lat, length(lon))),
                         Long=rep(lon, length(lat)),
                         depth=c((elevation)))

# Add UTC coords with the sp package
cord.dec = SpatialPoints(cbind(bathyRelief$Long,bathyRelief$Lat),
                         proj4string=CRS("+proj=longlat"))
cord.dec = spTransform(cord.dec, CRS("+init=epsg:32610"))
bathyRelief$UTMx =  coordinates(cord.dec)[,1]
bathyRelief$UTMy =  coordinates(cord.dec)[,2]



rm(list=setdiff(ls(), c("GPSdf", "WLA","noiseDf", 'p','bathyRelief')))


#########################################################################
# Create a moving animal in the space
#########################################################################

library(SiMRiv)

set.seed(20)

sim.crw = data.frame()

# Make a bunch of whale simulations
for(ii in 1:1){
# Simulate call source levels and times
MinTime =  as.POSIXct(min(GPSdf$UTCDatehour), tz = 'UTC')+hours(12)
MaxTime =   as.POSIXct(max(GPSdf$UTCDatehour), tz = 'UTC')-hours(24)

# Create the random movement and center it
c.rand.walker <- species(state.CRW(.99)+15)
sim.set <- as.data.frame(simulate(c.rand.walker, 10000))

sim.set$UTMx=sim.set$V2+runif(1, min(GPSdf$UTMx), max(WLA$UTMx))
sim.set$UTMy=sim.set$V1+runif(1,min(WLA$UTMy-20000) ,max(WLA$UTMy+2000))
sim.set$UTC = seq(MinTime, MaxTime, length.out = nrow(sim.set))

sim.set$WhaleId= ii
sim.crw=rbind(sim.crw, sim.set)

}

# Show the GPS tracks, whale, and exclusion area
#p = p+geom_path(data= sim.crw, aes(x=UTMx, y=UTMy, group=whaleGrid), color='red')


# simulate calling
sim.crw$SL =rbinom(n = nrow(sim.crw),size = 1, prob = .035)*
  rnorm(n = nrow(sim.crw), mean = 177, sd = 4)
sim.crw$SL[sim.crw$SL==0]=NaN

# check the plot
p= p+geom_point(data= subset(sim.crw, !is.na(SL)), aes(x=UTMx, y= UTMy, 
                                                       size = SL, color=UTC))

p
# 
# p +geom_path(data = subset(GPSdf, UTC >= min(sim.crw$UTC) & UTC <= max(sim.crw$UTC)),
#              aes(x = UTMx, y=UTMy, group= DriftName), color= 'cadetblue4',
#              size =2)

# Add lat/lon
# Add UTC coords with the sp package
cord.dec = SpatialPoints(cbind(sim.crw$UTMx,sim.crw$UTMy),
                         proj4string=CRS("+init=epsg:32610"))


cord.dec = spTransform(cord.dec, CRS("+proj=longlat"))
sim.crw$Lon =  coordinates(cord.dec)[,1]
sim.crw$Lat =  coordinates(cord.dec)[,2]


rm(list=setdiff(ls(), c("GPSdf", "WLA","noiseDf", 'p', 'sim.crw',
                        'alpha', 'TL','bathyRelief')))

############################################################################
# TL functions
############################################################################


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
SL_max=188
SL_min = 165
NL=80
SNRthresh=2
h=20


# Transmission loss
alpha = AcousticAbsorption(200)
TL<- function(r, h=100, alpha=0){
  (15*log10(r)+10*log10(h/2)+(alpha/1000)*r)}
#########################################################################
# Haversine fx
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

###########################################################################
# Calculate the recieved levels on each hydrophone


# clear out the data without source levels 
sim.calls=sim.crw[!is.na(sim.crw$SL),]
sim.calls$inWLA = point.in.polygon(sim.calls$UTMx, sim.calls$UTMy,
                                   WLA$UTMx, WLA$UTMy)
sim.calls$propAreainWLA = NaN


PositionError = 1152/1500 # 80th percentile of the GPS error
SSPError = (1500*.2)/1500 # 20% error in speed of sound
TDOA_error = SSPError+PositionError

figs =list()
sim.calls$Blobn
sim.calls$Blob
sim.calls$Correct =FALSE
sim.calls$Color='green'


#Basemap
p<-ggplot()+
  geom_tile(data= bathyRelief, aes(x = Long, y=Lat, fill= depth))+
  geom_path(data= WLA, aes(x= Lon, y=Lat), color = 'gold',linewidth=2)+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), color ='black')+
  theme_bw()+
  xlab('Longitude')+ylab('Latitude')+
  xlim(c(-122.25, -121.45))
  
  

# Step through each call, determine acceptable SNR and TDOA ranges
for(ii in 1:nrow(sim.calls)){
  # Figure out spatial error
  
  
  # Preallocate the grid
  # Create 8 grids to show where the whale might be
  whaleGrid = expand.grid(
    UTMx= seq(min(WLA$UTMx)-5000, max(WLA$UTMx)+5000, by=1000),
    UTMy= seq(min(GPSdf$UTMy)-5000, max(WLA$UTMy)+5000, by=1000),
    DriftName = unique(GPSdf$DriftName)
  )
  
  # Add lat/lon
  # Add UTC coords with the sp package
  cord.dec = SpatialPoints(cbind(whaleGrid$UTMx,whaleGrid$UTMy),
                           proj4string=CRS("+init=epsg:32610"))
  
  
  cord.dec = spTransform(cord.dec, CRS("+proj=longlat"))
  whaleGrid$Lon =  coordinates(cord.dec)[,1]
  whaleGrid$Lat =  coordinates(cord.dec)[,2]
  
  # whaleGrid$r=0
  whaleGrid$rElipsoid=NaN
  
  DriftNames = unique(noiseDf$DriftName)
  # Recieved level dataframe
  RLdf = data.frame(DriftName = as.factor(DriftNames))
  RLdf$UTMx=0
  RLdf$UTMy=0
  
  for(drift in DriftNames){
    GPSsub = subset(GPSdf, DriftName == drift)
    NLsub =  subset(noiseDf, DriftName==drift)
    
    
    # Functions to estimate lat, long, and NL
    UTMfxx <- approxfun(GPSsub$UTC, GPSsub$UTMx)
    UTMfxy <- approxfun(GPSsub$UTC, GPSsub$UTMy)
    
    UTMflon <- approxfun(GPSsub$UTC, GPSsub$Longitude)
    UTMflat <- approxfun(GPSsub$UTC, GPSsub$Latitude)
    
    
    NL_FUN <-  approxfun(NLsub$datetime_posix, NLsub$TOL_500)
    #knotsFx <- approxfun(GPSsub$UTC, GPSsub$knots)
    ##############################################################
    # Calculate the range from the whale to the GPS, TDOA and RL
    ############################################################
    # Lat/lon of the drift when the call was produced
    RLdf$UTMx[RLdf$DriftName==drift] =UTMfxx(sim.calls$UTC[ii])
    RLdf$UTMy[RLdf$DriftName==drift] = UTMfxy(sim.calls$UTC[ii])
  
    RLdf$Lon[RLdf$DriftName==drift] =UTMflon(sim.calls$UTC[ii])
    RLdf$Lat[RLdf$DriftName==drift] = UTMflat(sim.calls$UTC[ii])
    
    
    RLdf$NoiseLevel[RLdf$DriftName==drift] = NL_FUN(sim.calls$UTC[ii])
    
    
    # Spatial Uncertainty, GPS- estimate the speed at a the call arrival time
    # then how far it could drift
    idxMindiff = which.min(abs((sim.calls$UTC[ii])- (GPSsub$UTC)))
    Time_drift = seconds(GPSsub$UTC[idxMindiff]-seconds(sim.calls$UTC[ii]))
    # Include FFT resolution and soundspeed resolution
    #PosUncertsigma = 0.0067^2 +.003^2 + .25^2; % seconds see EM Nosal 20- R position, ssp, arrival time
    
    # Likely an overestimate of the error
    #distTravelled = (1852*knotsFx(sim.calls$UTC[ii]+Time_drift))*abs(Time_drift)
    #RLdf$TDOAError[RLdf$DriftName==drift]=as.numeric(distTravelled)/1500+TDOA_error
  
    # # Determine the range between drifter and all grid points
    # whaleGrid$r[whaleGrid$DriftName==drift] = sqrt(
    #   (whaleGrid$UTMx[whaleGrid$DriftName==drift]-
    #      RLdf$UTMx[RLdf$DriftName==drift])^2+
    #     (whaleGrid$UTMy[whaleGrid$DriftName==drift]-
    #        RLdf$UTMy[RLdf$DriftName==drift])^2)
    
    whaleGrid$rElipsoid[whaleGrid$DriftName==drift] = 
      haversine_dist(whaleGrid$Lat[whaleGrid$DriftName==drift],
                         whaleGrid$Lon[whaleGrid$DriftName==drift],
                         RLdf$Lat[RLdf$DriftName==drift], RLdf$Lon[RLdf$DriftName==drift])
  }
  
  # Range from the whale to the hydrophone in kms  (truth)
  #RLdf$rangeKm= sqrt((sim.calls$UTMx[ii]- RLdf$UTMx)^2+
  #                     (sim.calls$UTMy[ii]- RLdf$UTMy)^2)/1000
  
  RLdf$range_havers = haversine_dist(sim.calls$Lat[ii],
                                       sim.calls$Lon[ii],
                                       RLdf$Lat, RLdf$Lon)
  
  
  #RLdf$ArrialTime =  sim.calls$UTC[ii]+(RLdf$rangeKm/1.5)
  RLdf$ArrialTimeHavers =  sim.calls$UTC[ii]+(RLdf$range_havers/1500)
  
  #RLdf$SNR=       sim.calls$SL[ii]-TL(RLdf$rangeKm*1000)-RLdf$NoiseLevel
  RLdf$SNRHavers= sim.calls$SL[ii]-TL(RLdf$range_havers)-RLdf$NoiseLevel
  
  # Call detected or not
  RLdf$Detected = ifelse(RLdf$SNRHavers>12,1,0)
  
  ## estimate the drifter location from the available data
  #whaleGrid= merge(whaleGrid, RLdf[,c('ArrialTime','SNR', 'DriftName', 
  #                                    'NoiseLevel')], 
  #                  by.y='DriftName')
 
  # estimate the drifter location from the available data
  whaleGrid= merge(whaleGrid, RLdf[,c('ArrialTimeHavers','DriftName', 
                                      'SNRHavers', 'NoiseLevel', 'Detected')], 
                   by.y='DriftName')
  
  # The expected SNR grids
  #whaleGrid$ExpectedSNR = 177-TL(whaleGrid$r)-whaleGrid$NoiseLevel
  whaleGrid$ExpectedSNR_havers = 177-TL(whaleGrid$rElipsoid)-
    whaleGrid$NoiseLevel
  
  
  # which points matched the arrival SNR
  #whaleGrid$SNRok=ifelse(whaleGrid$ExpectedSNR+11>=whaleGrid$SNR &
  #                          whaleGrid$ExpectedSNR-12 <= whaleGrid$SNR, 1,0)
  
  whaleGrid$SNRok_havers=
    ifelse(whaleGrid$ExpectedSNR_havers+11  >= whaleGrid$SNRHavers &
          (whaleGrid$ExpectedSNR_havers-12) <= whaleGrid$SNRHavers, 1,0)
  
  
  #Deal with non-detected calls
  if(any(RLdf$Detected==0)){
  idxNotDetected = which(whaleGrid$Detected==0)
  whaleGrid$SNRHavers[idxNotDetected]=
    ifelse(whaleGrid$SNRHavers[idxNotDetected]>12,0,1)
  }
  
  whaleGrid$cellId =paste0(whaleGrid$UTMx, whaleGrid$UTMy)
  
  # Count up the cells that were OK by all buoys
  aggData = aggregate(whaleGrid, SNRok_havers~cellId,FUN=sum)
  colnames(aggData)[2]<-'Count'
  
  # Which grid locs were 'ok' by all receivers?
  whaleGrid = merge(whaleGrid, aggData, by = 'cellId', all.x=TRUE)
  
  RLDFDetected = subset(RLdf, Detected==1)
  
  # If at least some of the points fit the TDOA range, then use TDOA to further
  # trim
  if(max(whaleGrid$Count)==length(DriftNames)){
    
    # Pull out the cells that have already been 'okayed' by the SNR process
    accepted_locs = subset(whaleGrid, Count >= length(DriftNames))
    
    # # Plot
    # P1 =p+geom_point(data=accepted_locs, aes(UTMx, UTMy))+
    #   geom_point(data =sim.calls[ii,], aes(x= UTMx, y=UTMy),
    #              color='red', size=3)
    
    accepted_locs$TDOAok = FALSE
    RLDFDetected$SecondsDiff = max(RLDFDetected$ArrialTimeHavers)-
      RLDFDetected$ArrialTimeHavers
    
    ########################################################
    # Create n-1 tdoa trids, nix data outside the expected TDOA values
    ########################################################
    combinations = combn(RLDFDetected$DriftName, 2,simplify = TRUE)
    
    for(kk in 1:ncol(combinations)){
      
      # Pull out the grids for each drift
      grid1 = subset(accepted_locs, DriftName==combinations[1, kk])
      grid2 = subset(accepted_locs, DriftName==combinations[2, kk])
      
      # Caluclate the expected TDOA
      grid1$TDOA = (grid1$rElipsoid-grid2$rElipsoid)/1500
      
      #Observed TDOA between the pairs
      OBS_tdoa = RLdf$ArrialTimeHavers[RLdf$DriftName==combinations[1,kk]]-
        RLdf$ArrialTimeHavers[RLdf$DriftName==combinations[2,kk]]
      
      # Pick out the locations that are OK within the TDOA grid
      grid1$ok =(grid1$TDOA>(OBS_tdoa-TDOA_error) & 
                   grid1$TDOA<=(OBS_tdoa+TDOA_error))
      
      # Trim the accepted locations
      accepted_locs= subset(accepted_locs, 
                             cellId %in% grid1$cellId[grid1$ok==TRUE])
      
    }
    
    
          # Determine if the call was in the receiver by looking at the proportion of
    # the grid in vs out
    sim.calls$propAreainWLA[ii] =
      sum(point.in.polygon(accepted_locs$UTMx,accepted_locs$UTMy,
                           WLA$UTMx, WLA$UTMy))/nrow(accepted_locs)
 
    
   }else{
    sim.calls$propAreainWLA[ii]=0
  }
  
  
  # Determine if the call was in the receiver by looking at the proportion of 
  # the grid in vs out
  sim.calls$propAreainWLA[ii] =sum(point.in.polygon(accepted_locs$UTMx, 
                                                    accepted_locs$UTMy,
                                                    WLA$UTMx, WLA$UTMy))/nrow(accepted_locs)
 
  sim.calls$Correct[ii] =round(sim.calls$propAreainWLA[ii])==sim.calls$inWLA[ii]
  if(!sim.calls$Correct[ii]){
    sim.calls$Color[ii] = 'red'
  }
  # figs[[ii]]=
  #   ggplot()+
  #   geom_point(data = accepted_locs, aes(x=UTMx, y=UTMy), color = 'gray90')+
  #   geom_path(data = GPSdf[GPSdf$UTC<= sim.calls$UTC[ii],], 
  #             aes(x=UTMx, y=UTMy, group = DriftName), color='black')+
  #   geom_point(data = RLdf, 
  #              aes(x=UTMx, y=UTMy, group = DriftName), color='green')+
  #   geom_path(data= sim.crw[sim.crw$UTC<=sim.calls$UTC[ii],], 
  #             aes(x=UTMx, y=UTMy), color='red')+
  #   geom_point(data=sim.calls[ii,], 
  #              aes(UTMx, UTMy, color=as.factor(Correct)), size=3)+
  #   geom_path(data= WLA, aes(x= UTMx, y=UTMy), color = 'gold')+
  #   xlim(c(560000, 640000))+
  #   ylim(c(3900000, 3960000))+
  #   theme_bw()
    
    
  figs[[ii]]=p+
    geom_point(data = accepted_locs, aes(x=Lon, y=Lat), color = 'gray60')+
    geom_path(data = GPSdf[GPSdf$UTC<= sim.calls$UTC[ii],], 
              aes(x=Longitude, y=Latitude, group = DriftName), color='black')+
    geom_point(data = RLdf, 
               aes(x=Lon, y=Lat, group = DriftName), color='darkorange')+
    geom_path(data= sim.crw[sim.crw$UTC<=sim.calls$UTC[ii],], 
              aes(x=Lon, y=Lat), color='red')+
    geom_point(data=sim.calls[ii,], 
               aes(Lon, Lat), size=3, color=sim.calls$Color[ii])+
    theme_bw()
  
  
  
  
  print(ii)
}

# See how we did - proportion of correct guesses
sim.calls$Correct = round(sim.calls$propAreainWLA)==sim.calls$inWLA

aa = sim.calls[!is.na(sim.calls$propAreainWLA),]
sum(aa$Correct)/sum(!is.nan(sim.calls$propAreainWLA))
rownames(sim.calls)=as.character(1:nrow(sim.calls))


evaluatedSub = subset(sim.calls, !is.na(Correct))
# Proportion of calls inside exclusion zone correctly identified
sum(evaluatedSub$Correct[evaluatedSub$inWLA==1], na.rm = TRUE)/sum(evaluatedSub$inWLA)
sum(evaluatedSub$Correct[evaluatedSub$inWLA==0], na.rm = TRUE)/sum(evaluatedSub$inWLA==0)



 p+
  geom_path(data = GPSdf, 
            aes(x=Longitude, y=Latitude, group = DriftName), color='black')+
  geom_path(data= sim.crw, 
            aes(x=Lon, y=Lat), color='red')+
  geom_point(data=sim.calls, 
             aes(Lon, Lat, color= Correct), size=3)+
  xlim(c(-122.5,-121.25))+
  ylim(c(35, 36))+ 
   theme_bw()
   
 
 #############################################################
 # Get ancillary data
 ############################################################

 library(PAMpal)
 colnames(sim.calls)[c(9,10)]<-c('Latatitude', 'Lonitude')
 sim.calls=cbind(sim.calls, matchEnvData(sim.calls))
 
 
 
#############################################################
# Turn into gif
############################################################
# 
# 
# # Use the extents to download the GEBCo data from here:https://download.gebco.net/ 
# nc_data <- nc_open('F:\\GPS_CSV-20230923T045356Z-001\\gebco_2023_n36.0791_s34.6025_w-122.8052_e-120.8013.nc')
# lon <- ncvar_get(nc_data, "lon")
# lat <- ncvar_get(nc_data, "lat", verbose = F)
# elevation <- ncvar_get(nc_data, "elevation")
# bathyRelief = data.frame(Lat=sort(x=rep(lat, length(lon))),
#                          Long=rep(lon, length(lat)),
#                          depth=c((elevation)))
# 
# # Add UTC coords with the sp package
# cord.dec = SpatialPoints(cbind(bathyRelief$Long,bathyRelief$Lat),
#                          proj4string=CRS("+proj=longlat"))
# cord.dec = spTransform(cord.dec, CRS("+init=epsg:32610"))
# bathyRelief$UTMx =  coordinates(cord.dec)[,1]
# bathyRelief$UTMy =  coordinates(cord.dec)[,2]
# 
# ggplot(bathyRelief)+geom_tile(aes(x = UTMx, y=UTMy, fill= depth))
# ggplot(bathyRelief)+geom_tile(aes(x = Long, y=Lat, fill= depth))
# 
# 
# library(gganimate)
# # Create an animation from the list of ggplots
# animation <- plot_list(figs)
# 
# # Specify the filename and other parameters
# anim_save("your_animation.gif", animation, renderer = gifski_renderer())
# 
# 
# 
# 
# #############################################################
