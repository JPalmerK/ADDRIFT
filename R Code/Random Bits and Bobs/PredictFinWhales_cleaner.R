rm(list=ls())
source('SpatialFxs.R')
library(ggplot2)
library(lubridate)
library(dplyr)
library(sp)
library(ggstar)


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
#####################################################
# Shift the GPS df south by half a degree
#########################################################

GPSdf$Latitude=GPSdf$Latitude-0 # not today Satan!


#######################################################
# Load prediction grids and variables
##########################################################
predGridLoc ='C:\\Data\\Prediction Grids\\Grids_for_Kaitlin'
predGrid = read.csv(paste0(predGridLoc, '\\', 'CCE_0.1deg_2018-07-01.csv'))
denGridd = read.csv('C:\\Data/Prediction Grids/CCE_SDMs_2018_Bp_BiWeekly_Preds.csv')

denGridd$lat = denGridd$mlat
denGridd$lon = denGridd$mlon-360
# Trim the dnesity and the prediction grid to the region of the
# drifts

predGridSub =subset(predGrid, lat<= (max(GPSdf$Latitude+0.5)) & 
                      lat> (min(GPSdf$Latitude-0.5)) &
                      lon >  -122.5 & lon<= -121.5) 
denGridSub = subset(denGridd, mlat<= (max(GPSdf$Latitude+0.5)) &
                      mlat>(min(GPSdf$Latitude-0.5)) &
                      mlon > 237.5 & mlon<= 238.5)

# Plot check
ggplot(predGrid)+
  geom_tile(aes(x= lon, y= lat, fill = ild.mean))

ggplot(denGridSub)+
  geom_tile(aes(x=mlon-360, y = mlat, fill=X74.dens.2018.07.01))+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), color ='yellow')
##########################################################################
# Simulate whales within the array based on density likelihood


# Simulate Source Levels(https://academic.oup.com/icesjms/article/76/1/268/5115402)
# source, level, noise level, frequen(ies), and SNR threshold
f=100:200
h=100
SLmean =190.5-10#subtracting 10 since we are in the wrong noise band
SLstdev = 7.4


# normalize the density
denGridSub$NormDen = rescale(denGridSub$X74.dens.2018.07.01)


# TDOA ERROR in seconds
SNRthresh =15
ssp =1500 # soundspeed, 1500 m/s
PositionError = 1152/ssp # 80th percentile of the GPS error
SSPError = (ssp*.2)/ssp # 20% error in speed of sound
TDOA_error = SSPError+PositionError

# Allowable SNR error
SNR_error = SLstdev*1.5+TL(TDOA_error)

# create simulated calls and locations
sim.calls = data.frame()
for(ii in 1:50){sim.calls = rbind(sim.calls, 
                                   createSimWhales(denGridSub,  f, SLmean, 
                                                   SLstdev*.5, h))}
sim.calls$UTC = sample(seq(min(GPSdf$UTC), 
                           max(GPSdf$UTC), 
                           by="1 mins"), nrow(sim.calls), replace = TRUE)
sim.calls =  sim.calls[,c(1:4)]
sim.calls$ID =1:nrow(sim.calls) # for cross referencing
sim.calls$nSensors =0 # how many sensors the call was detected on
sim.calls$snapshotID =NaN # which time period was it produced in?




###############################################################################
# Step through the time series and calculate the underlying N dete at each location
###############################################################################


# Create a prediction grid with lat, lon, time, drift, and noise
drifterLocs = expand.grid(DriftName =  unique(GPSdf$DriftName),
                          UTC = seq(min(GPSdf$UTC), max(GPSdf$UTC), by= '20 min'))
drifterLocs$Lat=NaN
drifterLocs$Lon =NaN
drifterLocs$NL = NaN

# Fill in the drifter information
# Make the lat/lon noise predictions 
DriftNames = unique(drifterLocs$DriftName)
for(drift in DriftNames){
  
  GPSsub = subset(GPSdf, DriftName == drift)
  NLsub =  subset(noiseDf, DriftName==drift)
  
  UTMflon <- approxfun(GPSsub$UTC, GPSsub$Longitude)
  UTMflat <- approxfun(GPSsub$UTC, GPSsub$Latitude)
  NL_FUN <-  approxfun(NLsub$datetime_posix, NLsub$TOL_500)
  
  drifterLocs$NL[drifterLocs$DriftName==drift] = 
    NL_FUN(drifterLocs$UTC[drifterLocs$DriftName==drift])
  drifterLocs$Lat[drifterLocs$DriftName==drift] = 
    UTMflat(drifterLocs$UTC[drifterLocs$DriftName==drift])
  drifterLocs$Lon[drifterLocs$DriftName==drift] = 
    UTMflon(drifterLocs$UTC[drifterLocs$DriftName==drift])
  
  print(drift)
}

#################################################################


timeSteps = unique(drifterLocs$UTC)
SL = c(SLmean-SLstdev,SLmean+SLstdev)

# Preallocate the lat/lon grid
gridArea = expand.grid(Lat = seq(min(denGridSub$lat),
                                 max(denGridSub$lat), 
                                 length.out =80),
                       Lon = seq(min(denGridSub$lon), 
                                 max(denGridSub$lon), 
                                 length.out =80))
gridArea$cellId =paste0(gridArea$Lat,
                        gridArea$Lon)
gridArea$TDOAandSNROK_sum =0 
gridArea$PdetSum =0



# Create a dataframe that holds all the grid locations
acceptedLocsGrid = gridArea[,c(1:3)]

###########################################################
includedCalls =0 # Number of calls detected

CallDen = matrix(nrow = nrow(gridArea), ncol = length(timeSteps))
nDetectionArea = matrix(nrow = nrow(gridArea), ncol = length(timeSteps))

# For each timestep, figure out the density
for(ii in 1:length(timeSteps)){
  
  # Drifter subset, lat, long time
  drifterLoc = subset(drifterLocs, 
                      UTC == timeSteps[ii] & 
                        !is.na(NL) & 
                        !is.na(Lat)) 
  
  
  # Step through each drift to get the probability of detection
  detectionGrid = rep(0, nrow(gridArea))
  gridArea$Pdet_call = 0
  
  # Determine the detectable area, sum up all of the detection areas
  for(zz in 1:nrow(drifterLoc)){
    
    # This function returs a grid indicating which cells were 'monitored'
    # by an instrument at sensor lat/lon under a give source level range, ambient
    # noise level, and SNR threshold
    dataOut = acceptedSNR_AreaMonitored(SL=SL, 
                                        SNRthresh = SNRthresh, 
                                        NL=drifterLoc$NL[zz],
                                        senLat = drifterLoc$Lat[zz],
                                        senLon = drifterLoc$Lon[zz],
                                        whaleGrid = gridArea) 
    gridArea$Pdet_call= gridArea$Pdet_call+dataOut$Pdet
    
    # Plot check
    #p<-ggplot(gridArea)+geom_tile(aes(y=Lat, x=Lon, fill = Pdet_call))
    
  }
  gridArea$PdetSum = gridArea$PdetSum+ifelse(gridArea$Pdet_call>1,1,0)
  
  # by how many sensors was each cell monitored at each timestep
  nDetectionArea[,ii] = gridArea$Pdet_call
  # Pull out the calls in the snapshot period
  snapShotCalls = subset(sim.calls, 
                         UTC>= timeSteps[ii] &
                           UTC< timeSteps[ii]+minutes(20))
  
  # If there were any detected animals, figure out where they could be.
  if(nrow(snapShotCalls)>1){

    sim.calls$snapshotID[sim.calls$ID %in% snapShotCalls$ID]=ii
    CallOk =matrix(nrow = nrow(gridArea), ncol = nrow(snapShotCalls))

    # Step through each of the calls in the snapshot
    for(jj in 1:nrow(snapShotCalls)){

      ########################################################################
      # Distance and arrival SNR between each of the drifters and the
      # simulated call (truth)
      drifterLoc$r = haversine_dist(snapShotCalls$Lon[jj], snapShotCalls$Lat[jj],
                                    drifterLoc$Lon, drifterLoc$Lat)
      drifterLoc$SNR = snapShotCalls$SL[jj]-TL(drifterLoc$r)-drifterLoc$NL
      drifterLoc$CallArrivalTime = snapShotCalls$UTC[jj]+seconds(drifterLoc$r/ssp)
      drifterLoc$Detected = ifelse(drifterLoc$SNR>SNRthresh,1,0)

      ########################################################################


      # If missed by all instruments, don't bother otherwise use SNR and TDOA
      # to estimate locations

      if(any(drifterLoc$SNR>SNRthresh)){
        
        if(sum(drifterLoc$Detected) == 2){
          print(paste(ii, jj))
        }

        # Count how many calls to normalized the slices, eventually
        includedCalls =includedCalls+1

        # Report back the number of sensors the call was detected on
        sim.calls$nSensors[sim.calls$ID== snapShotCalls$ID[jj]]=
          sum(drifterLoc$Detected)

        #print(paste(ii, jj))


        # Determine the accepted locations based on TDOA and SNR
        whaleGrid = calcWhaleGrid(SL, SNRthresh, drifterLoc, gridArea)

        # Locations where the call could have origniated from
        TDOAandSNROK = ifelse(
          gridArea$cellId %in% whaleGrid$cellId[whaleGrid$TDOA_and_SNRok==1],
          1,0)

        # Indicate which cells were not monitored


        # Add the locations into the accepted grid area
        #acceptedLocsGrid= cbind(acceptedLocsGrid, TDOAandSNROK)

        CallOk[,jj] =TDOAandSNROK
        gridArea$WhaleRegion =TDOAandSNROK

        # drifterLoc$Detected = as.factor(drifterLoc$Detected)
        # ggplot(subset(gridArea, Pdet_call >0))+
        #   geom_tile(aes(x=Lon, y=Lat, fill = Pdet_call), alpha=.8)+
        #   guides(fill='none')+
        #   geom_point(data = subset(drifterLocs, UTC<= timeSteps[ii]),
        #              aes(x=Lon, y=Lat), color = 'black', size =0.5)+
        #   geom_point(data=drifterLoc, aes(x=Lon, y=Lat,
        #                                   color = Detected), size= 2.5)+
        #   scale_color_manual(values=c("brown1", "chartreuse4"))+
        #   theme_bw()+
        #   xlab('Longitude')+ylab('Latitude')+
        #   geom_tile(data= subset(whaleGrid, SNR_accepted>0),
        #             aes(x=Lon, y=Lat), fill = 'pink')+
        #   geom_star(data =snapShotCalls[jj,], aes(x=Lon, y=Lat), size=3,
        #             color = 'black', fill ='yellow')
        #
        #
        ggplot(gridArea, aes(y=Lat, x=Lon))+
          geom_tile(aes(fill = log10(aa)))+
          # geom_tile(aes(fill =aa))
          # geom_tile(data=whaleGrid,
          #           aes(x=Lon, y=Lat, fill = TDOA_and_SNRok))+
          geom_point(data = subset(drifterLocs, UTC<= timeSteps[ii]),
                     aes(x=Lon, y=Lat), color = 'black', size =0.5)+
          geom_point(data=drifterLoc, aes(x=Lon, y=Lat), color ='brown1', size= 2.5)+
          scale_color_manual(values=c("brown1", "chartreuse4"))+
          geom_star(data =snapShotCalls[jj,], aes(x=Lon, y=Lat), size=3,
                    color = 'black', fill ='yellow')+
          theme_bw()+
          xlab('Longitude')+ylab('Latitude')




        # gridArea$TDOAandSNROK_sum= gridArea$TDOAandSNROK_sum+gridArea$TDOAandSNROK
        #

      }

      # This is the density at each timestep, should sum to mean the total number
      # of calls
      CallDen[,ii]=rowSums(sweep(CallOk,MARGIN=2,FUN="/",
                                 STATS=colSums(CallOk)), na.rm = TRUE)

    }

  }

}


###########################################################
# Finalize and plot
 ####################################################
 gridArea$CallDen = rowSums(CallDen, na.rm = TRUE)


 # Number of nan values, higher number means less monitored
 gridArea$g_r =rowSums(nDetectionArea, na.rm = TRUE)/(length(timeSteps)*7)
 gridArea$g_r =rowSums(nDetectionArea^2, na.rm = TRUE)/(length(timeSteps)*7)
 
 gridArea$TotMonitored = rowSums(nDetectionArea, na.rm = TRUE)
 gridArea$NormCallDen = gridArea$CallDen/gridArea$g_r



# Plot the survey effort
ggplot(gridArea)+geom_tile(aes(x=Lon, y=Lat, fill = (CallDen)))+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), color ='yellow')+
  ggtitle('Effort, proportion of survey each cell monitored by >=1 instrument')


# Plot the survey effort
ggplot(gridArea)+geom_tile(aes(x=Lon, y=Lat, fill = CallDen))+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), color ='yellow')+
  ggtitle('Effort, proportion of survey each cell monitored by >=1 instrument')


# Plot the survey effort
ggplot(gridArea)+geom_tile(aes(x=Lon, y=Lat, fill = NormCallDen))+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), color ='yellow')+
  ggtitle('Density/g_r')


###################################################################
# Create a new column in the grid area that has the underlying density
library(MASS)
library(pracma) #2d interpolation
density_data <- kde2d(x = sim.calls$Lon, 
                      y = sim.calls$Lat, n = c(80, 80))


# Create 2d interpolations
# Interpolate density values to 'GridLocations'

  
  gridArea$trueDen <- interp2(
  x = density_data$x,
  y = density_data$y,
  Z = t(density_data$z),
  xp = gridArea$Lon,
  yp = gridArea$Lat)

  
  gridArea$gridDiff_truthNorm = gridArea$trueDen- gridArea$NormCallDen
  gridArea$gridDiff_truthNorm = gridArea$gridDiff_truthNorm-mean(gridArea$gridDiff_truthNorm)
  
  ggplot(gridArea, aes(y=Lat, x=Lon))+
  geom_tile(aes(fill=trueDen))+
    geom_density_2d(data= sim.calls,
                    aes(x=Lon, y=Lat), 
                    color = "green", alpha = 1)+
    ggtitle('True Call Density')+
    theme_bw()



ggplot(gridArea)+
  geom_tile(aes(x=Lon, y=Lat, fill =NormCallDen))+
  scale_fill_viridis_c()+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), 
             color ='yellow', size =0.75)+
  # stat_contour(aes(x=Lon, y=Lat, z =gridArea$NormCallDen, color = ..level..),
  #              color = 'red', bins = 3, size =1,)+
  geom_density_2d(data= sim.calls,
                  aes(x=Lon, y=Lat), 
                  color = "green", alpha = 1)+
  ggtitle('Effort Corrected Density ')

  ggplot(gridArea)+
    geom_tile(aes(x=Lon, y=Lat, fill =gridDiff_truthNorm))+
    scale_fill_viridis_c()
  
  
  


ggplot(gridArea)+
  geom_tile(aes(x=Lon, y=Lat, fill =CallDen))+
  scale_fill_viridis_c()+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), 
             color ='yellow', size =0.75)+
  stat_contour(aes(x=Lon, y=Lat, z =gridArea$CallDen, color = ..level..),
               color = 'red', bins = 3, size =1,)+
  geom_density_2d(data= sim.calls,
                  aes(x=Lon, y=Lat), 
                  color = "green", alpha = 1)+
  ggtitle('Sum Density Grids')



ggplot(gridArea)+
  geom_tile(aes(x=Lon, y=Lat, fill =g_r))+
  scale_fill_viridis_c()+
  geom_contour(aes(x=Lon, y=Lat, z =g_r, color = ..level..),
               color = 'black', breaks = quantile(gridArea$g_r), size =1) +
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), 
             color ='yellow', size =0.75)+
  geom_contour(aes(x=Lon, y=Lat, z =gridArea$g_r, color = ..level..),
               color = 'red', bins = 3, size =1)+
  ggtitle('Survey Effort')

  

#############################################################
# Ok cool, so now we have a grid of probabilities, so lets see what
# the underlying predictive variables are

# Add the prediction grid to the observed whale density using 2d splines or 
# interpolation
library(akima)


gridArea$sst.mean = interp2(
  x = matrix(unique(predGrid$lon)),
  y = matrix(unique(predGrid$lat)), 
  Z =   matrix(predGrid$ssh.mean, nrow= 181, byrow = TRUE),
  xp = gridArea$Lon,
  yp = gridArea$Lat)

gridArea$ssh.mean = interp2(
  x = matrix(unique(predGrid$lon)),
  y = matrix(unique(predGrid$lat)), 
  Z =   matrix(predGrid$ssh.mean, nrow= 181, byrow = TRUE),
  xp = gridArea$Lon,
  yp = gridArea$Lat)


gridArea$ild.mean = interp2(
  x = matrix(unique(predGrid$lon)),
  y = matrix(unique(predGrid$lat)), 
  Z =   matrix(predGrid$ild.mean, nrow= 181, byrow = TRUE),
  xp = gridArea$Lon,
  yp = gridArea$Lat)


gridArea$ild.mean = interp2(
  x = matrix(unique(predGrid$lon)),
  y = matrix(unique(predGrid$lat)), 
  Z =   matrix(predGrid$ild.mean, nrow= 181, byrow = TRUE),
  xp = gridArea$Lon,
  yp = gridArea$Lat)

gridArea$gridDiff_truthNorm = gridArea$trueDen- gridArea$CallDen
gridArea$gridDiff_truthNorm = gridArea$gridDiff_truthNorm-mean(gridArea$gridDiff_truthNorm)

# Raw differences in density and estimated call density, but a model
ggplot(gridArea, aes(y=Lat, x=Lon))+
  geom_tile(aes(fill = gridDiff_truthNorm))+
  scale_fill_gradient2()





##############################################################
# This is what was used in the paper, sort of, counts
###################################################################
library(mgcv)

mod.truth= gam(data=  gridArea, 
               trueDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean),
               #trueDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean)+s(ild.mean),
               family = nb())
plot.gam(mod.truth, pages = 1, se = TRUE, 
         main = 'True Density negative log normal',shade = TRUE)


gridArea$truthPred = (predict.gam(mod.truth, 
                                  newdata = gridArea,
                                  type = "response"))

ggplot(gridArea)+
  geom_tile(aes(x=Lon, y=Lat, fill =truthPred))+
  scale_fill_viridis_c()+
  geom_density_2d(data= sim.calls,
                  aes(x=Lon, y=Lat), 
                  color = "green", alpha = 1)+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), 
             color ='yellow', size =0.75)+
  ggtitle('Truth')


# Simulation Model
mod= gam(data=  gridArea, 
         #NormCallDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean)+s(ild.mean),
         NormCallDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean),
         family =nb())
# plot.gam(mod, pages = 1, se = TRUE,shade = TRUE, 
#          main = 'Predicted Normalized Call Density log Normal')

gridArea$simPred = (predict.gam(mod, 
                                newdata = gridArea,
                                type = "response"))
ggplot(gridArea)+
  geom_tile(aes(x=Lon, y=Lat, fill =simPred))+
  scale_fill_viridis_c()+
  geom_density_2d(data= sim.calls,
                  aes(x=Lon, y=Lat), 
                  color = "green", alpha = 1)+
  geom_point(data= GPSdf, aes(x=Longitude, y= Latitude), 
             color ='yellow', size =0.75)+
  ggtitle('Model Predictions with Truth Contours')
  




gridArea$gridDiff_truthNorm = (gridArea$simPred- gridArea$truthPred)/
  mean((gridArea$simPred- gridArea$truthPred), na.rm = TRUE)

ggplot(gridArea)+
  geom_tile(aes(x=Lon, y=Lat, fill =gridDiff_truthNorm))+
  scale_fill_viridis_c()
  scale_fill_gradient2()








#########################################################################
####################################################################


# Not awful...
mod= gam(data=  gridArea, CallDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean),
         family = tw())
plot.gam(mod, pages = 1, se = TRUE,shade = TRUE, 
         main = 'Predicted  Call Density Twedie')


# Not awful...
mod= gam(data=  gridArea, CallDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean),
         family = tw())
plot.gam(mod, pages = 1, se = TRUE,shade = TRUE, 
         main = 'Predicted  Call Density Twedie')


# Not awful...
mod= gam(data=  gridArea, 
         NormCallDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean))
plot.gam(mod, pages = 1, se = TRUE,shade = TRUE, 
         main = 'Predicted Normalized Call Density Twedie')



# This is what was used in the paper, sort of, counts
mod= gam(data=  gridArea, trueDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean))
plot.gam(mod, pages = 1, se = TRUE, 
         main = 'True Density Normal',shade = TRUE)


mod= gam(data=  gridArea, trueDen~ te(Lon,Lat)+s(sst.mean)+s(ssh.mean),
         family = Gamma(link = "inverse"))
plot.gam(mod, pages = 1, se = TRUE, 
         main = 'True Density Gamma',shade = TRUE)



ggplot(gridArea)+
  geom_point(aes(x=ssh.mean, y= trueDen))
########################################################################
# Evaluation
########################################################################
correlation <- cor(gridArea$NormCallDen, gridArea$trueDen)

ggplot(sim.calls, aes(x = Lon, y = Lat)) + 
  geom_point() + 
  stat_density2d(aes(fill=..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradient2(low = "white", high = "red")

