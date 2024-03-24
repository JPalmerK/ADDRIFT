
rm(list=ls())
library(sf)
library(PAMpal)
library(ggplot2)
library("RSQLite")
#library(marmap)
library(ggplot2)
library(zoo) # rolling mean
library(rgdal) # spatial transformations
library(PAMpal)
library(PAMmisc)


# Load the location dataframe
GPS_track = read.csv('E:/GPS/ADRIFT_018_GPS.csv')
GPS_track$UTCPosit =as.POSIXct(GPS_track$UTC, tz= 'UTC')

# Load the noise from the database
## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname="E:/DATA/2022-12-05_GPL_HB_Gray_Run_CorrectTimes/DATABASES/GPL_HB_Gray_PG2_02_02_ADRIFT_018.sqlite3")

annotations = dbGetQuery(con, 'SELECT * FROM Spectrogram_Annotation')
annotations$UTC =as.POSIXct(annotations$UTC ,tz = 'UTC') 
aa = dbGetQuery(con, 'SELECT * FROM Noise_Monitor')
aa=rbind(aa)

aa$UTC<-as.POSIXct(aa$UTC,tz = 'UTC')
fLat<-approxfun(GPS_track$UTCPosit, GPS_track$Latitude) #Interpolation function for latitudes
fLong<-approxfun(GPS_track$UTCPosit, GPS_track$Longitude) #Interpolation function for longitudes
aa$Lat = fLat(aa$UTC) 
aa$Long<-fLong(aa$UTC)

# Pull out the data for plotting
dataSub = aa[,c('Lat', 'Long', 'UTC', 'ThirdOctave_44_56_median')]
dataSub$NormNL = scale(dataSub$ThirdOctave_44_56_median)


# Clear out data without lat/lon 
dataSub = dataSub[!is.na(dataSub$Lat),]

# Turn into segments
dataWhaver = dataSub[(1:(nrow(dataSub)-1)), 
                     c('Lat','Long', 'ThirdOctave_44_56_median','UTC')]
dataWhaver$EndLat = dataSub$Lat[c(2:nrow(dataSub))]
dataWhaver$EndLon = dataSub$Long[c(2:nrow(dataSub))]
dataWhaver$id = 1:nrow(dataWhaver)
dataWhaver$NL = dataSub$ThirdOctave_44_56_median[1:nrow(dataSub)-1]
dataWhaver$NLnorm = (dataWhaver$NL-min(dataWhaver$NL))/
  (max(dataWhaver$NL-min(dataWhaver$NL)))*5


ggplot(dataWhaver) +
  geom_segment(
    mapping = aes(x=Long, y=Lat, 
                  xend=EndLon, yend=EndLat,
                  color = id,
                  size=NLnorm), 
    inherit.aes = FALSE
  )



# Back of the envelope calculation of detection range
thresh = 2 # 2 db above noise to be detected
SL = 160 # source level
r =rollmean((10^((thresh+SL- dataWhaver$NL)/18)/1000),20) # in km


# Convert to UTC
cord.dec = SpatialPoints(cbind(dataWhaver$Long, dataWhaver$Lat),
                         proj4string = CRS("+proj=longlat"))

# Setting existing coordinate as lat-long system
#cord.dec = SpatialPoints(cbind(data$Long, data$Lat), proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3857"))
cord.UTM

DF <- as.data.frame(cord.UTM)
colnames(DF)[1:2]<-c('x', 'y')
DF$Xmin = DF$x-(1000*r)
DF$Xmax = DF$x+(1000*r)
DF$UTC = dataWhaver$UTC
attr(DF$UTC, "tzone") <- "UTC"
DF$Valitated= 'No'

# Test fig
ggplot(DF) +
  geom_ribbon(aes(x=y, y=x,
                  ymin = Xmin, 
                  ymax = Xmax + 1), fill = "grey70") +
  geom_line(aes(x=y, y=x))+
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

####################################################################  
## Get the effort data from the binary files
####################################################################
pps <- PAMpalSettings(db = 'E:\\DATA\\2022-12-05_GPL_HB_Gray_Run_CorrectTimes\\DATABASES\\GPL_HB_Gray_PG2_02_02_ADRIFT_018.sqlite3',
                      binaries = 'E:\\DATA\\2022-12-05_GPL_HB_Gray_Run_CorrectTimes\\BINARY\\ADRIFT_018\\',
                      # these parameters are only for the click detector - can ignroe
                      sr_hz='auto',
                      filterfrom_khz=0,
                      filterto_khz=NULL,
                      winLen_sec=.0025)
data <- processPgDetections(pps, mode='db', id='Humpback007') # to read in events
gplDf <- getGPLData(data)

# Now we can add the wav files to this data. You might get a warning about
# "startSample", its safe to ignore that.
data <- addRecordings(data, 
                      folder='E:\\RECORDINGS\\ADRIFT_018_CENSOR_12kHz\\')
# that data is stored here as a dataframe. Has "start" & "end" as POSIXct and
# the fulle path to the file as "file"
wavDf <- files(data)$recordings


# figure out which file the first detections were in
fileStart = which(wavDf$start<= gplDf$UTC[1] & wavDf$end>= gplDf$UTC[1])

validatedWavs = wavDf[seq(fileStart, nrow(wavDf), by=5),]

# Figure out which sections were validated
for(ii in 1:nrow(validatedWavs)){
  
  # index of the times that fall in the wave file
  idx = which(as.numeric(DF$UTC)>= as.numeric(validatedWavs$start[ii]) &
          as.numeric(DF$UTC<= as.numeric(validatedWavs$end[ii])))
  
    DF$Valitated[idx]='yes' 

  
}

annotations$Stop = annotations$UTC+ annotations$Duration
attr(annotations$Stop, "tzone") <- "UTC"

# keep only humpback
annotations = annotations[annotations$Species != 'Unknown     ',]


# add annotations
for (ii in 1:nrow(annotations)){
  # index of the times that fall in the wave file
  idx = which(as.numeric(DF$UTC) <= as.numeric(annotations$Stop[ii]) &
                as.numeric(DF$UTC)>= as.numeric(annotations$UTC[ii]))
  
  spp = annotations$Species[ii]
  
  DF$Valitated[idx] = 'Hump'
}


# Reformat for nicer plotting
library(data.table)


DF$group <- rleid(DF$Valitated)
df_plot <- head(do.call(rbind, by(DF, DF$group, rbind, NA)), -1)
df_plot[,c("group","Valitated")] <- lapply(df_plot[,c("group","Valitated")], na.locf)
df_plot[] <- lapply(df_plot, na.locf, fromLast = TRUE)


# Test fig
ggplot(df_plot) +
  geom_ribbon(aes(x=y, y=x,
                  ymin = Xmin, 
                  ymax = Xmax,
                  fill=Valitated,
                  group = group)) +
  scale_fill_discrete()+
  geom_line(aes(x=y, y=x))+
  
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")






####################################################################  
## Get bathymethry data
####################################################################


library(ncdf4) # package for netcdf manipulation
library(ggnewscale)

# Use the extents to download the GEBCo data from here:https://download.gebco.net/ 
nc_data <- nc_open('C:\\Users\\Kaitlin Palmer\\Downloads\\GEBCO_11_Jan_2023_e12bb5dd2650\\GEBCO_11_Jan_2023_e12bb5dd2650\\gebco_2022_n42.0_s40.0_w-125.0_e-124.0.nc')
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
elevation <- ncvar_get(nc_data, "elevation")

outDatafor3mb = data.frame(Lat=sort(x=rep(lat, length(lon))),
                           Long=rep(lon, length(lat)),
                           depth=c((elevation)))


# Convert to UTC
basemp.dec = SpatialPoints(cbind(outDatafor3mb$Long, outDatafor3mb$Lat),
                         proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
cord.UTM <- spTransform(basemp.dec, CRS("+init=epsg:3857"))
cord.UTM

# 
# coordPlotUTM = SpatialPoints(cbind(df_plot$X, df_plot$y),
#                              proj4string = CRS("+init=epsg:3857"))

sputm <- SpatialPoints(df_plot[,c('x', 'y')],
                       proj4string=CRS("+init=epsg:3857"))

# First lat/lon
spgeo <- as.data.frame(spTransform(sputm, CRS("+proj=longlat +datum=WGS84")))


# Second lat lon
spgeo$Xmax <- as.data.frame(
  spTransform(
    SpatialPoints(df_plot[,c('Xmax', 'y')],
                  proj4string=CRS("+init=epsg:3857")),
                       CRS("+proj=longlat +datum=WGS84")))[,1]
spgeo$Xmin <- as.data.frame(
  spTransform(
    SpatialPoints(df_plot[,c('Xmin', 'y')],
                  proj4string=CRS("+init=epsg:3857")),
    CRS("+proj=longlat +datum=WGS84")))[,1]


spgeo$group = df_plot$group
spgeo$Valitated = df_plot$Valitated

#basemapTiles <- as.data.frame(cord.UTM)
basemapTiles <- as.data.frame(basemp.dec)
basemapTiles$depth = c((elevation))
colnames(basemapTiles)[1:2]<-c('x', 'y')
#basemapTiles=basemapTiles[basemapTiles$y<5050000,]
#basemapTiles=basemapTiles[basemapTiles$y<5050000,]

ggplot()+
    geom_raster(data = basemapTiles,
              aes(x=x, y=y,fill= depth))+
  new_scale_fill()+
  geom_raster(data = basemapTiles[basemapTiles$depth>0,],
            aes(x=x, y=y,color= 'gray80'))+
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


###############################################################
# figure out depth along the track
##############################################################

spgeo$depth = 0

for (ii in 1:nrow(spgeo)){
  dist = sqrt((spgeo$x[ii]- basemapTiles$x)^2+(spgeo$y[ii]- basemapTiles$y)^2)
  spgeo$depth[ii]= basemapTiles$depth[which.min(dist)]
  
}


  theme_bw()



library(ggOceanMaps)
library(ggOceanMapsData)

dt <- data.frame(lon = c(-125, -125, -124, -124), 
                 lat = c(40, 42, 42, 40))

basemap(data = dt, bathymetry = TRUE) + 
    geom_polygon(data = transform_coord(dt), 
               aes(x = lon, y = lat),
               color = "red", fill = NA)+
  
  
  basemap(limits = c(-125.5, -123.5, 39, 42), bathymetry = TRUE)+
  geom_line(data = GPS_track, 
            aes(x=Longitude, y=Latitude))

# convert plot data back to decimal degrees

df_plotdecDeg =df_plot
proj4string(df_plotdecDeg) <- CRS("+init=epsg:27700")
head(df_plotdecDeg)

# Convert to UTC
cord.dec = SpatialPoints(cbind(df_plotdecDeg$, dataWhaver$Lat),
                         proj4string = CRS("+proj=longlat"))

# Setting existing coordinate as lat-long system
#cord.dec = SpatialPoints(cbind(data$Long, data$Lat), proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3857"))
cord.UTM








b = getNOAA.bathy(lon1 = -124.9, lon2 = -124.6,
                  lat1 = 40.5, lat2 = 41.1, 
                  resolution = 1)


# convert bathymetry to data frame
bf = fortify.bathy(b)

# get regional polygons
reg = map_data("world2Hires")
reg = subset(reg, region %in% c('Canada', 'USA'))

# convert lat longs
reg$long = (360 - reg$long)*-1

# set map limits
lons = c(-67.5, -63.5)
lats = c(42, 45)

# make plot
ggplot()+
  


library(dplyr); library(ggplot2)
start %>%
  left_join(df[df$condition=="y",], by = "group") %>%
  
  ggplot(aes(x = X.x, y = Y.x,
             xend = X.y, yend = Y.y, group = group)) +
  geom_segment() +
  geom_point() +
  geom_point(data = df, aes(x = X, y = Y, shape = condition), inherit.aes = F)


h=ggplot(dataSub)
h +
  geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
  geom_line(aes(y = level))

# get the data

ibrary(oce)
library(ocedata)
data("coastlineWorldFine")

# convert bathymetry
bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)

# define plotting region
mlon = mean(pts$lon)
mlat = mean(pts$lat)
span = 300

# plot coastline (no projection)
plot(coastlineWorldFine, clon = mlon, clat = mlat, span = span)








# Load and format the lookup tables
pileCenter = st_as_sf(pileCenter,
                      coords = c("longitude", "latitude"),
                      agr = "constant")

st_crs(pileCenter)<-"WGS84"