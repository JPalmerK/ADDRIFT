
library(marmap)
# this code plots the 

# load the humpback data that Taiki's code injested
hmpData_all <- readRDS(here('data/humpback/humpbackDataFinal.rds'))

# Create names for the morro bay adrift numbers then pull them out
# this should be combined in the filter or subset functions above
completedDrifts <- paste0('ADRIFT_', sprintf("%03d", 46:53))
MorroBayHMP = subset(hmpData_all, DriftName %in% completedDrifts,
                     species == 'humpback')

# GPS data
MorroBayGps = subset(gps, DriftName %in% completedDrifts)

# Create the binning in Taiki's formatting
MorroBayHMPBinnedSng = formatBinnedPresence(mutate(MorroBayHMP, call='song'),
                     effort=effortHourly_adrift, 
                     bin='hour', 
                     gps=MorroBayGps)

# Not sure why there are na values outstanding, but clean them out
MorroBayHMPBinnedSng = subset(MorroBayHMPBinnedSng, !is.na(species))

# Giggle test envything looks alright
ggplot()+
  geom_path(data = MorroBayGps, aes(x=Longitude, y=Latitude, 
                                    group= DriftName))+
  geom_point(data= MorroBayHMPBinnedSng, aes(x= Longitude,
                                             y=Latitude, color = UTC))+
  ggtitle('Humpback Song')



# Get the depth variable from NOAA
library(marmap)
bathData = getNOAA.bathy(lon1= max(MorroBayGps$Longitude)+.1,
                         lon2=min(MorroBayGps$Longitude)-.1,
                         lat1=max(MorroBayGps$Latitude)+.1,
                         lat2=min(MorroBayGps$Latitude)-.1,
                         resolution=.2)



#Create color palettes 
blues<-c("royalblue4","royalblue3",
         "royalblue2","royalblue1")
greys<-c(grey(0.8),grey(0.93),grey(0.99))


# Create grid to plot the bathymetry
gridArea = expand.grid(Lon = 
                         seq(
                           min(MorroBayGps$Longitude)-.1,
                           max(MorroBayGps$Longitude)+.1, length.out =50),
                       Lat =
                         seq(
                           min(MorroBayGps$Latitude)-.1,
                           max(MorroBayGps$Latitude)+.1, length.out =150))


# Calculate depth for each grid point using tidy- I don't actually think this
# is easier to read than the for loop I had in there before but.. eh
gridArea <- gridArea %>%
  mutate(
    latIdx = map_dbl(Lat, ~ which.min(abs(.x - as.numeric(unlist(dimnames(bathData)[2]))))),
    lonIdx = map_dbl(Lon, ~ which.min(abs(.x - as.numeric(unlist(dimnames(bathData)[1]))))),
    depth = map2_dbl(lonIdx, latIdx, ~ bathData[.x, .y])
  )

# Create the proper plot
ggplot()+
  geom_tile(data = gridArea, aes(x=Lon, y=Lat, fill = depth))+
  scale_fill_gradientn(values = scales::rescale(c(-4000, -1000, -400,300, 2000)),
                       colors = c("midnightblue", "royalblue3", 
                                  "grey40","grey50", "grey80"),
                       name="Depth (m)")+
  geom_path(data = MorroBayGps, aes(x=Longitude, y=Latitude, 
                                    group= DriftName))+
  geom_point(data= MorroBayHMPBinnedSng, aes(x= Longitude,
                                             y=Latitude, color = UTC))+
  scale_color_viridis_c()+
  theme_bw()
  ggtitle('Humpback Song')
  
  
# Save
  savedir = getwd()
  ggsave(paste(savedir,'HmpSngMorroBay_Map.png',sep=""),
         width=6,height=6,units="in",dpi=300,device='png')