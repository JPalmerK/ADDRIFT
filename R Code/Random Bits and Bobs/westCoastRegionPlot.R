library(ggOceanMaps)
library(mapdata)
library(sf)
library(tidyr)



state <- map_data("state")
state <- state[state$region %in% c('california', 'oregon', 'washington'),]

ggplot()+  geom_polygon(data=state, aes(x=long, y=lat, group=group),
                        color = "black", fill='gray') 


#eez BOUNDARY
#eez boundary: https://www.marineregions.org/gazetteer.php?p=details&id=8456
eez_us=sf::read_sf("C:\\Users\\kaitlin.palmer\\Downloads\\USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp")

eez = read_sf("C:\\Users/kaitlin.palmer/Downloads/eez/eez.shp")
# Filter polygons west of -110 degrees longitude
# Create a bounding box for the area west of -110 degrees longitude
bbox <- st_bbox(c(xmin = -110, xmax = -160, ymin = 0, ymax = 90), 
                crs = st_crs(eez))

# Crop the EEZ polygons to the area west of -110 degrees longitude
eez_west <- st_crop(eez, bbox)

ggplot(eez_west)+geom_sf()


# Basemap with wet coast and bathymetry, state lines and EEZ
p<- basemap(limits = c(-130, -117, 30, 50),
            bathy.style = "rcb", grid.col = NA)+
  guides(fill = FALSE, size = FALSE) #ssl error
  
  p<-ggplot()+
  geom_polygon(data=state, aes(x=long, y=lat, group=group),
               color = "black", fill='gray') +
  geom_sf(data=eez_west,inherit.aes = FALSE)+
  scale_x_continuous(breaks = c(-116, -118, -120, -122, -124, -126, -128),
                      labels = c('-116', '-118', '-120', '-122', '-124',
                                  '-126', '-128')) +
  scale_y_continuous(breaks = (c(30, 35,40,45)),
                       labels = c('30', '35','40','45'))


#########################################################################
# Cut up the regions 
##########################################################################

siteList = read.csv("C:/Users/kaitlin.palmer/Downloads/Deployment Details - Site List.csv")  

siteList<-siteList[1:15,]
# Add latitude limits for each 
siteList$lat_min = NA
siteList$lat_max = NA

siteList$lat_max[1]<-49
siteList$lat_max[2]<-46.2469
siteList$lat_max[3]<-42
siteList$lat_max[4]<-40.74
siteList$lat_max[5]<-40.44
siteList$lat_max[6]<-38.90
siteList$lat_max[7]<-37.77
siteList$lat_max[8]<-37.463
siteList$lat_max[9]<-36.800
siteList$lat_max[10]<-35.36
siteList$lat_max[11]<-34.448
siteList$lat_max[12]<-34.05
siteList$lat_max[13]<-32.7157
siteList$lat_max[14]<-32.7157
siteList$lat_max[15]<-26.044

siteList$lat_min[1:14]<- siteList$lat_max[2:15]

branded_colors <- list(
  "blue"   = "#00798c",
           "red"    = "#d1495b",
           "yellow" = "#edae49",
           "green"  = "#66a182",
           "navy"   = "#2e4057",
           "grey"   = "#8d96a3"
)

ptest = p

for(ii in 1:14){
  latMax = siteList$lat_max[ii]
  latMi = siteList$lat_min[ii]
  
  lonMin = -160; lonMax = -110
  aa = matrix(c(lonMax,lonMin,lonMin,lonMax,lonMax,
                latMi,latMi,latMax,latMax,latMi),,2)
  
  Poly_Coord_df = data.frame(lon = aa[,1],
                             lat = aa[,2])
  
  region <- Poly_Coord_df %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = st_crs(eez_west)) %>% 
    st_bbox() %>% 
    st_as_sfc()
  region = st_intersection(region, eez_west)
  ptest=ptest +geom_sf(data = region, fill= branded_colors[(ii %% 6)+1])
  
}
  
# Create a single polygon from the eez lines
  
  aa = matrix(c(-110,-160,-160,-110,-110, 45,45,50,50,45),,2)
  Poly_Coord_df = data.frame(lon = aa[,1],
                             lat = aa[,2])
                             
  region1 <- Poly_Coord_df %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = st_crs(eez_west)) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  #p+geom_sf(data = region1)
  
  region1 = st_intersection(region1, eez_west)
  p+geom_sf(data = region1, fill= 'green')
  
  
  
  p+
  