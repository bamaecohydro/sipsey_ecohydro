#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: XS Analysis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 4/27/2021
#Purpose: Exam inundation patterns and elevation across proposed plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load libraries of interest
library(sf)
library(raster)
library(whitebox)
library(leaflet)
library(stringr)
library(tidyverse)
library(mapview)

#Set directories of interest
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Sipsey\\spatial_data\\"
workspace_dir <- "data/"

#Load dem and inundation rasters
dem<-raster(paste0(spatial_dir,"II_Work\\dem_neon.tif"))
flood<-raster(paste0(spatial_dir,"III_Products/median_inundation_dur.tif"))

#load XS data
xs<-list.files(paste0(spatial_dir,"I_Data/Sipsey transects/"), full.names = T ) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "shp")) %>% 
  pull() %>% 
  lapply(., 
         function(x){
           st_read(x) %>%  
             st_zm() %>% 
             st_coordinates() %>% 
             st_linestring()  %>% 
             st_sfc(., crs=26916) %>% 
             st_sf()}) %>% 
  bind_rows()

#plot for funzies
mapview(dem) + 
  mapview(xs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create sampling plots -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Sample 5 equally placed points along cross section
center_pnts<-st_line_sample(xs, 5) 

#Create box
p<-center_pnts %>% 
  st_coordinates(.) %>% 
  as_tibble() %>% 
  mutate(
    xmax = X+10, 
    xmin = X-10, 
    ymax = Y+10, 
    ymin = Y-10
  ) 

#Function to create sampling plots
fun<-function(n){st_bbox(c(xmin = p$xmin[n], xmax = p$xmax[n], ymax = p$ymax[n], ymin = p$ymin[n]), crs = 26916) %>% st_as_sfc() %>% st_sf()}
p<-lapply(X=seq(1,nrow(p)), FUN=fun) %>% bind_rows()

#plot for funzies
mapview(dem) + 
  mapview(xs) + 
  mapView(p)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Extract info about sampling plots------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate median stats (fow now, we can get crazy later!)
#stat_fun<-function(x) median(x)

#Apply functions
p$ele_m<-raster::extract(dem, p, fun = mean)
p$dur_day<-raster::extract(flood, p, fun = mean)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Compare distributions -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create mask raster to identify flooded areas
mask<-flood
mask[mask==0]<-NA
mask<-mask*0+1

#Extract distribution from DEM and Duration
ele_pnts<-dem*mask
ele_pnts<-rasterToPoints(ele_pnts) 
ele_pnts<-as_tibble(ele_pnts)














#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Create Map ----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project raster
r<-dur
r@crs<-dem@crs
r<-projectRaster(r, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

#Make zero values NA
r[r==0]<-NA
r[r>120]<-NA


#Create Map
#leaflet
m<-leaflet(r) %>% 
  #Add Basemaps
  addProviderTiles("Esri.WorldImagery", group = "ESRI") %>% 
  addTiles(group = "OSM") %>%
  #Add flowpath data
  addRasterImage(
    x=r,
    col=pal,
    opacity = 0.9,
    group = "Inundation Duration") %>%
  #AddLegend
  addLegend(
    title = "Inundation [days/yr]",
    pal = pal, 
    values = values(r)) %>% 
  #Add Layer Control Options
  addLayersControl(
    baseGroups = c("Esri", "OSM"), 
    overlayGroups = c("Inundation Duration"))

#print leaflet
m

#Export widget
saveWidget(m, file="inundation_dur.html")

#Export widget
mapshot(m, url="survey_plots2.html")
