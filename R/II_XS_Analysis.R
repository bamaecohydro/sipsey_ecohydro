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
flood<-raster(paste0(spatial_dir,"III_Products/mean_inundation_dur.tif"))
hand<-raster(paste0(spatial_dir,"III_Products/hand.tif"))

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
#3.0 Characterize distributions of elevationa nd flood duration ----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define  elevation and duration distributions across sampling plots
p$ele_m<-raster::extract(hand, p, fun = mean)
p$dur_day<-raster::extract(flood, p, fun = mean)

#Create mask raster to identify flooded areas
mask<-flood
mask[mask==0]<-NA
mask<-mask*0+1

#Extract distribution from DEM and Duration
ele_pnts<-hand*mask
ele_pnts<-rasterToPoints(ele_pnts) 
ele_pnts<-as_tibble(ele_pnts) %>% filter(layer>-1)
dur_pnts<-rasterToPoints(flood) 
dur_pnts<-as_tibble(dur_pnts) %>% filter(mean_inundation_dur>2)

#Conduct KS test on elevation dist
ks.test(p$ele_m, ele_pnts$layer)

#Conduct KS test on elevation dist
ks.test(p$dur_day, dur_pnts$mean_inundation_dur)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Plots ----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#elevation
ggplot() +
  geom_density(
    aes(ele_pnts$layer),
    bg='#E57200',
    alpha=0.7) +
  geom_density(
    aes(p$ele_m), 
    bg='#232D4B', 
    alpha = 0.7) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  ylab("Probability") + 
  xlab("Elevation [m]")  
  
#duration
ggplot() +
  geom_density(
    aes(dur_pnts$mean_inundation_dur),
    bg='#E57200',
    alpha=0.7) +
  geom_density(
    aes(p$dur_day), 
    bg='#232D4B', 
    alpha = 0.7) +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 14), 
    axis.text  = element_text(size = 10)
  ) + 
  ylab("Probability") + 
  xlab("Inundation Duration [Day]")  



