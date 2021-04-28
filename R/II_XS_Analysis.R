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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Creeat plots --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
