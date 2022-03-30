#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Hand Points
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 3/30/2022
#Purpose: Estimate point values of HAND based on 20 x 20 m DEM
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
hand<-raster(paste0(spatial_dir,"III_Products/hand.tif"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create Points--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Resample raster to 20 m DEM
hand_20m<-raster::aggregate(hand, 19.68324)

#Convert raster to xyz points
hand_pnt<-rasterToPoints(hand_20m) %>% as_tibble()

#export pnts
write_csv(hand_pnt, "docs/hand_pnt.csv")


