#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: DEM Inundate
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/4/2020
#Purpose: Plot mean stage of sipsey river
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load libraries of interest
library(dataRetrieval)
library(tidyverse)

#Set directories of interest
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Sipsey\\spatial_data\\"
workspace_dir <- "data/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Obtain USGS Stage Data ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Download NWIS Data --------------------------------------------------------
#Retreive daily data
df<-readNWISdv(
  siteNumbers = '02446500',
  parameterCd = '00060') #Daily flow

#Retreive sd_curve
sd_curve<-readNWISrating('02446500', "base")

#Get datum data and convert ot meters
datum <- readNWISsite('02446500')$alt_va*0.3048

#2.2 Create stage-discharge relationship----------------------------------------
#Clean up sd curve
sd_curve<-sd_curve %>% 
  dplyr::mutate(
    depth_ft = INDEP,
    depth_m  = depth_ft*0.3048,
    flow_cfs = DEP) %>% 
  dplyr::select(flow_cfs, depth_m)

#Create interpolation function
sd_interp<-approxfun(sd_curve$flow_cfs, sd_curve$depth_m)

#2.3 Estimate streamflow elevation at gage -------------------------------------
#clean up df
df<-df %>% 
  as_tibble() %>% 
  select(
    date = 'Date', 
    q_cfs = 'X_00060_00003')

#Add depth to return interval df
df <- df %>% mutate(stage_m = sd_interp(q_cfs))

#Convert stage to elevation
df <- df %>% mutate(ele_m = stage_m + datum)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plot Data -----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Tidy data
df %>% 
  #Define julian date
  mutate(yday = yday(date)) %>%
  #Summarize median and quantiles
  group_by(yday) %>% 
  summarise(
    stage_q1 = quantile(stage_m, 0.25, na.rm=T),
    stage_q2 = quantile(stage_m, 0.50, na.rm=T),
    stage_q3 = quantile(stage_m, 0.75, na.rm=T)) %>%
  #Smooth time series
  mutate(
    stage_q1 = stats::filter(x=stage_q1, filter = rep(1/10,10), sides=2),
    stage_q2 = stats::filter(x=stage_q2, filter = rep(1/10,10), sides=2),
    stage_q3 = stats::filter(x=stage_q3, filter = rep(1/10,10), sides=2)
  ) %>% 
  #plot
  ggplot() + 
    geom_ribbon(
      aes(x= yday, ymin=stage_q1, ymax=stage_q3),
      fill = '#a6bddb', 
      col='#2b8cbe', lwd=0.5,
      alpha = 0.7)+
    geom_line(
      aes(x = yday, y=stage_q2), 
      lty=1,
      lwd=1,
      col='#2b8cbe'
    )+
    theme_bw() +
    #Change font size of axes
    theme(
      axis.title = element_text(size = 14), 
      axis.text  = element_text(size = 10)
    ) + 
    #Add labels
    xlab('Julian Day') + 
    ylab("Stage [m]") +
    #Zoom in on area of interest
    coord_cartesian(
      xlim = c(20, 340), ylim = c(1.5, 4.05))
  
  
