#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: AGU Plots
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 12/8/2021
#Purpose: Develop plots for Matt's AGU poster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To do list
# Convert plots to water year
# Add this years data from the Sipsey

# Email to matt: 
# 3b Inundation. I would mirror the hydrograph plot, but instead show "modeled inundation" on the y-axis. This could be calculated as "% of max area" or "average width"
# 
# 3c Slough hydrographs. Don't worry about labeling sloughs here....however consider making colors based on your connectivity metric (i.e., higher inundation duration == deeper blue). Also, consider overlaying the "median" slough water level too.
# 
# 3d Slough stage-Discharge plots. Combine all the plots here. My suggestion would be to add all the data to these plots. For each slough, add a moving average line (checkout geom_smooth in ggplot) and then add your points with a little bit of alpha.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load libraries of interest
library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(whitebox)
library(dataRetrieval)
library(patchwork)
library(zoo)

#Set directories of interest
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Sipsey\\spatial_data\\"
workspace_dir <- "data/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Sipsey Hydrograph----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Sipsey Gage Data
df<-readNWISdv(siteNumbers = '02446500', 
               parameterCd = '00060')

#Create annual time series
df<-df %>% 
  #Tidy Data
  select(date = Date, 
         flow = X_00060_00003) %>% 
  mutate(date = ymd(date)) %>% 
  #Estimate daily stats
  mutate(yday=yday(date)) %>% 
  filter(yday!=366) %>% 
  group_by(yday) %>% 
  summarise(
    upr = quantile(flow, 0.75),
    med = quantile(flow, 0.5), 
    lwr = quantile(flow, 0.25)) %>% 
  ungroup() %>% 
  mutate(date = strptime(yday, format='%j'), 
         date = date(date))

#Apply moving window average to smooth things out a bit
df<-df %>% 
  mutate(
    upr = rollapply(upr, 30, mean, fill=NA),
    med = rollapply(med, 30, mean, fill=NA),
    lwr = rollapply(lwr, 30, mean, fill=NA))

#Add this years hydrograph
df_year<-
  #Download Data
  readNWISdv(
    siteNumbers = '02446500', 
    parameterCd = '00060', 
    startDate = "2021-01-15") %>% 
  #Tidy Data
  select(
    date = Date, 
    flow = X_00060_00003) %>% 
  mutate(
    date = ymd(date),
    yday= yday(date), 
    date = as.POSIXct(date)) %>% 
  rename(flow_2021=flow) %>% 
  select(- date)
df<-left_join(df, df_year) %>% drop_na()

#Plot
hydrograph_plot<-df %>% 
  filter(yday<342) %>% 
  #Convert to posix
  mutate(date = as.POSIXct(date)) %>% 
  #Start ggplot object
  ggplot() + 
  #Add ribbon
  geom_ribbon(
    aes(x=date, ymin = lwr, ymax=upr),
    col="#E57200",
    bg="#E57200", 
    alpha = 0.5) +
  #Add line data
  geom_line(
    aes(x=date, y=med), 
    lwd=0.75, 
    col="#E57200") +
  #Add this years flow data
  geom_line(
    aes(x=date, y=flow_2021),
    col="#232D4B", lwd=1.2
  )+
  #Plot y-axis in log scale
  scale_y_log10() +
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 32), 
    axis.text  = element_text(size = 28)
  ) + 
  #Add axes titles
  xlab(NULL) + 
  ylab("Flow [cfs]")+ 
  #Control labels
  scale_x_datetime(date_labels = "%b")
  
#Print plot for funzies
hydrograph_plot 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Modeled Inundation---------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Load relevant data --------------------------------------------------------
dem<-raster(paste0(spatial_dir,"II_Work\\dem_neon.tif"))
flowlines<-
  st_read(paste0(spatial_dir,"II_Work\\flowlines.shp")) %>% 
  st_zm() %>% 
  filter(GNIS_NAME == "Sipsey River")
gage<-st_read(paste0(spatial_dir,"II_Work\\gage_point.shp")) %>% st_zm()

#3.2 Remove valley slope from DEM ----------------------------------------------
#Export files to scratch workspace
# writeRaster(dem,paste0(workspace_dir,"dem.tif"), overwrite=T)
# st_write(flowlines,paste0(workspace_dir,"flowlines.shp"))
# 
# #Convert stream to raster 
# wbt_rasterize_streams(
#   streams = 'flowlines.shp',
#   base = 'dem.tif',
#   output = 'streams.tif',
#   wd = workspace_dir
# )  
# 
# #Add dem value to stream raster
# wbt_multiply(
#   input1 = "streams.tif",
#   input2 = "dem.tif",
#   output = "stream_ele.tif",
#   wd = workspace_dir
# )
# 
# #Convert raster to point
# wbt_raster_to_vector_points(
#   input = "stream_ele.tif",
#   output = "streams_pnts.shp",
#   wd = workspace_dir
# )
# 
# #Complete IDW interp
# wbt_idw_interpolation(
#   input = "streams_pnts.shp",
#   field = "VALUE", 
#   output = "idw.tif",
#   base =  "dem.tif",
#   radius = 5000,
#   weight = 4.2,
#   wd = workspace_dir
# )
# 
# #Remove valley slope from dem
# wbt_subtract(
#   input1 = "dem.tif",
#   input2 = "idw.tif",
#   output = "dem_norm.tif",
#   wd = workspace_dir)

#Read dem into R 
dem_norm<-raster(paste0(workspace_dir,"dem_norm.tif"))

#3.3 Update gage datum ---------------------------------------------------------
#Retreive sd_curve
sd_curve<-readNWISrating('02446500', "base")

#Clean up sd curve
sd_curve<-sd_curve %>% 
  dplyr::mutate(
    depth_ft = INDEP,
    depth_m  = depth_ft*0.3048,
    flow_cfs = DEP) %>% 
  dplyr::select(flow_cfs, depth_m)

#Get datum data and convert ot meters
datum <- readNWISsite('02446500')$alt_va*0.3048

#Create interpolation function
sd_interp<-approxfun(sd_curve$flow_cfs, sd_curve$depth_m)

#Add depth to return interval df
df<-df %>%
  drop_na() %>% 
  mutate(
    stage_med = sd_interp(med), 
    stage_upr = sd_interp(upr), 
    stage_lwr = sd_interp(lwr), 
    stage_2021 = sd_interp(flow_2021))

#Convert stage to elevation
df <- df %>% 
  mutate(
    ele_med = stage_med + datum, 
    ele_lwr = stage_lwr + datum,
    ele_upr = stage_upr + datum, 
    ele_2021 = stage_2021 + datum)

#Calc offset
offset <- raster::extract(dem_norm, gage) - raster::extract(dem, gage)

#Apply offset
df<- df %>% 
  mutate(
    ele_norm_med = ele_med + offset, 
    ele_norm_lwr = ele_lwr + offset, 
    ele_norm_upr = ele_upr + offset, 
    ele_norm_2021 = ele_2021 + offset)

#3.4 Create Inundation Relationship --------------------------------------------
#Create inundation function 
dem_inundate<-function(ele, weight=1){
  
  #Call libraries of interest
  library(raster)
  
  #Create conditional fun
  con<-function(condition, trueValue, falseValue){
    return(condition * trueValue + (!condition)*falseValue)
  }
  
  #apply fun
  r<-con(dem_norm>ele,0,1)
  
  #Estimate inundation area
  area_km2<-cellStats(r, sum, na.rm=T)*res(r)[1]*res(r)[2]*1e-6
  
  #Export Tibble
  tibble(ele,area_km2)
}

#Apply inundation function
inundation<-lapply(seq(-1.5, 1.7, 0.1), dem_inundate)
inundation<-inundation %>% bind_rows

#3.5 Estimate inundation over the year -----------------------------------------
#Create inundation area interpolation
inun_fun<-approxfun(inundation$ele, inundation$area_km2)

#apply to annual time series
df<-df %>% 
  mutate(
    area_med = inun_fun(ele_norm_med), 
    area_lwr = inun_fun(ele_norm_lwr),
    area_upr = inun_fun(ele_norm_upr), 
    area_2021 = inun_fun(ele_norm_2021))

#3.6 Plots ---------------------------------------------------------------------
inundation_plot<-df %>% 
  #Convert to posix
  mutate(date = as.POSIXct(date)) %>% 
  #Start ggplot object
  ggplot() + 
  #Add ribbon
  geom_ribbon(
    aes(x=date, ymin = area_lwr, ymax=area_upr),
    col="#E57200",
    bg="#E57200", 
    alpha = 0.5) +
  #Add line data
  geom_line(
    aes(x=date, y=area_med), 
    lwd=0.75, 
    col="#E57200") +
  #Add this years flow data
  geom_line(
    aes(x=date, y=area_2021),
    col="#232D4B", lwd=1.2
  )+
  #Add predefined black/white theme
  theme_bw() +
  #Change font size of axes
  theme(
    axis.title = element_text(size = 32), 
    axis.text  = element_text(size = 28)
  ) + 
  #Add axes titles
  xlab(NULL) + 
  ylab(expression(Inundation~Area~"[km"^2*"]"))+ 
  #Control labels
  scale_x_datetime(date_labels = "%b")

#Print
inundation_plot

#5.05 Plots --------------------------------------------------------------------
hydrograph_plot + inundation_plot





















