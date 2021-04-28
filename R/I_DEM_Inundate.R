#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: DEM Inundate
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/4/2020
#Purpose: Create initial inundation maps of Sipsey River
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#Load libraries of interest
library(dataRetrieval)
library(sf)
library(raster)
library(whitebox)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(tidyverse)

#Set directories of interest
spatial_dir<-"C:\\Users\\cnjones7\\Box Sync\\My Folders\\Research Projects\\Sipsey\\spatial_data\\"
workspace_dir <- "data/"

#Load relevant data
dem<-raster(paste0(spatial_dir,"II_Work\\dem_neon.tif"))
flowlines<-st_read(paste0(spatial_dir,"II_Work\\flowlines.shp")) %>% st_zm() %>% 
  filter(GNIS_NAME == "Sipsey River")
gage<-st_read(paste0(spatial_dir,"II_Work\\gage_point.shp")) %>% st_zm()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Streamflow stats-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Download USGS data --------------------------------------------------------
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
#3.0 Remove valley slope from DEM-----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Export files to scratch workspace
writeRaster(dem,paste0(workspace_dir,"dem.tif"), overwrite=T)
st_write(flowlines,paste0(workspace_dir,"flowlines.shp"))

#Resample dem [to lower res]
wbt_aggregate_raster(
  input = "dem.tif",
  output = "dem_10.tif",
  agg_factor = 10,
  wd = workspace_dir
)

#Convert stream to raster 
wbt_rasterize_streams(
  streams = 'flowlines.shp',
  base = 'dem_10.tif',
  output = 'streams.tif',
  wd = workspace_dir
)  

#Add dem value to stream raster
wbt_multiply(
  input1 = "streams.tif",
  input2 = "dem_10.tif",
  output = "stream_ele.tif",
  wd = workspace_dir
)

#Convert raster to point
wbt_raster_to_vector_points(
  input = "stream_ele.tif",
  output = "streams_pnts.shp",
  wd = workspace_dir
)

#Complete IDW interp
wbt_idw_interpolation(
  input = "streams_pnts.shp",
  field = "VALUE", 
  output = "idw.tif",
  base =  "dem_10.tif",
  radius = 5000,
  weight = 4.2,
  wd = workspace_dir
)

#Remove valley slope from dem
wbt_subtract(
  input1 = "dem_10.tif",
  input2 = "idw.tif",
  output = "dem_norm.tif",
  wd = workspace_dir)

#Read dem into R 
dem_norm<-raster(paste0(workspace_dir,"dem_norm.tif"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Inundate ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Update gage datum ----------------------------------------------------------
#Calc offset
offset <- raster::extract(dem_norm, gage) - raster::extract(dem, gage)

#Apply offset
df<- df %>% mutate(ele_norm_m = ele_m + offset)

#4.2 Inundation function----------------------------------------------------------
#Create inundation function 
dem_inundate<-function(dem_norm, ele){
  
  #Create conditional fun
  con<-function(condition, trueValue, falseValue){
    return(condition * trueValue + (!condition)*falseValue)
  }
  
  #apply fun
  con(dem_norm>ele,0,1)
}

#4.3 Wrapper function-----------------------------------------------------------
#Denote water year for each record
df<-df %>% 
  mutate(
    water_year = year(date),
    month      = month(date),
    water_year = if_else(month>=10, water_year+1, water_year)
  ) %>% 
  select(-month) %>% 
  filter(water_year>=2000)

#Create vector of water years
water_year<-df %>% select(water_year) %>% distinct()

#Create Wrapper function for each water year
fun<-function(n){
  
  #add libraries of interest
  library(dplyr)
  library(raster)
  
  #isolate water year
  ts<-df %>% filter(water_year == water_year[n])
  
  #Create inner fun
  inner_fun<-function(m){
    ele<-df$ele_norm_m[m]
    dem_inundate(dem_norm, ele)
  }
  
  #apply inner function
  rs<-lapply(seq(1,nrow(ts)), inner_fun)
  
  #Create raster brick
  rs<-stack(rs)
  
  #Calculate sum
  dur<-raster::calc(rs, sum, na.rm=T)
  
  #Export raster
  return(dur)
}

#Apply function
t0<-Sys.time()
x<-lapply(seq(1,nrow(water_year)), fun) 
tf<-Sys.time()
tf-t0

#Create raster brick
rs<-stack(x)

#Calculate sum
dur<-calc(rs, median, na.rm=T)
backup<-dur
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Create an interactive leaflet map------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#project raster
r<-dur
r@crs<-dem@crs
r<-projectRaster(r, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

#Make zero values NA
r[r==0]<-NA
r[r>120]<-NA

#Color pallet
pal <- colorNumeric(c("#e0f3db","#a8ddb5","#0868ac","#084081"), values(r),
                    na.color = "transparent")

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