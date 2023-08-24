#### ................................................................ ####
####            Calculate average PM25 per Indian subdistrict         ####
#### ................................................................ ####

#### ________________________________________________________________ ####
####  Extract municipality monthly means of the ACAG data ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the necessary packages ####
library(tidyverse)
library(data.table)
library(terra)
library(raster)
library(stringr)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### load India shapefile (Terra) ####
india_shp = vect("01_data/04_shp/gadm40_IND_shp/gadm40_IND_3.shp")

#### Import one nc file as raster to align shapefile and raster CRS ####
imported_raster = rast("01_data/02_pol/acag/tif_files/monthly/V5GL02.HybridPM25.Asia.201001-201001.tif")

#### Project the shapefile CRS to same as raster files ####
india_shp = project(india_shp, raster::crs(imported_raster)) 

#### create vector of all ACAG file names ####
files = list.files("01_data/02_pol/acag/tif_files/monthly/", pattern = ".tif", full.names = T)

#### Create a function to average the ACAG-AQI over Indian Sub-districts ####
mean_pm25 = function(data){
  
  # create a SpatRaster (terra) from my_file in files
  imported_raster = rast(data)
  
  # extract file year and month
  year_month = str_extract(data,"2[0-9][0-9][0-9][0-9][0-9]")
  year = substr(year_month, 1, 4)
  month = substr(year_month, 5, 6)
  
  # compute PM25 mean by municipality (weighted mean)
  pm25mean = terra::extract(imported_raster, india_shp, fun = mean, weights = T, na.rm =T)
  
  # initiate PM25 data frame from subdistrict IDs
  data.frame(id3 = india_shp$ID_3, year = year, month = month, 
             gwr_pm25 = pm25mean[, 2])}

#### Run the function across all raster files ####
acag_data = lapply(files, mean_pm25)

#### Bind the ACAG data together ####
acag_data = rbindlist(acag_data) |> 
  select(id3, year, month, acag_pm25 = gwr_pm25)

#### Save the data set ####
write_rds(acag_data, file = "02_GenData/02_pol/acag/acag_monthly_pm25.rds")

#### Clear the space ####
rm(list = ls());gc()