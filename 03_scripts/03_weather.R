#### ................................................................. ####
####          Extract the district values of weather variables         ####
#### ................................................................. ####


#### ________________________________________________________________ ####
#### Create a data-frame of weighted thermal inversions per district ####
#### ________________________________________________________________ ####
#### Set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Clear the space ####
rm(list = ls()); gc(); library(pacman)

#### Load the necessary packages ####
p_load(ncdf4, chron, RColorBrewer, lattice, readr, lubridate,
       stringr, tidyverse, rgdal, sp, ggplot2, terra, udpipe,
       tidync, weathermetrics, sf, parallel, conflicted, data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")

#### Suppress summarise info #### 
options(dplyr.summarise.inform = FALSE)

#### Read India shapefile as sf #### 
mun = read_sf("01_data/04_shp/gadm40_IND_shp/gadm40_IND_3.shp")

#### select the relevant columns of the shapefile ####
mun = select(mun, id3 = ID_3)

#### Create vector of thermal inversion .nc files from Copernicus ####
files = list.files("01_data/03_weather/inversions_copernicus/", 
                   full.names = T)[-1] %>% 
  lapply(., list.files, full.names = T) %>% unlist(.)

#### Create a function to extract all the .nc files ####
tidy_inv = function(data){
  
  #### create tibble from .nc file
  hyper_tibble(data, na.rm = TRUE) %>% arrange(longitude, latitude) %>%
    mutate(time =  as.POSIXct(time*3600, tz ="UCT", origin = "1900-01-01")) %>% 
    
    mutate(level = paste0("temp_", level), t = kelvin.to.celsius(t)) %>% as.data.frame(.) %>% 
    
    split(., f = .$level) %>% rbindlist(.) %>% 
    
    spread(., level, t) %>% 
    
    mutate(date = as.Date(time)) %>%  select(-time)}

#### Run the function to extract the thermal inversions data ####
inv = mclapply(files, tidy_inv, mc.cores = 30)

#### Create a function to assign inversions to Indian sub-districts ####
dist_inv = function(data){
  
  #### Estimate the thermal inversions for counties with polygon centroids inside their borders
  
  matched = st_as_sf(data, coords = c("longitude","latitude"), crs = st_crs(mun)) %>% 
    
    st_join(., mun, join = st_intersects) %>%
    
    
    select(id3, temp_925, temp_1000, date) %>% as.data.frame(.) %>%
    
    filter(!is.na(id3)) %>% 
    
    group_by(id3, date) %>% 
    
    summarise(temp_925 = mean(temp_925, na.rm = TRUE),
              temp_1000 = mean(temp_1000, na.rm = TRUE)) %>% 
    ungroup() 
  
  ### subset "non-matched" municipalities and relevant geometries
  subsetshp <- mun %>% filter(id3 %in% unique(mun$id3)[!(unique(mun$id3) %in% unique(matched$id3))])
  
  ### join by nearest grid cell for unmatched municipalities
  unmatched = st_join(subsetshp, st_as_sf(data, coords = c("longitude","latitude"), crs = st_crs(mun)), 
                      join = st_nearest_feature) %>% as.data.frame() %>% select(id3,temp_925, temp_1000, date)
  
  ### merge the two (matched and unmatched)
  rbind(matched, unmatched) %>% 
    
    mutate(temp_diff = temp_925 - temp_1000,
           temp_inversion = ifelse(temp_diff > 0, 1,0),
           invstrength = ifelse(temp_diff > 0, temp_diff, 0)) %>% 
    
    mutate(year = year(date), month = month(date),
           weightedtinv = temp_inversion * invstrength) %>% ungroup()
}

#### Run the function to extract the thermal inversions data ####
DistInv = mclapply(inv, dist_inv, mc.cores = 30)

#### Bind all the list elements ####
DistInv = rbindlist(DistInv)

#### Aggregate to the monthly level ####
DistInv = DistInv %>% group_by(id3, month, year) %>% summarise(tempinv = sum(weightedtinv, na.rm = T))

#### Save the file ####
write_rds(DistInv, "02_gen/03_weather/inversions_copernicus/monthlythermalinversions_ERA5.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Extract county monthly means of the temperature data ####
#### ________________________________________________________________ ####
#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Clear the space ####
rm(list = ls()); gc(); library(pacman)

#### Load the necessary packages ####
p_load(raster, rasterVis, ncdf4, weathermetrics,
       lattice, tidync, parallel, data.table,
       dplyr, stringr, readr, terra, sf)

#### load India shape file  ####
india_shp = read_sf("01_data/04_shp/gadm40_IND_shp/gadm40_IND_3.shp")

#### select sample ".nc" file to get relevant CRS  ####
sample_nc = "01_data/03_weather/temp_copernicus/nc_files/2018/Temperature-Air-2m-Mean-24h_C3S-glob-agric_AgERA5_20180101_final-v1.0.nc"
nc_raster = raster(sample_nc, varname = "Temperature_Air_2m_Mean_24h")

#### adapt shapefile crs to match ".nc" files  ####
india_shp = st_transform(india_shp, crs = st_crs(nc_raster))

#### Create vector of temperature NC files ####
files = list.files("01_data/03_weather/temp_copernicus/nc_files/", full.names = T) %>% 
  lapply(., list.files, full.names = T) %>% unlist(.)

#### Create a function to average the precipitation fluxes over Indian Sub-districts ####
mean_tmp = function(data){
  
  # read daily precipitation nc file as raster
  nc2raster = raster(data, varname = "Temperature_Air_2m_Mean_24h")
  
  # subset nc file to India boundaries
  raster = crop(nc2raster, india_shp)
  
  
  # extract file date and year
  file_date = str_extract(data, "2[0-9][0-9][0-9][0-9][0-9][0-9][0-9]")
  year = substr(file_date, 1, 4)
  
  # compute daily precipitation mean by municipality (weighted mean)
  tmp_mean = terra::extract(raster, india_shp, fun = mean, weights = TRUE, na.rm =TRUE)
  
  
  # Construct the data frame
  data.frame(id3 = india_shp$ID_3, year = year, date = file_date, 
             mean_temp = tmp_mean[, 1])}

#### Run the function across all raster files ####
tmp_data = mclapply(files, mean_tmp, mc.cores = 30)

#### Bind the temperature data together ####
tmp_data = rbindlist(tmp_data) %>% 
  select(id3, date, mean_temp) 

#### Aggregate to the monthly level ####
tmp_data = tmp_data %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  mutate(year = year(date), month = month(date)) %>%
  group_by(id3, year, month) %>% 
  summarise(mean_temp = mean(mean_temp, na.rm = T)) %>% 
  mutate(mean_temp = kelvin.to.celsius(mean_temp))

#### Save the data set ####
write_rds(tmp_data, file = "02_GenData/03_weather/temp_copernicus/temp_copernicus_2010_2018.rds")

#### Clear the space ####
rm(list = ls());gc()

#### ________________________________________________________________ ####
#### Extract county monthly means of the precipitation data ####
#### ________________________________________________________________ ####

#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Clear the space ####
rm(list = ls()); gc(); library(pacman)

#### Load the necessary packages ####
p_load(raster, rasterVis, ncdf4,
       lattice, tidync, parallel,
       dplyr, stringr, readr, terra, sf, 
       data.table)

#### load India shape file  ####
india_shp = read_sf("01_data/04_shp/gadm40_IND_shp/gadm40_IND_3.shp")

#### select sample ".nc" file to get relevant CRS  ####
sample_nc <- "01_data/03_weather/precipflux_copernicus/nc_files/2010/Precipitation-Flux_C3S-glob-agric_AgERA5_20100101_final-v1.0.nc"
nc_raster = raster(sample_nc, varname = "Precipitation_Flux")

#### adapt shapefile crs to match ".nc" files  ####
india_shp <- st_transform(india_shp, crs = st_crs(nc_raster))

#### Create vector of precipitation-flux tiff files ####
files = list.files("01_data/03_weather/precipflux_copernicus/nc_files/", full.names = T) %>% 
  lapply(., list.files, full.names = T) %>% unlist(.)

#### Create a function to average the precipitation fluxes over Indian Sub-districts ####
mean_flux = function(data){
  
  # read daily precipitation nc file as raster
  nc2raster = raster(data, varname = "Precipitation_Flux")
  
  # subset nc file to India boundaries
  raster = crop(nc2raster, india_shp)
  
  
  # extract file date and year
  file_date = str_extract(data, "2[0-9][0-9][0-9][0-9][0-9][0-9][0-9]")
  year = substr(file_date, 1, 4)
  
  # compute daily precipitation mean by municipality (weighted mean)
  precip_mean = terra::extract(raster, india_shp, fun = mean, weights = TRUE, na.rm =TRUE)
  
  
  # Construct the data frame
  data.frame(id3 = india_shp$ID_3, year = year, date = file_date, 
             flux = precip_mean[, 1])}

#### Run the function across all raster files ####
flux_data = mclapply(files, mean_flux, mc.cores = 30)

#### Bind the flux data together ####
flux_data = rbindlist(flux_data) %>% 
  select(id3, date, precipitation_flux = flux) 

#### Aggregate to the monthly level ####
flux_data = flux_data %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  mutate(year = year(date), month = month(date)) %>%
  group_by(id3, year, month) %>% 
  summarise(flux = mean(precipitation_flux, na.rm = T))

#### Save the data set ####
write_rds(flux_data, file = "02_GenData/03_weather/precipflux_copernicus/precipflux_copernicus_2010_2018.rds")

#### Clear the space ####
rm(list = ls());gc()



#### ________________________________________________________________ ####
#### Extract county monthly means of wind speed and wind direction ####
#### ________________________________________________________________ ####

#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### remove existing files ####
rm(list = ls()); gc(); library(pacman)

#### Load packages ####
p_load(ncdf4, chron, RColorBrewer, lattice, readr, lubridate,
       stringr, dplyr, rgdal, sp, ggplot2,data.table, parallel, 
       terra, udpipe, tidync, numbers, sf, geosphere, nngeo, rCAT)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")

#### Suppress summarise info #### 
options(dplyr.summarise.inform = FALSE)

#### load India municipality shapefile #### 
indiashape = read_sf("01_data/04_shp/gadm40_IND_shp/gadm40_IND_3.shp")

#### Create vector of wind-direction nc files ####
files = list.files("01_data/03_weather/wind_copernicus_monthly/",
                   full.names = T, pattern = ".nc") 

#### change crs to match nc files #### 
indiashape <- st_transform(indiashape, crs = crs(raster::raster(files[1]))) %>% 
  select(id3 = ID_3)

#### Create a function to average the precipitation fluxes over Indian Sub-districts ####
wind_sat = function(data){
  
  # Compute the wind direction of districts with polygon centroids
  matched = hyper_tibble(data, na.rm = TRUE) %>% arrange(longitude, latitude) %>%
    
    mutate(time = as.POSIXct(time*3600, tz ="UCT", origin = "1900-01-01"),
           year = year(time),  month = month(time)) %>% select(-time) %>%
    
    st_as_sf(., coords = c("longitude","latitude"), crs = st_crs(indiashape)) %>% 
    
    st_join(., indiashape, join = st_intersects) %>%
    
    as.data.frame() %>% filter(!(is.na(id3))) %>% 
    
    group_by(year, month, id3) %>%
    
    summarise(u = mean(u10, na.rm = T), 
              v = mean(v10, na.rm = T)) %>% 
    
    ungroup()  %>%
    
    mutate(wdr = atan2(u,v)) %>% mutate(wdr = rad2deg(wdr)) %>% mutate(wdr = wdr + 180) %>% 
    mutate(wsp = ((u^2 + v^2)^0.5))
  
  # Compute the wind direction of districts without polygon centroids
  
  unmatched = hyper_tibble(data, na.rm = TRUE) %>% arrange(longitude, latitude) %>%
    
    mutate(time = as.POSIXct(time*3600, tz ="UCT", origin = "1900-01-01"),
           year = year(time),  month = month(time)) %>% select(-time) %>%
    
    st_as_sf(., coords = c("longitude","latitude"), crs = st_crs(indiashape)) %>% 
    
    st_join(indiashape %>% filter(id3 %in% unique(indiashape$id3)[!(unique(indiashape$id3) %in% unique(matched$id3))]), ., 
            join = st_nearest_feature) %>% as.data.frame() %>% select(id3, year, month, u = u10, v = v10) %>%
    
    mutate(wdr = atan2(u,v)) %>% mutate(wdr = rad2deg(wdr)) %>% mutate(wdr = wdr + 180) %>% 
    mutate(wsp = ((u^2 + v^2)^0.5))
  
  # Bind the matvched and unmatched counties
  rbindlist(list(unmatched, matched), use.names = T) %>% arrange(id3, month, year) }

#### Run the function to extract the thermal inversions data ####
wind = mclapply(files, wind_sat, mc.cores = 30)

#### Bind the list elements ####
wind = rbindlist(wind)

#### write wind to csv #### 
write_rds(wind, file = "02_GenData/03_weather/wind_copernicus/monthly.rds")

#### Clear the space ####
rm(list = ls()); gc()




