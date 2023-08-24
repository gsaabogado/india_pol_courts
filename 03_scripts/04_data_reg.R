#### ................................................................ ####
####         Create the final data set for the regressions            ####
#### ................................................................ ####
#### ________________________________________________________________ ####
#### Aggregate the court data to the daily level ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the necessary packages ####
library(conflicted)
library(tidyverse)
library(readr)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")


#### Load the courts data set ####
data = read_rds("02_gen/01_judicial/reg_data.rds")

#### Aggregate the judicial data to the number of cases ####
data = data %>% group_by(state, StateName, district, DistrictName, CourtNo, date = DecisionDate) %>% 
  
  summarise(cases = n(), convicted = sum(conviction == "convicted", na.rm = TRUE), 
            acquitted = sum(conviction == "acquitted", na.rm=TRUE),
            decision = sum(conviction == "decision", na.rm = TRUE))

#### Only keep data between 2010 and 2018 ####
data = data %>% filter(between(date, as.Date("2010-01-01"), as.Date("2018-12-31")))

#### Save aggregated data ####
write_rds(data, file = "02_gen/01_judicial/aggregate_courts.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Assign PM 2.5 to each subdistrict ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the necessary packages ####
library(tidyverse)
library(readxl)
library(readr)
library(sf)
library(raster)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the data ####
courts = read_excel("01_data/01_judicial/courts_manual.xlsx") # Court identifiers
mun = read_sf("01_data/04_shp/gadm40_IND_shp/gadm40_IND_3.shp") # Counties Shapefile
coord = read_xlsx("02_gen/01_judicial/google_finall.xlsx") # court Coordinates
data = read_rds("02_gen/01_judicial/aggregate_courts.rds") %>% ungroup() # Aggregate case data

#### Add column of Google search terms to estimate the coordinates of each district ####
courts = mutate(courts, GoogleSearch = ifelse(city != DistrictName,
                                              paste(city, DistrictName, StateName, "India", sep = ", "),
                                              paste(Headquarters, StateName, "India", sep = ", ")))


#### Rename court coordinates column to match courts data #### 
coord = rename(coord, GoogleSearch = address)
head(coord)

#### Join court coordinates to courts data ####
coord = left_join(courts, coord) |> filter(is.na(long) == F)


#### Select the relevant columns of the shapefile ####
mun = select(mun, StateShp = NAME_1, DistrictShp = NAME_2, SubdistrictShp = NAME_3, id3 = ID_3)


#### Transform the court-city coordinates to a spatial data set ####
coord = st_as_sf(coord, coords = c("long","lat"), crs = st_crs(mun)) 

#### Overlay the city coordinates over the municipality data ####
overlay = as.data.frame(st_join(coord, mun, join = st_intersects))

#### remove court data district and state names (come from shapefile later) ####
data = select(data, -c(DistrictName, StateName)) %>%
  
  rename(StateId = state, DistId = district) %>% 
  
  left_join(., overlay)


#### Create unique codes for each shapefile state and district in courts data ####
data = data %>%
  
  group_by(StateShp, DistrictShp) %>% mutate(DistrictCodeShp = cur_group_id()) %>%
  
  ungroup() %>% 
  
  group_by(StateShp) %>% 
  
  mutate(StateCodeShp = cur_group_id()) %>% ungroup()

#### Aggregate data to daily subdistrict level (shapefile subdistrict) ####
data <- data %>% group_by(date,StateShp, StateCodeShp, DistrictShp, DistrictCodeShp, SubdistrictShp, id3) %>% 
  
  summarise(cases = sum(cases, na.rm = TRUE), 
            convicted = sum(convicted, na.rm = TRUE),
            acquitted = sum(acquitted, na.rm = TRUE), 
            decision = sum(decision, na.rm=TRUE)) %>%
  
  ungroup() %>% 
  
  arrange(id3, date)

#### Save aggregated (daily) data ####
write_rds(data, file = "02_gen/01_judicial/daily_aggregate_courts.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Aggregate data to the monthly level and add the PM2.5 measures ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the necessary packages ####
library(conflicted)
library(tidyverse)
library(lubridate)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")


#### Load the data ####
data = read_rds("02_gen/01_judicial/daily_aggregate_courts.rds")
acag_pm25 = read_rds("02_gen/02_pol/acag/acag_monthly_pm25.rds")

#### Aggregate judicial data to monthly cases to merge with PM2.5 data ####
data <- data %>% mutate(month = month(date), year = year(date)) %>%
  
  group_by(year, month, StateShp, StateCodeShp, DistrictShp, DistrictCodeShp, SubdistrictShp, id3) %>%
  
  summarise(cases = sum(cases, na.rm = TRUE), 
            convicted = sum(convicted, na.rm = TRUE),
            acquitted = sum(acquitted, na.rm = TRUE),
            decision = sum(decision, na.rm = TRUE)) %>% ungroup() %>%
  
  mutate(month = as.numeric(month), year = as.numeric(year)) %>% 
  
  as.data.frame() %>% arrange(year, month, StateCodeShp, DistrictCodeShp, id3)

#### Transform the hammer data for the left-join with the cases data ####
acag_pm25 = acag_pm25 %>%
  mutate(year = as.numeric(year), month = as.numeric(month)) %>% 
  as.data.frame()

#### Add PM2.5 to court hearings data ####
data = left_join(data, acag_pm25)

#### Save monthly pm25 + judicial data frame for analysis ####
write_rds(data, file = "02_gen/05_analysis_data/monthly_analysis.rds")

#### Clear the space ####
rm(list = ls());gc()


#### ________________________________________________________________ ####
#### Create a data-frame of weighted thermal inversions per district ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the necessary packages ####
library(conflicted)
library(tidyverse)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#### Load the data ####
data = read_rds("02_gen/01_judicial/monthly_analysis.rds")
tinv = read_rds("02_gen/03_weather/inversions_copernicus/monthlythermalinversions_ERA5.rds")
rain = read_rds("02_gen/03_weather/precipflux_copernicus/precipflux_copernicus_2010_2018.rds")
temp = read_rds("02_gen/03_weather/temp_copernicus/temp_copernicus_2010_2018.rds")
wind = read_rds("02_gen/03_weather/wind_copernicus/monthly.rds")

#### Merge temperature with monthly analysis data ####
data = left_join(data, temp %>% rename(temp = mean_temp))
 
#### Merge precipitation flux with monthly analysis data ####
data = left_join(data, rain %>% rename(precipflux = flux))

#### Merge the wind direction with monthly analysis data ####
data = left_join(data, wind)

#### Merge the thermal inversions with the monthly analysis data ####
data = left_join(data, tinv)

#### Create a variable with discrete temperature ####
data = data %>% group_by(StateCodeShp) %>% 
  mutate(TempBin = ntile(temp, 10), RainBin = ntile(precipflux, 5))

#### Save the data set ####
write_rds(data, file = "02_gen/05_analysis_data/monthly_analysis.rds")

#### Clear the space ####
rm(list = ls()); gc()
