#### ................................................................ ####
####                Load the sentences data for India                 ####
#### ................................................................ ####

#### ________________________________________________________________ ####
#### construct the R-environment for the server ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### set working directory ####
setwd("/net/work/sarmiento/IndiaCourts/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(purrr)
library(lubridate)
library(vroom)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### ________________________________________________________________ ####
#### Load the raw sentence data file ####
#### ________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(tidyverse)
library(data.table)
library(vroom)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#### Load all the files on Indian sentences ####
data = list.files("01_data/01_judicial/01_cases", 
                  pattern =".csv", full.names = T) %>% 
  lapply(., vroom) 

#### Select relevant columns ####
data = lapply(data, function(x) 
  select(x, id = ddl_case_id, year, state = state_code, district = dist_code, CourtNo = court_no,
         CaseNo = cino, JudgeLvl = judge_position, TypeId = type_name, PurposeId = purpose_name, 
              DispId = disp_name, FilingDate = date_of_filing, DecisionDate = date_of_decision, 
              FirstHearingDate = date_first_list, LastHearingDate = date_last_list, 
              NextHearingDate = date_next_list))

#### Bind the list of data frames ####
data = rbindlist(data)

#### Glimpse the data ####
glimpse(data)

#### save the data set ####

write_rds(data, file = "02_gen/01_judicial/raw_cases.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Only keep the criminal cases ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#### Load the data set ####
data = read_rds("02_gen/01_judicial/raw_cases.rds")
acts = fread("01_data/01_judicial/02_keys/acts_sections.csv")

#### Rename the columns in the acts data ####
acts = rename(acts, id = ddl_case_id, bailable = bailable_ipc, SectionsNo = number_sections_ipc)

#### Transform empty strings in bailable dummy to NAs ####
acts = mutate(acts, bailable = ifelse(bailable == "", NA, bailable))

#### Left join the raw cases with the acts and sections file ####
data = left_join(data, acts)

#### Restrict the data to only criminal cases ####
data = filter(data, criminal == 1)

#### Check the data ####
glimpse(data)

#### Save the data set ####
write_rds(data, file = "02_gen/01_judicial/criminal_cases.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Include the judges ID ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(readr)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#### Load the fixes data set ####
data = read_rds("02_gen/01_judicial/criminal_cases.rds")
judges = read_csv("01_data/01_judicial/judges_clean.csv")
JudgesKey = read_csv("01_data/01_judicial/02_keys/judge_case_merge_key.csv")

#### Fix all the wrong types of hearing ####
data = mutate_at(data, vars(state, district, CourtNo), function(x) x = as.numeric(x))

#### Change the column names of the Judges identifier data ####
JudgesKey = rename(JudgesKey, id = ddl_case_id, 
                   judge = ddl_decision_judge_id, 
                   FillingJudge = ddl_filing_judge_id)

#### Add the judge identifier to the raw data ####
data = left_join(data, JudgesKey)

#### Add the date the judge started working in that courthouse ####
judges = select(judges, judge = ddl_judge_id, state = state_code, 
               district = dist_code, CourtNo = court_no, JudgeStartDate = start_date)

#### Left Join the judges data ####
data = left_join(data, judges)

#### Check the data frame ####
data %>% as.data.frame(.) %>% head(.)

#### Save the data set ####
write_rds(data, file = "02_gen/01_judicial/criminal_cases.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Include information on the type of case, disposition, and purposes ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(readr)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#### Load the data sets with the key of the variables ####
data = read_rds("02_gen/01_judicial/raw_cases.rds")
type = read_csv("01_data/01_judicial/02_keys/type_name_key.csv")
disp = read_csv("01_data/01_judicial/02_keys/disp_name_key.csv")
purpose = read_csv("01_data/01_judicial/02_keys/purpose_name_key.csv")

#### Modify the key data sets to merge ####
type = rename(type, TypeId = type_name, TypeName = type_name_s) %>% select(-count)
disp = rename(disp, DispId = disp_name, DispName = disp_name_s) %>% select(-count)
purpose = rename(purpose, PurposeId = purpose_name, PurposeName = purpose_name_s) %>% select(-count)

#### Include the key data sets to the cases data ####
data = left_join(data, type) %>% left_join(., disp) %>% 
  left_join(., purpose)

#### Glimpse the data ####
glimpse(data)

#### save the data set ####
write_rds(data, file = "02_gen/01_judicial/criminal_cases.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _______________________________________________________________ ####
#### Include the names and coordinates of courthouses ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(readr)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#### Load all the files on Indian sentences ####
data = read_rds("02_gen/01_judicial/criminal_cases.rds")
district = read_csv("01_data/01_judicial/02_keys/cases_district_key.csv")
CourtsCoord = read_rds("02_gen/01_judicial/courts_coordinates.rds")

#### Include the name of the district in the data ####
data = select(district, year,  state = state_code, StateName = state_name, 
                  district = dist_code, DistrictName = district_name) %>% 
  mutate_at(vars(state, district), as.numeric) %>% left_join(data, .)

#### Include the coordinates of the courthouse ####
data = select(CourtsCoord, state = state_code, district = dist_code, 
              CourtNo = court_no, CourtName = court_name, lon, lat) %>%
  left_join(mutate_at(data, vars(state, district, CourtNo), as.numeric), .)

#### See the final data ####
data %>% glimpse()

#### Save the data set ####
write_rds(data, file = "02_gen/01_judicial/criminal_cases.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()
#### ________________________________________________________________ ####
#### Organize the courts data ####
#### ________________________________________________________________ ####
#### Clear the data set ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(vroom)
library(lubridate)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")

#### Load the data set ####
conv = vroom("01_data/01_judicial/01_csv/keys/convictions.csv")
data = read_rds("02_gen/01_CourtHearings/criminal_cases.rds")

#### select only the necessary columns ####
data = select(data, CaseNo, state, StateName, district, DistrictName, CourtNo, CourtName, 
              lon, lat, judge, FilingDate, DecisionDate, FirstHearingDate, LastHearingDate, 
              NextHearingDate, act, section, bailable, DispName)

#### Take away cases without a specific state ####
data = filter(data, state != 33)

#### Take away cases with no decision date ####
data = filter(data, is.na(DecisionDate) == F)

#### Add the time fixed effects ####
data = mutate(data, year = year(DecisionDate), 
              month = lubridate::month(DecisionDate, abbr = T, label = T), 
              weekday = weekdays(DecisionDate, abbreviate = T))

#### Exclude all decision dates that do not occur between 2010 and 2019 ####
data = filter(data, year %in% seq(2010, 2019, 1))

#### Add the conviction or acquited identifier ####
data = left_join(data, rename(conv, DispName = disposition))

#### View the data ####
glimpse(data)

##### Save the file ####
write_rds(data, file = "02_gen/01_judicial/reg_data.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()
