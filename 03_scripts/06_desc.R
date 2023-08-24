#### ................................................................ ####
####              Descriptives for the paper                          ####
#### ................................................................ ####


#### _____________________________________________________________________ ####
#### Map of India with air pollution####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Set the path #### 
file = paste0(gsub("/05_submissions/original","", getwd()))

#### load the data ####
map = read_sf(paste0(file, "/01_RawData/04_shp/gadm40_IND_shp/gadm40_IND_3.shp"))
pol = read_rds(paste0(file, "/02_GenData/02_AirData/acag/acag_monthly_pm25.rds"))


#### Aggregate pollution to the district level ####
pol = pol |> arrange(id3, year, month) |>
  group_by(ID_3 = id3) |> summarise(`Fine PM` = mean(acag_pm25, na.rm = T))

#### Create the plot #### 
map = left_join(map, pol)

#### Make the map #### 
tmap_style("classic")

tmap = tm_graticules() + tm_shape(map) +
  
  tm_fill(col = "Fine PM", showNA = F, n = 10,
          palette = viridisLite::viridis(10, begin = 0, end = 0.77),
          style = "quantile", 
          title = str_wrap("PM 2.5 (ACAG)", 15)) +
  
  tm_legend(position = c(0.05, 0.05), scale = 1.25) +
  tm_layout(inner.margins = c(0,0.1,0,0), legend.format = c(digits = 0), 
            legend.title.size = 1) +
  
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "brown", show.labels = 2, color.light = "white") +
  
  tm_scale_bar(width = 0.17, position = c(0.025, 0.875), text.size = 0.85, 
               color.dark = "brown"); tmap

#### Save the map #### 
tmap_save(tmap, "05_submissions/eaere/01_images/MapIndia.pdf", width = 7, height = 7)

#### Clear the space ####
rm(list = ls()); gc()

#### ________________________________________________________________ ####
#### Map of population density over India ####
#### ________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Set the path #### 
file = paste0(gsub("/05_submissions/original","", getwd()))

#### Clear the space ####
map = read_sf(paste0(file, "/01_RawData/04_shp/gadm40_IND_shp/gadm40_IND_3.shp"))
pop = read_csv(paste0(file, "/01_RawData/05_other/pop/density1k.csv"))

#### Transform the population density to a lon-lat ####
pop = st_as_sf(pop, coords = c(lon = "X", lat = "Y"), crs = 4326)

#### Overlay the population over the sub-districts ####
overlay = st_join(pop, map, join = st_intersects) %>% as.data.frame(.) %>%
  select(ID_3, Z) %>% group_by(ID_3) %>% summarise(pd = mean(Z, na.rm = T))

#### Left join the map over the overlay ###
map = left_join(map, overlay)
map = mutate(map, pdlog = log(pd))

#### Make the map #### 
tmap_style("classic"); tmap = tm_graticules() + 
  tm_shape(map) + 
  tm_fill(col = "pd", showNA = F, n = 20, 
          title = strwrap("Population Density", 20), 
          style = "fixed",
          breaks = c(0, 50, 100, 250, 500, 30000),
          palette = viridisLite::viridis(5, begin = 1, end = 0, option = "inferno")) +
  
  tm_legend(position = c(0.05, 0.05), scale = 1.25) +
  tm_layout(inner.margins = c(0,0.1,0,0), legend.format = c(digits = 0), 
            legend.title.size = 1) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "brown", show.labels = 2, color.light = "white") +
  tm_scale_bar(width = 0.17, position = c(0.025, 0.875), text.size = 0.85, 
               color.dark = "brown"); tmap

#### Save the map #### 
tmap_save(tmap, "05_submissions/eaere/01_images/MapIndiaPD.pdf", width = 7, height = 7)

#### Clear the space ####
#### _____________________________________________________________________ ####
#### Map of India with temperature ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Set the path #### 
file = paste0(gsub("/05_submissions/original","", getwd()))

#### load the data ####
map = read_sf(paste0(file, "/01_RawData/04_shp/gadm40_IND_shp/gadm40_IND_3.shp"))
pol = read_rds(paste0(file, "/02_GenData/03_WeatherData/temp_copernicus/temp_copernicus_2010_2018.rds"))

#### Aggregate pollution to the district level ####
pol = pol |> arrange(id3, year, month) |>
  group_by(ID_3 = id3) |> summarise(Temperature = mean(mean_temp, na.rm = T))

#### Transform the errors in average temperature ####
pol = mutate(pol, Temperature = ifelse(Temperature < -10, -5, Temperature))

#### Left join the temperature to the spatial data ####
map = left_join(map, pol)
x = st_simplify(map, dTolerance = 1000)

#### Create the plot #### 
tmap_style("classic"); tmap = tm_graticules(alpha = 0.25, labels.size = 0.6) + 
  tm_shape(map) +
  tm_fill(col = "Temperature", showNA = F, n = 10, style = "quantile",
          palette = viridisLite::viridis(10, begin = 0, end = 0.8), 
          title = str_wrap("Temperature Degrees", 15)) +
  
  tm_legend(position = c(0.05, 0.05), scale = 1.25) +
  tm_layout(inner.margins = c(0,0.1,0,0), legend.format = c(digits = 0), 
            legend.title.size = 1) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "brown", show.labels = 2, color.light = "white") +
  tm_scale_bar(width = 0.17, position = c(0.025, 0.875), text.size = 0.85, 
               color.dark = "brown"); tmap

#### Save the map #### 
tmap_save(tmap, "05_submissions/original/01_images/MapIndiaTmp.pdf", width = 7, height = 7)

#### Clear the space ####
#### _____________________________________________________________________ ####
#### Map of India with precipitation ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Set the path #### 
file = paste0(gsub("/05_submissions/original","", getwd()))

#### load the data ####
map = read_sf(paste0(file, "/01_RawData/04_shp/gadm40_IND_shp/gadm40_IND_3.shp"))
rain = read_rds(paste0(file, "/02_GenData/03_WeatherData/precipflux_copernicus/precipflux_copernicus_2010_2018.rds"))

#### Aggregate pollution to the district level ####
rain = rain |> arrange(id3, year, month) |>
  group_by(ID_3 = id3) |> summarise(Rain = mean(flux, na.rm = T))

#### Create the plot #### 
map = left_join(map, rain)
x = st_simplify(map, dTolerance = 1000)

#### Make the map #### 
tmap_style("classic"); tmap = tm_graticules(alpha = 0.25, labels.size = 0.6) + 
  tm_shape(map) +
  tm_fill(col = "Rain", showNA = F, n = 10, 
          title = "Precipitation Flux",
          palette = viridisLite::viridis(15, begin = 0, end = 0.8),
          syle = "quantile") +
  tm_legend(position = c(0.05, 0.05), scale = 1.25) +
  tm_layout(inner.margins = c(0,0.1,0,0), legend.format = c(digits = 0), 
            legend.title.size = 1) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "brown", show.labels = 2, color.light = "white") +
  tm_scale_bar(width = 0.17, position = c(0.025, 0.875), text.size = 0.85, 
               color.dark = "brown"); tmap

#### Save the map #### 
tmap_save(tmap, "05_submissions/original/01_images/MapIndiaRain.pdf", width = 7, height = 7)

#### Clear the space ####
#### _____________________________________________________________________ ####
#### Map of temperature inversions ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls());gc()

#### Set the path #### 
file = paste0(gsub("/05_submissions/original","", getwd()))

#### load the data ####
map = read_sf(paste0(file, "/01_RawData/04_shp/gadm40_IND_shp/gadm40_IND_3.shp"))
inv = read_rds(paste0(file, "/02_GenData/03_WeatherData/inversions_copernicus/monthlythermalinversions_ERA5.rds"))

#### Aggregate pollution to the district level ####
inv = inv |> arrange(id3) |> mutate(inv = ifelse(tempinv > 0, 1, 0)) |> 
  summarise(tempinv = sum(inv == 1, na.rm = T))

#### Create the plot #### 
map = left_join(map, inv, by = c("ID_3" = "id3"))

#### Make the map #### 
tmap_style("classic"); tmap = tm_graticules(alpha = 0.25, labels.size = 0.6) + 
  tm_shape(map) +
  tm_fill(col = "tempinv", showNA = F, n = 10, 
          title = str_wrap("No. of Thermal Inversions (2010-2018)", 15), style = "equal", 
          palette = viridisLite::viridis(15, begin = 0, end = 0.8)) +
  
  tm_legend(position = c(0.05, 0.05), scale = 1.25) +
  tm_layout(inner.margins = c(0,0.1,0,0), legend.format = c(digits = 0), 
            legend.title.size = 1) +
  
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "brown", show.labels = 2, color.light = "white") +
  tm_scale_bar(width = 0.17, position = c(0.025, 0.875), text.size = 0.85, 
               color.dark = "brown"); tmap

#### Save the map #### 
tmap_save(tmap, "05_submissions/eaere/01_images/MapIndiaInv.pdf", width = 7, height = 7)

#### Clear the space ####
rm(list = ls());gc()
#### _____________________________________________________________________ ####
#### Correlation between inversions and air pollution ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the monthly data file ####
data = read_rds("02_GenData/05_AnalysisData/MonthlyAnalysis.rds")

#### Construct a non-parametric function of thermal inversions ####
data = data %>% group_by(id3) %>% mutate(tempinv2 = ntile(tempinv, 10))

#### Aggregate to the average value by inversion decile ####
sum = data |> group_by(tempinv2, id3) |> 
  summarise(pm = mean(acag_pm25, na.rm = T), pm_sd = sd(acag_pm25, na.rm = T)) |> 
  group_by(tempinv2)  |> 
  summarise(pm = mean(pm, na.rm = T), pm_sd = mean(pm_sd, na.rm = T)) |> 
  mutate(pm_sd = paste0("(", round(pm_sd,2), ")"))

head(sum)
#### Plot the relationship between inversion strength and PM2.5 ####
ggplot(sum) +
  geom_point(aes(x = tempinv2, y = pm)) +
  geom_line(aes(x = tempinv2, y = pm), alpha = 0.25) +
  geom_text(aes(y = pm, x = tempinv2, label = pm_sd), vjust = 2) +
  
  theme(panel.background = element_blank(), strip.background = element_blank(), 
        axis.line = element_line(), strip.text = element_text(hjust = 0)) +
  
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  grids(axis = "y") +
  labs(y = "PM 2.5", x = "Thermal Inversion (Decile)")

#### Save the map #### 
ggsave("05_submissions/original/01_images/InvLine.png", width = 7, height = 7)

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### First stage monthly pm25 as a function of inversions ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/05_AnalysisData/MonthlyAnalysis.rds")

#### Transform the state code into a factor variable ####
data = mutate(data, StateCodeShp = as_factor(StateCodeShp))

#### Take away NAs in dependent variable or instrument ####
data = filter(data, is.na(tempinv) == F, is.na(acag_pm25) == F)

#### Construct a non-parametric function of thermal inversions ####
data = data %>% group_by(id3) %>% mutate(tempinv2 = ntile(tempinv, 5))

#### Effect of pm25 on the total number of monthly convictions ####
FirstStage = feols(convicted ~ 1 | 
                     sw(id3, 
                        id3 + year + month,
                        id3 + year + month + TempBin + RainBin,
                        year^month + id3 + TempBin + RainBin) | acag_pm25 ~ i(tempinv2, StateCodeShp, 1), 
                   data = data, mem.clean = T, cluster = "id3^year")

#### Extract the point estimates ####
est = lapply(FirstStage, function(x) 
  
  tidy(x$iv_first_stage$acag_pm25) |> 
    
    mutate(state = gsub(".*::|StateCodeShp", "", term)) |> 
    
    mutate(inv = gsub(".*tempinv2::|:State.*", "", term)) |> 
    
    mutate(sign = ifelse(p.value < .1, "Significant", "Insignificant"))) %>% 
  
  rbindlist(., idcol = "Specification")

#### Load the name of the states and add them to the estimates data frame ####
est = st_read("01_RawData/04_shp/00_gadm36_IND/gadm36_IND_1.shp") %>% 
  mutate(as.data.frame(.), state = as.character(seq(1, 36, 1))) |> 
  select(StateName = NAME_1, state = state) %>% left_join(est, .)

#### Add a description of the specification ####
est = data.frame(Specification = seq(1,4,1), 
                 SpecName = c("(1) State FEs", 
                              "(2) + year and Month FEs", 
                              "(3) + quintile indicators of weather", 
                              "(4) + year-by-month FEs")) %>% 
  left_join(est, .) |> 
  mutate(NameInv = paste0("Q", inv)) |> 
  mutate(SpecName = factor(SpecName, levels = c("(1) State FEs", 
                                                "(2) + year and Month FEs", 
                                                "(3) + quintile indicators of weather", 
                                                "(4) + year-by-month FEs")))

est = mutate(est, statistic2 = ifelse(statistic > 10, 10, statistic))

#### Plot the estimates ####
ggplot(est |> filter(Specification %in% seq(1,2,1))) +
  
  geom_point(aes(y = reorder(StateName, as.numeric(state)), 
                 x = reorder(NameInv, as.numeric(inv)),
                 size=  estimate, color = sign, alpha= abs(statistic)), 
             shape=20, stroke=FALSE) +
  

  theme(legend.position = "right", 
        plot.background = element_blank(), 
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_text(hjust = 0.90), 
        strip.text = element_text(hjust = 0, vjust = 1),
        panel.grid = element_blank()) +
  
  scale_size_continuous(name="Point Estimate", trans="pseudo_log", range=c(1,11), breaks= c(0, 10, 20, 40)) +
  scale_alpha_continuous(name="T-Value", trans="pseudo_log", breaks= c(0, 2, 5, 10), range = c(0, 3)) +
  scale_color_viridis(option="G", name="Significance (P.value > 0.1)", discrete = T) + labs(y = "", x = "") +
  
  guides(colour = guide_legend(override.aes = list(size=8)),
         alpha = guide_legend(override.aes = list(size=8))) +
  
  facet_wrap(~SpecName)


#### Save the plot #### 
ggsave(file = "05_submissions/eaere/01_images/FirstStageB.pdf", width = 10, height = 6)

#### Plot the estimates ####
ggplot(est |> filter(Specification %in% seq(3,4,1))) +
  geom_point(aes(y = reorder(StateName, as.numeric(state)), 
                 x = reorder(NameInv, as.numeric(inv)),
                 size=  estimate, color = sign, alpha= abs(statistic)), shape=20, stroke=FALSE) +
 
   theme(legend.position = "right", 
        plot.background = element_blank(), 
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_text(hjust = 0.90), 
        strip.text = element_text(hjust = 0, vjust = 1),
        panel.grid = element_blank()) + 
  
  scale_size_continuous(name="Point Estimate", trans="pseudo_log", range=c(1,11), breaks= c(0, 10, 20, 40)) +
  scale_alpha_continuous(name="T-Value", trans="pseudo_log", breaks= c(0, 2, 5, 10), range = c(0, 3)) +
  scale_color_viridis(option="G", name="Significance (P.value > 0.1)", discrete = T) + labs(y = "", x = "") +
  
  guides(colour = guide_legend(override.aes = list(size=8)),
         alpha = guide_legend(override.aes = list(size=8))) +
  
  facet_wrap(~SpecName)


#### Save the plot #### 
ggsave(file = "05_submissions/eaere/01_images/FirstStageA.pdf", width = 10, height = 7)

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### First stage monthly pm25 Box Plots ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/05_AnalysisData/MonthlyAnalysis.rds")

#### Transform the state code into a factor variable ####
data = mutate(data, StateCodeShp = as_factor(StateCodeShp))

#### Take away NAs in dependent variable or instrument ####
data = filter(data, is.na(tempinv) == F, is.na(acag_pm25) == F)

#### Construct a non-parametric function of thermal inversions ####
data = data %>% group_by(id3) %>% mutate(tempinv2 = ntile(tempinv, 5))

#### Effect of pm25 on the total number of monthly convictions ####
FirstStage = feols(convicted ~ 1 | year^month + id3 + TempBin + RainBin | 
                     acag_pm25 ~ i(tempinv2, StateCodeShp, 1), 
                   data = data, mem.clean = T, cluster = "id3^year")

#### Predict the values of each regression ####
predict = data.frame(iv = predict(FirstStage$iv_first_stage$acag_pm25))


#### create the data set for the left join ####
data = cbind(data, predict)

#### Load the name of the states  and add them to the estimates data frame ####
data = select(data, id3, year, month, acag_pm25, iv)

plot = gather(data, var, value, -c(id3, year, month))

#### Plot the estimates ####
ggplot(plot) + 
  ggdist::stat_halfeye(aes(x = value, fill = var), height = .6,  justification = -.3,
                       .width = 0, point_colour = NA, alpha = 0.5) +
  
  geom_boxplot(aes(x = value, group = var, color = var), width = .25, 
               fill = "transparent", outlier.shape = NA) + 
  
  
  theme_economist() %+replace%
  theme(legend.title = element_blank(), plot.background = element_blank()) +
  grids("y")



  geom_point(aes(x = acag_pm25, y = 0), size = 0.2, alpha = .2, 
             position = position_jitter(seed = 1, height = 0.1), 
             color = "red")+ 

  labs(y = "", x = "Absent Students") 


#### Save the tmap #### 
ggsave(file = "05_submissions/original/01_images/FirstStageB.pdf", width = 10, height = 6)

#### _____________________________________________________________________ ####
#### Elevation map of India ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls())
  
#### Load the elevation data for Mexico ####
india = read_sf("01_RawData/04_shp/gadm40_IND_shp/gadm40_IND_0.shp")
elevation_data = elevatr::get_elev_raster(locations = india, z = 5, clip = "locations")

  
#### Plot the elevation in mexico ####
tmap_style("classic"); tmap = tm_graticules(alpha = 0.25, labels.size = 0.6) + 
  tm_shape(elevation_data) + tm_raster(col = "file4bac52935b1e", title = "Elevation", 
                                       style = "fixed", n = 10, legend.show = T, 
                                       breaks = c(0, 100, 250,500, 1000, 2000, 4000, 8000), 
                                       palette = viridisLite::viridis(10, begin = 0.7, end = 0)) +
  
  tm_legend(position = c(0.05, 0.05), scale = 1.25) +
  tm_layout(inner.margins = c(0,0.1,0,0), legend.format = c(digits = 0), 
            legend.title.size = 1) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "brown", show.labels = 2, color.light = "white") +
  tm_scale_bar(width = 0.17, position = c(0.025, 0.875), text.size = 0.85, 
               color.dark = "brown"); tmap



  
#### Save the map ####
tmap_save(tmap, file = "05_submissions/original/01_images/ElevationMap.png", width = 6, height = 5)

#### _____________________________________________________________________ ####
#### Political map of India ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Extract the road map of North America ####
india = get_stamenmap(bbox=c(left = 64, bottom = 7, right = 98, top = 37.4), zoom = 5, 
                          maptype='toner-lite')

#### Transform the ggmap to a raster ####
mgmap <- as.matrix(india)
vgmap <- as.vector(mgmap)
vgmaprgb <- col2rgb(vgmap)
gmapr <- matrix(vgmaprgb[1, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
gmapg <- matrix(vgmaprgb[2, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
gmapb <- matrix(vgmaprgb[3, ], ncol = ncol(mgmap), nrow = nrow(mgmap))
india <- brick(raster(gmapr), raster(gmapg), raster(gmapb))


#### Plot the political map of India ####
tmap_style("white"); tmap = tm_graticules(alpha = 0.25, labels.size = 0.6) + 
  tm_shape(india)+
  tm_rgb() +
  tm_compass(type = "rose", position = c("right", "top"), size = 2, 
             color.dark = "black", show.labels = 2, color.light = "white") +
  tm_scale_bar(width = 0.17, position = c(0.025, 0.025), text.size = 0.85, 
               color.dark = "black"); tmap


#### Save the map ####
tmap_save(tmap, file = "05_submissions/eaere/01_images/PoliticalMap.png", width = 6, height = 5)

#### _____________________________________________________________________ ####
#### Lag estimates ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

####  Load the results #### 
tab = read_rds("02_GenData/06_results/LagPoissonFE.rds")

#### Plot the results ####
ggplot(tab) +
  geom_point(aes(x = estimate, y = term, color = spec), position = position_dodge2(width = 0.5)) +
  geom_errorbar(aes(x = estimate, y = term, xmax = estimate + std.error*1.97, 
                    xmin = estimate-std.error*1.97,color = spec), width = 0.5, 
                position = position_dodge2(width = 0.25)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  
  labs(y ="", x = "Point estimate and 95% CIs") + 
  scale_color_viridis(discrete = T, option = "mako", begin = 0.2, end = 0.8, name = "Specification") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  
  theme(plot.background = element_blank(), strip.text = element_text(hjust = 0),
        panel.background = element_blank(), strip.background = element_blank(),
        axis.line.x = element_line(), legend.key = element_rect(fill = "transparent"),
        legend.background  = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.title = element_text(size =9),
        legend.text = element_text(size = 9), 
        text = element_text(size = 9),
        panel.grid = element_blank(),
        panel.grid.minor.y = element_line(color = "gray85", size = 0.05, linetype = "dotted"),
        panel.grid.major.y = element_line(color = "gray90", size = 0.05))

#### Save the plot ####
ggsave(file = "05_submissions/eaere/01_images/lag_results.png", width = 7, height = 4.5)


#### Clear the space ####
rm(list = ls()); gc()
