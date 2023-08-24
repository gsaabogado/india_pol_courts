# Replication package 

This repository contains the code to replicate the results from the study "Court Decisions and Air Pollution: Evidence from 10,000 Penal Cases in India," Published in Environmental and Resource Economics. It contains two main folders. 

- 03_scripts: Contains the scripts necessary to construct the data sets and run the regressions
- 05_submissions: Contains the quarto files to replicate the working paper one by one. This allows researchers to replicate each of the tables and figures in the study.

Because of space constraints, we store all the data in the following dropbox links.

* [Raw data sets](https://www.dropbox.com/sh/b9y8jznjxc85xzb/AAAKiy58JwYTAJcYbCTadZv_a?dl=0)
* [Generated data sets](https://www.dropbox.com/sh/ai2reos94poti0w/AADJgI8i75MQ7TLuuZNrp9Z5a?dl=0)


## Structure of the 03_scripts folder

The folder contains six different files. Each file allows you to create specific data sets necessary for the final regression. Note that you have access to the generated data sets in the "Generated data sets" Dropbox folder. So, there is no need actually to construct the data by yourself. You can jump directly to 05_reg and run the econometric specifications necessary to replicate the results of the paper.

- 01_cases: Loads and manipulates the data on judicial cases
- 02_pm25: Loads and manipulates the data on fine particulate matter concentrations from the Atmospheric Composition Analysis group of Washington University.
- 03_weather: Loads and manipulates the weather data from ERA-5
- 04_data_ref: Merges the data on cases, pollution, and weather in a single data set optimal for the econometric section
- 05_reg: The script contains all the econometric specifications necessary to replicate the study results
- 06_desc: Contains the code to construct the descriptive statistics we use in the study.

## 05_submissions

This folder contains the quarto file "art.qmd" to replicate the study.

## R Environment

These are the characteristics of the R environment we had when running the code. This is relevant in case some of the packages or used functions do not have support anymore in CRAN. I also include the list of packages and their versions in the folder R_packages.

R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale:
[1] LC_COLLATE=English_Canada.utf8  LC_CTYPE=English_Canada.utf8    LC_MONETARY=English_Canada.utf8
[4] LC_NUMERIC=C                    LC_TIME=English_Canada.utf8    

attached base packages:
[1] graphics  grDevices utils     datasets  stats     methods   base     

other attached packages:
 [1] viridis_0.6.2              viridisLite_0.4.2          terra_1.7-39              
 [4] tidync_0.3.0               ncdf4_1.21                 rasterVis_0.51.2          
 [7] raster_3.6-23              sp_2.0-0                   pacman_0.5.1              
[10] scales_1.2.1               tmap_3.3-3                 sf_1.0-14                 
[13] texreg_1.38.6              knitr_1.39                 kableExtra_1.3.4          
[16] marginaleffects_0.7.0.9001 fixest_0.10.4              Hmisc_4.7-0               
[19] Formula_1.2-4              survival_3.3-1             lattice_0.20-45           
[22] mgsub_1.7.3                vroom_1.6.1                readxl_1.4.0              
[25] ggthemes_4.2.4             ggpubr_0.4.0               ggmap_3.0.0               
[28] rCAT_0.1.6                 numbers_0.8-2              weathermetrics_1.2.2      
[31] broom_1.0.0                forcats_0.5.1              stringr_1.5.0             
[34] dplyr_1.1.1                purrr_1.0.1                readr_2.1.4               
[37] tidyr_1.3.0                tibble_3.2.1               ggplot2_3.4.2             
[40] tidyverse_1.3.2            data.table_1.14.8          conflicted_1.1.0          

loaded via a namespace (and not attached):
  [1] backports_1.4.1      systemfonts_1.0.4    lwgeom_0.2-13        plyr_1.8.8           splines_4.2.1       
  [6] crosstalk_1.2.0      leaflet_2.1.1        digest_0.6.29        htmltools_0.5.6      rsconnect_0.8.27    
 [11] ncmeta_0.3.5         fansi_1.0.4          magrittr_2.0.3       checkmate_2.1.0      memoise_2.0.1       
 [16] googlesheets4_1.0.0  cluster_2.1.3        tzdb_0.3.0           rayrender_0.29.6     modelr_0.1.8        
 [21] sandwich_3.0-2       svglite_2.1.0        timechange_0.2.0     jpeg_0.1-9           colorspace_2.1-0    
 [26] rvest_1.0.2          ggdist_3.2.0         haven_2.5.0          xfun_0.39            rgdal_1.6-7         
 [31] leafem_0.2.0         hexbin_1.28.2        crayon_1.5.2         jsonlite_1.8.7       progressr_0.11.0    
 [36] zoo_1.8-12           glue_1.6.2           stars_0.6-2          gtable_0.3.3         gargle_1.2.0        
 [41] webshot_0.5.3        distributional_0.3.0 car_3.1-0            abind_1.4-5          DBI_1.1.3           
 [46] rstatix_0.7.0        Rcpp_1.0.10          htmlTable_2.4.1      units_0.8-2          foreign_0.8-82      
 [51] bit_4.0.5            proxy_0.4-27         htmlwidgets_1.6.2    httr_1.4.6           RColorBrewer_1.1-3  
 [56] farver_2.1.1         pkgconfig_2.0.3      XML_3.99-0.10        nnet_7.3-17          dbplyr_2.2.1        
 [61] deldir_1.0-6         utf8_1.2.3           tidyselect_1.2.0     rlang_1.1.0          tmaptools_3.1-1     
 [66] munsell_0.5.0        cellranger_1.1.0     tools_4.2.1          cachem_1.0.6         cli_3.6.1           
 [71] generics_0.1.3       evaluate_0.15        fastmap_1.1.0        leafsync_0.1.0       bit64_4.0.5         
 [76] fs_1.6.1             RNetCDF_2.6-1        RgoogleMaps_1.4.5.3  nlme_3.1-157         pracma_2.4.2        
 [81] xml2_1.3.3           compiler_4.2.1       rstudioapi_0.15.0    png_0.1-7            e1071_1.7-13        
 [86] ggsignif_0.6.3       reprex_2.0.1         stringi_1.7.12       Matrix_1.4-1         classInt_0.4-9      
 [91] vctrs_0.6.1          pillar_1.9.0         lifecycle_1.0.3      bitops_1.0-7         elevatr_0.4.2       
 [96] R6_2.5.1             latticeExtra_0.6-30  KernSmooth_2.23-20   gridExtra_2.3        codetools_0.2-18    
[101] dichromat_2.0-0.1    assertthat_0.2.1     rjson_0.2.21         withr_2.5.0          parallel_4.2.1      
[106] hms_1.1.3            dreamerr_1.2.3       grid_4.2.1           rpart_4.1.16         class_7.3-20        
[111] rmarkdown_2.14       carData_3.0-5        googledrive_2.0.0    numDeriv_2016.8-1.1  lubridate_1.9.2     
[116] base64enc_0.1-3      interp_1.1-2        
