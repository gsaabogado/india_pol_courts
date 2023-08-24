#### ................................................................ ####
####                          Regressions                             ####
#### ................................................................ ####


#### ________________________________________________________________ ####
#### Poisson regressions with monthly pm25 ####
#### ________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Effect of pm25 on the total number of monthly convictions ####
est = fepois(convicted ~ acag_pm25 | 
               sw(id3, 
                  id3 + year + month,
                  id3 + year + month + TempBin + RainBin,
                  year^month + id3 + TempBin + RainBin), 
             data = data, mem.clean = T, cluster = "id3^year")

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(term == "acag_pm25") |> 
  mutate(spec = paste0("(", spec, ")"))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*1000) |> 
  mutate_at(vars(estimate, std.error), function(x) x = round(x, 2))

#### Transform into html format ####
tab = matrixreg(lapply(split(tab, f = tab$spec), function(x) 
  createTexreg(x$term, x$estimate, x$std.error, x$p.value,
               gof.names = c(rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$n, x$r2, x$bic), 
               gof.decimal = c(F,T,F))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
rownames(tab) = c("names", "Estimate", "", "N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = tab[1,]; tab = tab[-1, ]

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_fE.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson regressions with monthly pm25 (Robustness Fixed Effects) ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Effect of pm25 on the total number of monthly convictions ####
est = fepois(convicted ~ acag_pm25 | 
               sw(TempBin + RainBin,
                  TempBin + RainBin + id3, 
                  TempBin + RainBin + id3 + year + month,
                  year^month + id3 + TempBin + RainBin,
                  year^DistrictShp  + year^month + id3 + TempBin + RainBin,
                  year^DistrictShp + month^StateShp + year^month + id3 + TempBin + RainBin), 
             data = data, mem.clean = T, cluster = "id3^year")

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(term == "acag_pm25") |> 
  mutate(spec = paste0("(", spec, ")"))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*1000) |> 
  mutate_at(vars(estimate, std.error), function(x) x = round(x, 2))

#### Transform into html format ####
tab = matrixreg(lapply(split(tab, f = tab$spec), function(x) 
  createTexreg(x$term, x$estimate, x$std.error, x$p.value,
               gof.names = c(rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$n, x$r2, x$bic), 
               gof.decimal = c(F,T,F))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
rownames(tab) = c("names", "Estimate", "", "N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = tab[1,]; tab = tab[-1, ]

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_fes_robustness.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson regressions with monthly pm25 (Robustness - weather) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Effect of pm25 on the total number of monthly convictions ####
est = fepois(convicted ~ acag_pm25 + sw(,
                                      temp + precipflux, 
                                      temp + temp^2 + precipflux,
                                      temp + temp^2 + precipflux + wsp,
                                      TempBin + RainBin) | id3 + year^month, 
             data = data, mem.clean = T, cluster = "id3^year")

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(term == "acag_pm25") |> 
  mutate(spec = paste0("(", spec, ")"))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*1000) |> 
  mutate_at(vars(estimate, std.error), function(x) x = round(x, 2))

#### Transform into html format ####
tab = matrixreg(lapply(split(tab, f = tab$spec), function(x) 
  createTexreg(x$term, x$estimate, x$std.error, x$p.value,
               gof.names = c(rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$n, x$r2, x$bic), 
               gof.decimal = c(F,T,F))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
rownames(tab) = c("names", "Estimate", "", "N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = tab[1,]; tab = tab[-1, ]

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_weather_robustness.rds")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Poisson regressions with monthly pm25 (Robustness - cluster) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Effect of pm25 on the total number of monthly convictions ####
est = list(fepois(convicted ~ acag_pm25 | id3 + year^month + TempBin + RainBin, 
                  data = data, mem.clean = T, cluster = "id3^year"),
           
           fepois(convicted ~ acag_pm25 | id3 + year^month + TempBin + RainBin, 
                  data = data, mem.clean = T, cluster = "DistrictShp^year"),
           
           fepois(convicted ~ acag_pm25 | id3 + year^month + TempBin + RainBin, 
                  data = data, mem.clean = T, cluster = "id3"),
           
           fepois(convicted ~ acag_pm25 | id3 + year^month + TempBin + RainBin, 
                  data = data, mem.clean = T, cluster = "DistrictShp"))

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(term == "acag_pm25") |> 
  mutate(spec = paste0("(", spec, ")"))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*1000) |> 
  mutate_at(vars(estimate, std.error), function(x) x = round(x, 2))

#### Transform into html format ####
tab = matrixreg(lapply(split(tab, f = tab$spec), function(x) 
  createTexreg(x$term, x$estimate, x$std.error, x$p.value,
               gof.names = c(rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$n, x$r2, x$bic), 
               gof.decimal = c(F,T,F))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
rownames(tab) = c("names", "Estimate", "", "N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = tab[1,]; tab = tab[-1, ]

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_cluster_robustness.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson-IV regressions with monthly pm25 (Inversions) ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()


#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(purrr)
library(lubridate)
library(vroom)
library(fixest)
library(parallel)
library(broom)
library(scales)
library(texreg)

#### Set the random number generator ####
set.seed(7468)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### Load the data ####
#data = read_csv("02_gen/05_analysis_data/monthly_analysis2.csv")
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

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


#### Predict the values of each regression ####
predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
  lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
  lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,4,1)))

#### create the data set for the left join ####
data = cbind(data, predict)

#### Run the second stage ####
est = list(fepois(convicted ~  IV1 | id3, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV2 | id3 + year + month, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV3 | id3 + year + month + TempBin + RainBin, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV4 | year^month + id3 + TempBin + RainBin, data = data,  cluster = "id3^year"))

#### Extract the point estimates #####
sum = lapply(est, function(x) data.frame(tidy(x), n = x$nobs, bic = BIC(x), r2 = r2(x, "pr2"))) %>% rbindlist(.)

#### Add the cluster dimension for the bootstrap ####
data = mutate(data, cluster = paste(id3, year, sep = "_"))

#### Create the bootstrap algorithm to estimate the standard errors ####
ivpois_est = function(data){
  # Construct the random sample
  random = rbindlist(lapply(sample(data$cluster, length(unique(data$cluster))), function(x) filter(data, cluster == x)))
  # 1st stage OLS
  FirstStage = feols(convicted ~ 1 | 
                       sw(id3, 
                          id3 + year + month,
                          id3 + year + month + TempBin + RainBin,
                          year^month + id3 + TempBin + RainBin) | acag_pm25 ~ i(tempinv2, StateCodeShp, 1), 
                     data = random, mem.clean = T)
  # Getting the fitted values
  predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
    lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
    lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,4,1)))
  
  rm(FirstStage)
  # bind fitted values to the original data
  random = cbind(random, predict)
  
  # run second stage
  list(fepois(convicted ~  IV1 | id3, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV2 | id3 + year + month, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV3 | id3 + year + month + TempBin + RainBin, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV4 | year^month + id3 + TempBin + RainBin, data = random,  cluster = "id3^year")) %>% 
    
    lapply(., tidy)
}

#### Run the 1000 bootstraps ####
bs = mclapply(vector("list", length = 100), function(x) x = ivpois_est(data = data), mc.cores = 30)

#bs = lapply(vector("list", length = 100), function(x) x = ivpois_est(data = data))
#### Bind the bootstraps together #####
bs2 = lapply(bs, rbindlist) %>% rbindlist(., idcol = "bs")

#### Compute the standard error of the bootstrapped sample ####
BsSe = bs2 %>% group_by(term) %>% summarise(bse = sd(estimate, na.rm = T))

#### Left join with the main data set #####
sum = left_join(sum, BsSe)

#### Compute the P-value with the bootstrapped SE ####
sum =  sum %>%  mutate(bp = 2*pnorm(abs(estimate/bse), lower.tail = F))

#### Compute the F-test of each city ####
test = lapply(FirstStage, function(x)   x = fitstat(x,"ivf1")$ivf1[1])
test = lapply(test, function(x)  x = data.frame(f_test = c(x)))
test = rbindlist(test, idcol = "term")
test = mutate(test, term = paste0("IV", term)) %>% rename(f_test = stat)

#### Add the first stage Fstatistics to the regression ####
sum = left_join(sum, test)

#### Transform the point estimates to the effect of inceasing PM25 by ten units ####
sum = mutate_at(sum, vars(estimate, std.error, bse), function(x) x = (exp(x) -1)*1000)

#### See the data set ####
glimpse(sum)


#### Transform the tab object to a data frame for HTML ###
tab = matrixreg(lapply(split(sum, f = sum$term), function(x) 
  createTexreg("", x$estimate, x$bse, x$bp,
               gof.names = c(rep("F-test", nrow(x)),
                             rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$f_test, x$n, x$r2, x$bic/1000), 
               gof.decimal = c(T,F,T,T))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
tab = tab[-1, ]; rownames(tab) = c("Estimate", "", "F-Test","N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = c("(1)", "(2)", "(3)", "(4)")
tab[4,] = comma(as.numeric(tab[4,])); tab[6,] = comma(as.numeric(tab[6,]))

tab

#### Save the data set ####
write_rds(tab, file = "02_gen/06_results/main_iv.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson-IV regressions with monthly pm25 (Wind Direction) ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()


#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(purrr)
library(lubridate)
library(vroom)
library(fixest)
library(parallel)
library(broom)
library(scales)
library(texreg)
library(Hmisc)

#### Set the random number generator ####
set.seed(7468)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### Load the data ####
#data = read_csv("02_gen/05_analysis_data/monthly_analysis2.csv")
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Transform the state code into a factor variable ####
data = mutate(data, StateCodeShp = as_factor(StateCodeShp))

#### Take away NAs in dependent variable or instrument ####
data = filter(data, is.na(wdr) == F, is.na(acag_pm25) == F)

#### Construct a non-parametric function of thermal inversions ####
data = data %>% mutate(wdriv = cut2(wdr, seq(0, 360, 10), oneval = F, onlycuts = T))

#### Effect of pm25 on the total number of monthly convictions ####
FirstStage = feols(convicted ~ 1 | 
                     sw(id3, 
                        id3 + year + month,
                        id3 + year + month + TempBin + RainBin,
                        year^month + id3 + TempBin + RainBin) | acag_pm25 ~ i(wdriv, StateCodeShp),
                   data = data, mem.clean = T, cluster = "id3^year")


#### Predict the values of each regression ####
predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
  lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
  lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,4,1)))

#### create the data set for the left join ####
data = cbind(data, predict)

#### Run the second stage ####
est = list(fepois(convicted ~  IV1 | id3, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV2 | id3 + year + month, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV3 | id3 + year + month + TempBin + RainBin, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV4 | year^month + id3 + TempBin + RainBin, data = data,  cluster = "id3^year"))

#### Extract the point estimates #####
sum = lapply(est, function(x) data.frame(tidy(x), n = x$nobs, bic = BIC(x), r2 = r2(x, "pr2"))) %>% rbindlist(.)

#### Add the cluster dimension for the bootstrap ####
data = mutate(data, cluster = paste(id3, year, sep = "_"))

#### Create the bootstrap algorithm to estimate the standard errors ####
ivpois_est = function(data){
  # Construct the random sample
  random = rbindlist(lapply(sample(data$cluster, length(unique(data$cluster))), function(x) filter(data, cluster == x)))
  # 1st stage OLS
  FirstStage = feols(convicted ~ 1 | 
                       sw(id3, 
                          id3 + year + month,
                          id3 + year + month + TempBin + RainBin,
                          year^month + id3 + TempBin + RainBin) | acag_pm25 ~ i(wdriv, StateCodeShp), 
                     data = random, mem.clean = T)
  # Getting the fitted values
  predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
    lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
    lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,4,1)))
  
  rm(FirstStage)
  # bind fitted values to the original data
  random = cbind(random, predict)
  
  # run second stage
  list(fepois(convicted ~  IV1 | id3, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV2 | id3 + year + month, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV3 | id3 + year + month + TempBin + RainBin, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV4 | year^month + id3 + TempBin + RainBin, data = random,  cluster = "id3^year")) %>% 
    
    lapply(., tidy)
}

#### Run the 1000 bootstraps ####
bs = mclapply(vector("list", length = 100), function(x) x = ivpois_est(data = data), mc.cores = 10)

#bs = lapply(vector("list", length = 100), function(x) x = ivpois_est(data = data))
#### Bind the bootstraps together #####
bs2 = lapply(bs, rbindlist) %>% rbindlist(., idcol = "bs")

#### Compute the standard error of the bootstrapped sample ####
BsSe = bs2 %>% group_by(term) %>% summarise(bse = sd(estimate, na.rm = T))

#### Left join with the main data set #####
sum = left_join(sum, BsSe)

#### Compute the P-value with the bootstrapped SE ####
sum =  sum %>%  mutate(bp = 2*pnorm(abs(estimate/bse), lower.tail = F))

#### Compute the F-test of each city ####
test = lapply(FirstStage, function(x)   x = fitstat(x,"ivf1")$ivf1[1])
test = lapply(test, function(x)  x = data.frame(f_test = c(x)))
test = rbindlist(test, idcol = "term")
test = mutate(test, term = paste0("IV", term)) %>% rename(f_test = stat)

#### Add the first stage Fstatistics to the regression ####
sum = left_join(sum, test)

#### Transform the point estimates to the effect of inceasing PM25 by ten units ####
sum = mutate_at(sum, vars(estimate, std.error, bse), function(x) x = (exp(x) -1)*1000)

#### See the data set ####
glimpse(sum)


#### Transform the tab object to a data frame for HTML ###
tab = matrixreg(lapply(split(sum, f = sum$term), function(x) 
  createTexreg("", x$estimate, x$bse, x$bp,
               gof.names = c(rep("F-test", nrow(x)),
                             rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$f_test, x$n, x$r2, x$bic/1000), 
               gof.decimal = c(T,F,T,T))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
tab = tab[-1, ]; rownames(tab) = c("Estimate", "", "F-Test","N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = c("(1)", "(2)", "(3)", "(4)")
tab[4,] = comma(as.numeric(tab[4,])); tab[6,] = comma(as.numeric(tab[6,]))

tab

#### Save the data set ####
write_rds(tab, file = "02_gen/06_results/main_wind_iv.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson regressions with monthly pm25 (Non-linearities) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Create a variable with discrete temperature ####
data = data %>% group_by(StateCodeShp) %>% 
  mutate(TempBin = ntile(temp, 10), RainBin = ntile(precipflux, 5))

#### Divide PM25 into exposure Quintiles ####
data = data %>% ungroup() %>% 
  mutate(pmbin = ntile(acag_pm25, 5))

#### Effect of pm25 on the total number of monthly convictions ####
est = fepois(convicted ~  i(pmbin, 1)| 
               sw(id3, 
                  id3 + year + month,
                  id3 + year + month + TempBin + RainBin,
                  year^month + id3 + TempBin + RainBin, 
                  id3 + year^month^StateCodeShp  + TempBin + RainBin), 
             data = data, mem.clean = T, cluster = "id3^year")

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(grepl("pmbin", term)) |> 
  mutate(spec = paste0("(", spec, ")")) %>% 
  mutate(bin = gsub(".*::", "", term))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*100)

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_bins.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson regressions for cases with monthly pm25 ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Effect of pm25 on the total number of monthly convictions ####
est = fepois(cases ~ acag_pm25 | 
               sw(id3, 
                  id3 + year + month,
                  id3 + year + month + TempBin + RainBin,
                  year^month + id3 + TempBin + RainBin), 
             data = data, mem.clean = T, cluster = "id3^year")

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(term == "acag_pm25") |> 
  mutate(spec = paste0("(", spec, ")"))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*1000)

#### Transform into html format ####
tab = matrixreg(lapply(split(tab, f = tab$spec), function(x) 
  createTexreg(x$term, x$estimate, x$std.error, x$p.value,
               gof.names = c(rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$n, x$r2, x$bic), 
               gof.decimal = c(F,T,F))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
rownames(tab) = c("names", "Estimate", "", "N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = tab[1,]; tab = tab[-1, ]

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_cases.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson regressions for share of convictions with monthly pm25 ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Share of convictions per case ####
data = mutate(data, share = (convicted/cases)*100)

#### Effect of pm25 on the total number of monthly convictions ####
est = list(Share = feols(share ~ acag_pm25 | 
              year^month + id3 + TempBin + RainBin, 
             data = data, mem.clean = T, cluster = "id3^year"), 
           
           Control = fepois(convicted ~ acag_pm25 + cases | 
                             year^month + id3 + TempBin + RainBin,
                           data = data, mem.clean = T, cluster = "id3^year"))


#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(term == "acag_pm25") 

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate(tab, estimate = ifelse(spec == "Control", (exp(estimate) -1)*1000, estimate))
tab = mutate(tab, std.error = ifelse(spec == "Control", (exp(std.error) -1)*1000, std.error))

#### Transform into html format ####
tab = matrixreg(lapply(split(tab, f = tab$spec), function(x) 
  createTexreg(x$term, x$estimate, x$std.error, x$p.value,
               gof.names = c(rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$n, x$r2, x$bic), 
               gof.decimal = c(F,T,F))),
  booktabs = T, digits = 4, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
rownames(tab) = c("names", "Estimate", "", "N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = tab[1,]; tab = tab[-1, ]

#### Check the data-set of summary results ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_cases_share.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson regressions with monthly pm25 (Lag Model) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load packages ####
library(conflicted)
library(tidyverse)
library(fixest)
library(texreg)
library(data.table)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("between", "dplyr")

#### Load the monthly data file ####
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Create the lagged elements ####
data = data |> group_by(id3) |> mutate(lag1 = lag(acag_pm25, 1), lag2 = lag(acag_pm25,2))

#### Effect of pm25 on the total number of monthly convictions ####
est = fepois(convicted ~ acag_pm25 + lag1 + lag2 | 
               sw(id3, 
                  id3 + year + month,
                  id3 + year + month + TempBin + RainBin,
                  year^month + id3 + TempBin + RainBin), 
             data = data, mem.clean = T, cluster = "id3^year")

#### Extract the table characteristics ####
tab = lapply(est, function(x) 
  x = data.frame(tidy(x), r2 = r2(x, type = "apr2"), 
                 n = nobs(x), bic = BIC(x)/1000, N = x$fixef_sizes[1])) %>% 
  rbindlist(., idcol = "spec") |> filter(grepl("acag_pm25|lag", term)) |> 
  mutate(spec = paste0("(", spec, ")"))

#### Transform the point estimates to the effect of increasing PM25 by ten units ####
tab = mutate_at(tab, vars(estimate, std.error), function(x) x = (exp(x) -1)*1000)

#### Change the name of the Lags ####
tab$term = gsub("lag1", "PM2.5 (Lag: t = -1)", tab$term)
tab$term = gsub("lag2", "PM2.5 (Lag: t = -2)", tab$term)
tab$term = gsub("acag_pm25", " PM2.5 (Lag: t = 0)", tab$term)

#### See the results table ####
tab %>% head(.)

#### Save the results ####
write_rds(tab, file = "02_gen/06_results/poisson_lag.rds")

#### Clear the space ####
rm(list = ls()); gc()


#### _____________________________________________________________________ ####
#### Poisson-IV regressions with monthly pm25 (Robustness fixed effects) ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()


#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(purrr)
library(lubridate)
library(vroom)
library(fixest)
library(parallel)
library(broom)
library(scales)
library(texreg)

#### Set the random number generator ####
set.seed(7468)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### Load the data ####
#data = read_csv("02_gen/05_analysis_data/monthly_analysis2.csv")
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Transform the state code into a factor variable ####
data = mutate(data, StateCodeShp = as_factor(StateCodeShp))

#### Take away NAs in dependent variable or instrument ####
data = filter(data, is.na(tempinv) == F, is.na(acag_pm25) == F)

#### Construct a non-parametric function of thermal inversions ####
data = data %>% group_by(id3) %>% mutate(tempinv2 = ntile(tempinv, 5))

#### Effect of pm25 on the total number of monthly convictions ####
FirstStage = feols(convicted ~ 1 | 
                     sw(TempBin + RainBin,
                        TempBin + RainBin + id3, 
                        TempBin + RainBin + id3 + year + month,
                        year^month + id3 + TempBin + RainBin,
                        year^DistrictShp + month^DistrictShp + year^month + id3 + TempBin + RainBin) | acag_pm25 ~ i(tempinv2, StateCodeShp, 1),
                   data = data, mem.clean = T, cluster = "id3^year")


#### Predict the values of each regression ####
predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
  lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
  lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,5,1)))

#### create the data set for the left join ####
data = cbind(data, predict)

#### Run the second stage ####
est = list(fepois(convicted ~  IV1 | TempBin + RainBin, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV2 | id3 + TempBin + RainBin, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV3 | id3 + year + month + TempBin + RainBin, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV4 | id3 + year^month + TempBin + RainBin, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV5 | year^DistrictShp + month^DistrictShp + year^month + id3 + TempBin + RainBin, data = data,  cluster = "id3^year"))


#### Add the cluster dimension for the bootstrap ####
data = mutate(data, cluster = paste(id3, year, sep = "_"))

#### Create the bootstrap algorithm to estimate the standard errors ####
ivpois_est = function(data){
  # Construct the random sample
  random = rbindlist(lapply(sample(data$cluster, length(unique(data$cluster))), function(x) filter(data, cluster == x)))
  
  # 1st stage OLS
  FirstStage = feols(convicted ~ 1 | 
                       sw(TempBin + RainBin,
                          TempBin + RainBin + id3, 
                          TempBin + RainBin + id3 + year + month,
                          year^month + id3 + TempBin + RainBin,
                          year^StateShp + month^StateShp + year^month + id3 + TempBin + RainBin) | acag_pm25 ~ i(tempinv2, StateCodeShp, 1), 
                     data = random, mem.clean = T)
  
  # Getting the fitted values
  predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
    lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
    lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,5,1)))
  
  rm(FirstStage)
  # bind fitted values to the original data
  random = cbind(random, predict)
  
  # run second stage
  list(fepois(convicted ~  IV1 | TempBin + RainBin, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV2 | id3 + TempBin + RainBin, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV3 | id3 + year + month + TempBin + RainBin, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV4 | id3 + year^month + TempBin + RainBin, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV5 | year^StateShp + month^StateShp + year^month + id3 + TempBin + RainBin, data = random,  cluster = "id3^year")) %>% 
    
    lapply(., tidy)
}

#### Run the 1000 bootstraps ####
bs = mclapply(vector("list", length = 200), function(x) x = ivpois_est(data = data), mc.cores = 30)

bs = lapply(vector("list", length = 100), function(x) x = ivpois_est(data = data))
#### Bind the bootstraps together #####
bs2 = lapply(bs, rbindlist) %>% rbindlist(., idcol = "bs")

#### Compute the standard error of the bootstrapped sample ####
BsSe = bs2 %>% group_by(term) %>% 
  summarise(n = n(), 
            bse = sqrt((n-1)/n) * sd(estimate, na.rm = T))  %>%
  select(-n)

#### Extract the point estimates #####
sum = lapply(est, function(x) data.frame(tidy(x), n = x$nobs, bic = BIC(x), r2 = r2(x, "pr2"))) %>% rbindlist(.)

#### Left join with the main data set #####
sum = left_join(sum, BsSe)

#### Compute the P-value with the bootstrapped SE ####
sum =  sum %>%  mutate(bp = 2*pnorm(abs(estimate/bse), lower.tail = F))

#### Compute the F-test of each city ####
test = lapply(FirstStage, function(x)   x = fitstat(x,"ivf1")$ivf1[1])
test = lapply(test, function(x)  x = data.frame(f_test = c(x)))
test = rbindlist(test, idcol = "term")
test = mutate(test, term = paste0("IV", term)) %>% rename(f_test = stat)

#### Add the first stage Fstatistics to the regression ####
sum = left_join(sum, test)

#### Transform the point estimates to the effect of inceasing PM25 by ten units ####
sum = mutate_at(sum, vars(estimate, std.error, bse), function(x) x = (exp(x) -1)*1000)

#### See the data set ####
glimpse(sum)


#### Transform the tab object to a data frame for HTML ###
tab = matrixreg(lapply(split(sum, f = sum$term), function(x) 
  createTexreg("", x$estimate, x$bse, x$bp,
               gof.names = c(rep("F-test", nrow(x)),
                             rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$f_test, x$n, x$r2, x$bic/1000), 
               gof.decimal = c(T,F,T,T))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
tab = tab[-1, ]; rownames(tab) = c("Estimate", "", "F-Test","N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = c("(1)", "(2)", "(3)", "(4)", "(5)")
tab[4,] = comma(as.numeric(tab[4,])); tab[6,] = comma(as.numeric(tab[6,]))

#### Save the data set ####
write_rds(tab, file = "02_gen/06_results/robustness_iv_fes_tab.rds")
write_rds(sum, file = "02_gen/06_results/robustness_iv_fes_sum.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson-IV regressions with monthly pm25 (Inversions - Robustness weather) ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()


#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(purrr)
library(lubridate)
library(vroom)
library(fixest)
library(parallel)
library(broom)
library(scales)
library(texreg)

#### Set the random number generator ####
set.seed(7468)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### Load the data ####
#data = read_csv("02_gen/05_analysis_data/monthly_analysis2.csv")
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Transform the state code into a factor variable ####
data = mutate(data, StateCodeShp = as_factor(StateCodeShp))

#### Take away NAs in dependent variable or instrument ####
data = filter(data, is.na(tempinv) == F, is.na(acag_pm25) == F)

#### Construct a non-parametric function of thermal inversions ####
data = data %>% group_by(id3) %>% mutate(tempinv2 = ntile(tempinv, 5))

#### Effect of pm25 on the total number of monthly convictions ####
FirstStage = feols(convicted ~  sw(,
                                    temp + precipflux, 
                                    temp + temp^2 + precipflux,
                                    temp + temp^2 + precipflux + wsp,
                                    TempBin + RainBin) | year^month + id3 | acag_pm25 ~ i(tempinv2, StateCodeShp, 1),
                   data = data, mem.clean = T, cluster = "id3^year")


#### Predict the values of each regression ####
predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
  lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
  lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,5,1)))

#### create the data set for the left join ####
data = cbind(data, predict)

#### Run the second stage ####
est = list(fepois(convicted ~  IV1 | year^month + id3, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV2 + temp + precipflux | year^month + id3, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV3 + temp + precipflux + temp^2| year^month + id3, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV4 + temp + temp^2 + precipflux + wsp| year^month + id3, data = data,  cluster = "id3^year"),
           fepois(convicted ~  IV5 | year^month + id3 + TempBin + RainBin, data = data,  cluster = "id3^year"))


#### Add the cluster dimension for the bootstrap ####
data = mutate(data, cluster = paste(id3, year, sep = "_"))

#### Create the bootstrap algorithm to estimate the standard errors ####
ivpois_est = function(data){
  # Construct the random sample
  random = rbindlist(lapply(sample(data$cluster, length(unique(data$cluster))), function(x) filter(data, cluster == x)))
  # 1st stage OLS
  FirstStage = feols(convicted ~  sw(,
                                     temp + precipflux, 
                                     temp + temp^2 + precipflux,
                                     temp + temp^2 + precipflux + wsp,
                                     TempBin + RainBin) | year^month + id3 | acag_pm25 ~ i(tempinv2, StateCodeShp, 1),
                     data = random, mem.clean = T)
  
  
  # Getting the fitted values
  predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
    lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
    lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,4,1)))
  
  rm(FirstStage)
  # bind fitted values to the original data
  random = cbind(random, predict)
  
  # run second stage
  list(fepois(convicted ~  IV1 | year^month + id3, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV2 + temp + precipflux | year^month + id3, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV3 + temp + precipflux + temp^2| year^month + id3, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV4 + temp + temp^2 + precipflux + wsp| year^month + id3, data = random,  cluster = "id3^year"),
       fepois(convicted ~  IV5 | year^month + id3 + TempBin + RainBin, data = random,  cluster = "id3^year")) %>% 
    lapply(., tidy)
}

#### Run the 1000 bootstraps ####
bs = mclapply(vector("list", length = 500), function(x) x = ivpois_est(data = data), mc.cores = 30)

#bs = lapply(vector("list", length = 100), function(x) x = ivpois_est(data = data))

#### Extract the point estimates #####
sum = lapply(est, function(x) 
  data.frame(tidy(x), n = x$nobs, bic = BIC(x), r2 = r2(x, "pr2"))) %>% rbindlist(.) %>%
  filter(grepl("IV", term))

#### Bind the bootstraps together #####
bs2 = lapply(bs, rbindlist) %>% rbindlist(., idcol = "bs") %>% 
  filter(grepl("IV", term))

#### Compute the standard error of the bootstrapped sample ####
BsSe = bs2 %>% group_by(term) %>% 
  summarise(n = n(), 
            bse = sqrt((n-1)/n) * sd(estimate, na.rm = T)) %>% 
  select(-n)

#### Left join with the main data set #####
sum = left_join(sum, BsSe)

#### Compute the P-value with the bootstrapped SE ####
sum =  sum %>%  mutate(bp = 2*pnorm(abs(estimate/bse), lower.tail = F))

#### Compute the F-test of each city ####
test = lapply(FirstStage, function(x)   x = fitstat(x,"ivf1")$ivf1[1])
test = lapply(test, function(x)  x = data.frame(f_test = c(x)))
test = rbindlist(test, idcol = "term")
test = mutate(test, term = paste0("IV", term)) %>% rename(f_test = stat)

#### Add the first stage Fstatistics to the regression ####
sum = left_join(sum, test)

#### Transform the point estimates to the effect of inceasing PM25 by ten units ####
sum = mutate_at(sum, vars(estimate, std.error, bse), function(x) x = (exp(x) -1)*1000)

#### See the data set ####
glimpse(sum)


#### Transform the tab object to a data frame for HTML ###
tab = matrixreg(lapply(split(sum, f = sum$term), function(x) 
  createTexreg("", x$estimate, x$bse, x$bp,
               gof.names = c(rep("F-test", nrow(x)),
                             rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$f_test, x$n, x$r2, x$bic/1000), 
               gof.decimal = c(T,F,T,T))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
tab = tab[-1, ]; rownames(tab) = c("Estimate", "", "F-Test","N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = c("(1)", "(2)", "(3)", "(4)", "(5)")
tab[4,] = comma(as.numeric(tab[4,])); tab[6,] = comma(as.numeric(tab[6,]))

tab

#### Save the data set ####
write_rds(tab, file = "02_gen/06_results/robustness_iv_weather_tab.rds")
write_rds(sum, file = "02_gen/06_results/robustness_iv_weather_sum.rds")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Poisson-IV regressions with monthly pm25 (Inversions - Robustness Cluster) ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### set working directory ####
setwd("/work/seme/ls01122/IndiaCourts/")

#### Load packages ####
library(conflicted)
library(tidyverse)
library(data.table)
library(purrr)
library(lubridate)
library(vroom)
library(fixest)
library(parallel)
library(broom)
library(scales)
library(texreg)

#### Set the random number generator ####
set.seed(7468)

#### Select the correct functions ####
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("map", "purrr")

#### Load the data ####
#data = read_csv("02_gen/05_analysis_data/monthly_analysis2.csv")
data = read_rds("02_gen/05_analysis_data/monthly_analysis.rds")

#### Transform the state code into a factor variable ####
data = mutate(data, StateCodeShp = as_factor(StateCodeShp))

#### Take away NAs in dependent variable or instrument ####
data = filter(data, is.na(tempinv) == F, is.na(acag_pm25) == F)

#### Construct a non-parametric function of thermal inversions ####
data = data %>% group_by(id3) %>% mutate(tempinv2 = ntile(tempinv, 5))

#### Effect of pm25 on the total number of monthly convictions ####
FirstStage = list(feols(convicted ~ 1 | year^month + id3 + TempBin + RainBin  | acag_pm25 ~ i(tempinv2, StateCodeShp, 1),
                   data = data, mem.clean = T, cluster = "id3^year"))


#### Predict the values of each regression ####
predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
  lapply(., function(x) lapply(x, function(x) x = data.frame(IV = predict(x, na.rm = F)))) %>% 
  lapply(., rbindlist) %>% reduce(., cbind) %>% setNames(., paste0(colnames(.), seq(1,1,1)))

#### create the data set for the left join ####
data = cbind(data, predict)

#### Run the second stage ####
est = list(fepois(convicted ~  IV1 | year^month + id3 + TempBin + RainBin, data = data,  cluster = "id3^year"))

#### Add the cluster dimension for the bootstrap ####
data = mutate(data, 
              cluster1 = paste(id3, year, sep = "_"),
              cluster2 = paste(DistrictCodeShp, year, sep = "_"),
              cluster3 = paste(id3, sep = "_"),
              cluster4 = paste(DistrictShp, sep = "_"))

#### Create the bootstrap algorithm to estimate the standard errors ####
ivpois_est = function(data){
  # Construct the random sample
  random = list(cluster1 = rbindlist(lapply(sample(data$cluster1, length(unique(data$cluster1))), function(x) filter(data, cluster1 == x) %>% mutate(cluster = "cluster1"))),
                cluster2 = rbindlist(lapply(sample(data$cluster2, length(unique(data$cluster2))), function(x) filter(data, cluster2 == x) %>% mutate(cluster = "cluster2"))),
                cluster3 = rbindlist(lapply(sample(data$cluster3, length(unique(data$cluster3))), function(x) filter(data, cluster3 == x) %>% mutate(cluster = "cluster3"))),
                cluster4 = rbindlist(lapply(sample(data$cluster4, length(unique(data$cluster4))), function(x) filter(data, cluster4 == x) %>% mutate(cluster = "cluster4"))))
  
  # 1st stage OLS
  FirstStage = lapply(random, function(x) 
    feols(convicted ~ 1 | year^month + id3 + TempBin + RainBin  | 
            acag_pm25 ~ i(tempinv2, StateCodeShp, 1),
          data = x, mem.clean = T))
  
  
  # Getting the fitted values
  predict = lapply(FirstStage, function(x) x = x$iv_first_stage) %>% 
    lapply(., function(x) lapply(x, function(x) x = data.frame(value = predict(x, na.rm = F)))) %>% 
    lapply(., rbindlist)


  # bind fitted values to the original data
  random$cluster1 = cbind(random$cluster1, predict$cluster1)
  random$cluster2 = cbind(random$cluster2, predict$cluster2)
  random$cluster3 = cbind(random$cluster3, predict$cluster3)
  random$cluster4 = cbind(random$cluster4, predict$cluster4)
  
  # run second stage
  list(fepois(convicted ~  value | year^month + id3 + TempBin + RainBin, data = random$cluster1),
       fepois(convicted ~  value | year^month + id3 + TempBin + RainBin, data = random$cluster2),
       fepois(convicted ~  value | year^month + id3 + TempBin + RainBin, data = random$cluster3),
       fepois(convicted ~  value | year^month + id3 + TempBin + RainBin, data = random$cluster4)) %>% 
    lapply(., function(x) x = data.frame(tidy(x), n = x$nobs))
}

#### Run the 1000 bootstraps ####
bs = mclapply(vector("list", length = 1000), function(x) 
  x = ivpois_est(data = data), mc.cores = 30)

#bs = lapply(vector("list", length = 100), function(x) x = ivpois_est(data = data))

#### Extract the point estimates #####
sum = lapply(est, function(x) 
  data.frame(tidy(x), n = x$nobs, bic = BIC(x), r2 = r2(x, "pr2"))) %>% 
  rbindlist(.) 

#### Bind the bootstraps together #####
bs2 = lapply(bs, rbindlist, idcol = "cluster") %>% rbindlist(., idcol = "bs")
bs2 = mutate(bs2, cluster=paste0("cluster", cluster))

#### Compute the standard error of the bootstrapped sample ####
BsSe = bs2 %>% group_by(cluster) %>% 
  summarise(n = n(), 
            bse = sqrt((n-1)/n) * sd(estimate, na.rm = T))

#### Left join with the main data set #####
sum = mutate(BsSe, estimate = sum$estimate,
             n = sum$n, bic = sum$bic, r2 = sum$r2)

#### Compute the P-value with the bootstrapped SE ####
sum =  sum %>%  mutate(bp = 2*pnorm(abs(estimate/bse), lower.tail = F))

#### Compute the F-test of each city ####
test = lapply(FirstStage, function(x)   x = fitstat(x,"ivf1")$ivf1[1])
test = lapply(test, function(x)  x = data.frame(f_test = c(x)))
test = rbindlist(test, idcol = "term")
test = mutate(test, term = paste0("IV", term)) %>% rename(f_test = stat)

#### Add the first stage Fstatistics to the regression ####
sum = mutate(sum, test = test$f_test)

#### Transform the point estimates to the effect of inceasing PM25 by ten units ####
sum = mutate_at(sum, vars(estimate, bse), function(x) x = (exp(x) -1)*1000) 

#### See the data set ####
glimpse(sum)

#### Transform the tab object to a data frame for HTML ###
tab = matrixreg(lapply(split(sum, f = sum$cluster), function(x) 
  createTexreg("", x$estimate, x$bse, x$bp,
               gof.names = c(rep("F-test", nrow(x)),
                             rep("N.Obs", nrow(x)), 
                             rep("R2", nrow(x)), 
                             rep("BIC", nrow(x))),
               gof = c(x$test, x$n, x$r2, x$bic/1000), 
               gof.decimal = c(T,F,T,T))),
  booktabs = T, digits = 2, stars = c(0.01, 0.05, 0.1)) %>%
  as.data.frame(.)

#### Style the data frame ####
tab = tab[-1, ]; rownames(tab) = c("Estimate", "", "F-Test","N.obs", "R2", "BIC") 
tab = select(tab, -V1); colnames(tab) = c("(1)", "(2)", "(3)", "(4)")
tab[4,] = comma(as.numeric(tab[4,])); tab[6,] = comma(as.numeric(tab[6,]))

tab

#### Save the data set ####
write_rds(tab, file = "02_gen/06_results/robustness_iv_cluster_tab.rds")
write_rds(sum, file = "02_gen/06_results/robustness_iv_cluster_sum.rds")


#### Clear the space ####
rm(list = ls()); gc()