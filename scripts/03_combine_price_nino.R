######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)

price <- readRDS(here("data", "processed", "annual_price_by_spp.rds"))
nino <- readRDS(here("data", "processed", "annual_nino34.rds"))


data <- left_join(price, nino, by = c("year_cut" = "year")) %>% 
  rename(ano = year_cut, nombre_comun = new_name, precio_promedio = mean_p, precio_sd = sd_p, n_obs = n, nino34_promedio = nino34_m)


write.csv(data, here("data", "processed", "precios_y_nino.csv"))
