######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(data.table)
library(tidyverse)

my_read <- function(path){
  fread(path,
        col.names = c("vessel_rnpa", "vessel_name",
                      "landing_site_key", "landing_site",
                      "eu_rnpa", "economic_unit",
                      "state", "office_key",
                      "office_name", "receipt_type",
                      "receipt_id", "receipt_date",
                      "origin", "fishing_site_key",
                      "fishing_site_name", "n_vessels",
                      "month_cut", "year_cut",
                      "period_start", "period_end",
                      "period_length", "period_effective_dates",
                      "fishing_zone_type", "acuaculture_production",
                      "permit_number", "permit_issue_date",
                      "permit_expiration_date", "main_species_group",
                      "species_key", "species_name",
                      "landed_weight", "live_weight",
                      "price", "value",
                      "coastline"),
        select = 1:35,
        colClasses = "character",
        na.strings = c("NULL", "NA"),
        blank.lines.skip = TRUE) %>% 
    janitor::clean_names() %>% 
    mutate(source = basename(path),
           landed_weight = as.numeric(landed_weight),
           value = as.numeric(value))
}


files <- here("data", "raw", "conapesca") %>% 
  list.files(full.names = T)

landings <- map_dfr(files, my_read)

short <- landings %>% 
  tibble() %>% 
  filter(state %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
         coastline == "PACIFICO",
         office_name %in% c("BAHIA ASUNCION", "BAHIA TORTUGAS", "GUERRERO NEGRO", "VILLA DE JESUS MARIA"),
         main_species_group %in% c("TIBURON", "CAZON", "RAYA Y SIMILARES"),
         str_detect(species_name, "FCO."),
         !str_detect(species_name, "FILETE")) %>% 
  mutate(landing_site = str_remove_all(landing_site, "[:punct:]"),
         landing_site = str_remove_all(landing_site, "BCN"),
         landing_site = str_remove_all(landing_site, "BCS"),
         landing_site = str_remove_all(landing_site, "BCNORTE"),
         landing_site = str_remove_all(landing_site, "BCSUR"),
         landing_site = str_squish(landing_site),
         new_name = str_remove_all(species_name, "DE LEY|DESV. FCO.|DESV. Y DESC. FCO.|\\((.*?)\\)|ENT. FCO.|DESV. Y  DESC. FCO.|DESV. Y DESC.  FCO."),
         new_name = str_replace_all(new_name, "Ó", "O"),
         new_name = ifelse(new_name == "TIBURON ", "OTROS", new_name),
         new_name = str_remove_all(new_name, "TIBURON"),
         new_name = str_replace_all(new_name, "OTROS", "TIBURON"),
         new_name = str_squish(new_name),
         new_name = case_when(new_name == "COLUDO" ~ "ZORRO",
                              new_name == "TUNERO" ~ "SEDOSO",
                              T ~ new_name),
         new_name = str_to_sentence(new_name),
         price = as.numeric(price),
         price = ifelse(price > 50, price / 10, price),
         year_cut = as.numeric(year_cut),
         month_cut = case_when(month_cut == "ENERO" ~ 1,
                               month_cut == "FEBRERO" ~ 2,
                               month_cut == "MARZO" ~ 3,
                               month_cut == "ABRIL" ~ 4,
                               month_cut == "MAYO" ~ 5,
                               month_cut == "JUNIO" ~ 6,
                               month_cut == "JULIO" ~ 7,
                               month_cut == "AGOSTO" ~ 8,
                               month_cut == "SEPTIEMBRE" ~ 9,
                               month_cut == "OCTUBRE" ~ 10,
                               month_cut == "NOVIEMBRE" ~ 11,
                               month_cut == "DICIEMBRE" ~ 12)) %>% 
  select(landing_site, office_name, month_cut, year_cut, main_species_group, species_name, new_name, landed_weight, price) %>% 
  group_by(new_name) %>% 
  mutate(n = n_distinct(year_cut)) %>% 
  ungroup() %>% 
  filter(n >= 6)


ggplot(data = short, aes(x = year_cut, y = price)) +
  geom_jitter(height = 0, width = 0.25, size = 0.1, alpha = 0.5, aes(color = new_name)) +
  stat_summary(aes(group = new_name, color = new_name), geom = "line", fun = mean, fun.args = list(na.rm = T, mult = 1, size = 0.1)) +
  stat_summary(geom = "pointrange", fun.data = mean_sdl, fun.args = list(na.rm = T, mult = 1),
               fill = "steelblue", shape = 21, size = 1) +
  theme_bw() +
  labs(x = "Año",
       y = "Precio (Pesos / Kg)",
       color = "Especie",
       title = "Histórico de precios de elasmobranquios en BC y BCS",
       subtitle = "Cada punto es un reporte de arribo. Cada lína con color es el promedio anual por especie.\nLos puntos azules muestran el promedio anual general y desviación estandar.") +
  scale_x_continuous(breaks = c(2007:2018), labels = c(2007:2018))

prices <- short %>% 
  group_by(year_cut, new_name) %>% 
  summarize(mean_p = mean(price, na.rm = T),
            sd_p = sd(price, na.rm = T),
            n = n()) %>% 
  ungroup() %>% 
  complete(year_cut, new_name)

saveRDS(prices, file = here("data", "processed", "annual_price_by_spp.rds"))
