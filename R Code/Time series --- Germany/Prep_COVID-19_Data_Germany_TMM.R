rm(list = ls())

library(tidyverse)
library(janitor)
library(rjson)
library(geojsonR)
library(wiesbaden)
library(lubridate)


# CoViD-19 data
dd_case <- read_csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv") %>% 
  clean_names()

write_csv(dd_case, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- Germany/COVID_Time_series_Germany_TMM.csv")


dd_hosp <- read_csv("https://raw.githubusercontent.com/entorb/COVID-19-Coronavirus-German-Regions/master/data/de-divi/downloaded/2020-10-14.csv")

dd_rki_recent <- read_csv("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv")


file_js = FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson")
dd_rki_all <- unlist(file_js$features)

head(dd_rki_all)




dd_pop <- read_csv2("~/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/12411-0015_flat.csv") %>% 
  janitor::clean_names(.) %>% 
  dplyr::select(., -c(statistik_code, statistik_label, zeit_code, zeit_label, x1_merkmal_code, x1_merkmal_label)) %>% 
  rename(., id_landkreis = x1_auspraegung_code,
         name_landkreis = x1_auspraegung_label,
         population = bevstd_bevoelkerungsstand_anzahl) %>% 
  mutate(., year = year(dmy(as.character(zeit)))) %>% 
  separate(., name_landkreis, into = c("name", "type"), sep = "\\, ", remove = TRUE) %>% 
  filter(., !is.na(population)) %>% 
  # filter(., !str_detect(type, "\\(b")) %>% 
  group_by(id_landkreis) %>% 
  filter(., population != "-") %>% 
  filter(., year == max(year) & population != "-") %>% 
  ungroup() %>% 
  rename(., year_most_recent = year) %>% 
  dplyr::select(., -zeit) %>% 
  arrange(., id_landkreis)


length(unique(dd_pop$id_landkreis))


# # Population data for each county.
# dd_org_pop <- desstatiscleanr("~/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/12411-0015_flat.csv") %>% 
#   rename(., id_landkreis = na_na,
#          name_landkreis = na_na2) %>% 
#   rename_all(funs(str_remove(., "stichtag_31.12."))) %>% 
#   pivot_longer(., cols = -c(id_landkreis, name_landkreis), names_to = "year", values_to = "population") %>% 
#   filter(., !is.na(population)) %>% 
#   filter(., !str_detect(name_landkreis, "\\(b")) %>% 
#   group_by(., name_landkreis) %>% 
#   filter(., year == max(year)) %>% 
#   ungroup() %>% 
#   rename(., year_most_recent = year) %>% 
#   separate(., name_landkreis, into = c("name", "type"), sep = "\\. ", remove = TRUE) %>% 
#   mutate(., type = str_remove(type, "\\((.*)$"))
# 
# str(dd_org_pop)

write_csv(dd_pop, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- Germany/COVID_Demographics_Germany_TMM.csv")

str(dd_pop)


sessionInfo()


# # My credentials:
# cred <- c(user = "DE2HO5NO4X", password = "TomDestatis-79", db = "de")
# 
# test_login(genesis=cred)
# 
# d <- retrieve_datalist(tableseries="12411-0018", cred) 
