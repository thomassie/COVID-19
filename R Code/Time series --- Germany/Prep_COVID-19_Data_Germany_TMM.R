rm(list = ls())

library(tidyverse)
library(janitor)
library(rjson)
library(wiesbaden)
library(destatiscleanr)

# CoViD-19 data
dd_org_case <- read_csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv") %>% 
  clean_names()

write_csv(dd_org_case, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- Germany/COVID_Time_series_Germany_TMM.csv")


# Population data for each county.
dd_org_pop<- destatiscleanr("~/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/12411-0015.csv") %>% 
  rename(., id_landkreis = na_na,
         name_landkreis = na_na2) %>% 
  rename_all(funs(str_remove(., "stichtag_31.12."))) %>% 
  pivot_longer(., cols = -c(id_landkreis, name_landkreis), names_to = "year", values_to = "population") %>% 
  filter(., !is.na(population)) %>% 
  filter(., !str_detect(name_landkreis, "\\(b")) %>% 
  group_by(., name_landkreis) %>% 
  filter(., year == max(year)) %>% 
  ungroup() %>% 
  rename(., year_most_recent = year) %>% 
  separate(., name_landkreis, into = c("name", "type"), sep = "\\. ", remove = TRUE) %>% 
  mutate(., type = str_remove(type, "\\((.*)$"))

str(dd_org_pop)

write_csv(dd_org_pop, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- Germany/COVID_Demographics_Germany_TMM.csv")




# # My credentials:
# cred <- c(user = "DE2HO5NO4X", password = "TomDestatis-79", db = "de")
# 
# test_login(genesis=cred)
# 
# d <- retrieve_datalist(tableseries="12411-0018", cred) 
