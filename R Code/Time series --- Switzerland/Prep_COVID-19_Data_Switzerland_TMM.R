# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(plotly)



## >>>>>>>>>>>>>>>>>>>>>>>
# Load CoViD-19 data from GitHub.

dd_cases <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv") %>% 
  dplyr::select(., -c(CH)) %>% 
  pivot_longer(., cols = -c(Date), names_to = "canton", values_to = "cases") %>% 
  clean_names()

dd_fatalities <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_fatalities_switzerland_openzh.csv") %>% 
  dplyr::select(., -c(CH)) %>% 
  pivot_longer(., cols = -c(Date), names_to = "canton", values_to = "fatalities") %>% 
  clean_names()

dd_hospitalized <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_hospitalized_switzerland_openzh.csv") %>% 
  dplyr::select(., -c(CH)) %>% 
  pivot_longer(., cols = -c(Date), names_to = "canton", values_to = "hospitalized") %>% 
  clean_names()

dd_icu <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_icu_switzerland_openzh.csv") %>% 
  dplyr::select(., -c(CH)) %>% 
  pivot_longer(., cols = -c(Date), names_to = "canton", values_to = "icu") %>% 
  clean_names()

dd_released <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_released_switzerland_openzh.csv") %>% 
  dplyr::select(., -c(CH)) %>% 
  pivot_longer(., cols = -c(Date), names_to = "canton", values_to = "released") %>% 
  clean_names()

dd_ventilated <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_vent_switzerland_openzh.csv") %>% 
  dplyr::select(., -c(CH)) %>% 
  pivot_longer(., cols = -c(Date), names_to = "canton", values_to = "ventilated") %>% 
  clean_names()

dd_all_cases <- dd_cases %>% 
  left_join(., dd_fatalities, by = c("date", "canton")) %>% 
  left_join(., dd_icu, by = c("date", "canton")) %>% 
  left_join(., dd_released, by = c("date", "canton")) %>% 
  left_join(., dd_ventilated, by = c("date", "canton")) 


write_csv(dd_all_cases, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- Switzerland/COVID_Time_series_Switzerland_TMM.csv")



dd_demography <- read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/demographics.csv") %>% 
  filter(., Canton != "CH") %>% 
  clean_names()

write_csv(dd_demography, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- Switzerland/COVID_Demographics_Switzerland_TMM.csv")












