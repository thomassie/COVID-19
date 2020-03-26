# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)



## >>>>>>>>>>>>>>>>>>>>>>>
# Load CoViD-19 data from GitHub.
dd_org_confirmed = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>% 
  select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Lat", "Long"), names_to = "Date", values_to = "confirmed")

dd_org_deaths = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>% 
  select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Lat", "Long"), names_to = "Date", values_to = "deaths") 

# Build combined dataset.
dd <- dd_org_confirmed %>% 
  mutate(., deaths = dd_org_deaths$deaths) %>% 
  pivot_longer(., cols = c(confirmed, deaths), names_to = "Status", values_to = "Cases") %>% 
  clean_names()

# Export to .csv file.
write.csv(dd, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/NEW_COVID_Time_series_TMM.csv")


