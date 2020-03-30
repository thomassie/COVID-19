# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)



## >>>>>>>>>>>>>>>>>>>>>>>
# Load CoViD-19 data from GitHub.
dd_org_confirmed = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>% 
  # select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Province/State", "Lat", "Long"), names_to = "Date", values_to = "confirmed")

dd_org_deaths = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>% 
  # select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Province/State", "Lat", "Long"), names_to = "Date", values_to = "deaths") 

dd_org_recovered = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>% 
  # select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Province/State", "Lat", "Long"), names_to = "Date", values_to = "recovered") 

dd <- dd_org_confirmed %>% 
  left_join(., dd_org_deaths, by = c("Country/Region", "Province/State", "Date", "Lat", "Long"), copy = FALSE, keep = FALSE) %>% 
  left_join(., dd_org_recovered, by = c("Country/Region", "Province/State", "Date", "Lat", "Long"), copy = FALSE, keep = FALSE) %>% 
  mutate(active = confirmed - deaths - recovered) %>% 
  pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "status", values_to = "cases") %>% 
  clean_names()


# # Build combined dataset.
# dd <- dd_org_confirmed %>% 
#   mutate(., deaths = dd_org_deaths$deaths, recovered = dd_org_recovered$recovered) %>% 
#   mutate(., active = confirmed - deaths - recovered) %>% 
#   pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "Status", values_to = "Cases")

# Export to .csv file.
write.csv(dd, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/COVID_Time_series_TMM.csv")





## >>>>>>>>>>>>>>>>>>>>>>>
## Load supplementary data from Worldbank from local folder.

# GDP (US$).
dd_org_gdp = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_GDP.csv",
                      skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "GDP_current_USDollar") %>% 
  mutate(description_gdp = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_GDP_Meta.csv")$SOURCE_NOTE)

# Life expectancy at birth in years.
dd_org_LifeExpect = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_LifeExpectancy.csv",
                             skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "LifeExpectBirth") %>% 
  mutate(description_LifeExpect = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_LifeExpectancy_Meta.csv")$SOURCE_NOTE)

# Health expenditure as % GDP.
dd_org_HealthExp_relGDP = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_HealthExpenditure.csv",
                                   skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "HealthExp_relGDP") %>% 
  mutate(description_HealthExp_relGDP = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_HealthExpenditure_Meta.csv")$SOURCE_NOTE)

# Health expenditure per capita.
dd_org_HealthExp_perCapita = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_HealthExpenditurePerCapita.csv",
                                      skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "HealthExp_perCapita") %>% 
  mutate(description_HealthExp_perCapita = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_HealthExpenditurePerCapita_Meta.csv")$SOURCE_NOTE)

# Total population numbers.
dd_org_TotalPop = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_TotalPop.csv",
                           skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "TotalPop") %>% 
  mutate(description_TotalPop = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_TotalPop_Meta.csv")$SOURCE_NOTE)

# Fertility rare: briths per woman.
dd_org_FertilityRate = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_FertilityRate.csv",
                                skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "FertilityRate") %>% 
  mutate(description_FertilityRate = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_FertilityRate_Meta.csv")$SOURCE_NOTE)

# Access to electricity in %.
dd_org_AccessElect = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AccessElectricity.csv",
                              skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "AccessElect") %>% 
  mutate(description_AccessElect = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AccessElectricity_Meta.csv")$SOURCE_NOTE)

# Age dependent ratio of old people in %.
dd_org_AgeDepRatio_old = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AgeDepRatio_old.csv",
                                  skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "AgeDepRatio_old") %>% 
  mutate(description_AgeDepRatio_old = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AgeDepRatio_old_Meta.csv")$SOURCE_NOTE)

# Age dependent ratio of young people in %.
dd_org_AgeDepRatio_young = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AgeDepRatio_young.csv",
                                    skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "AgeDepRatio_young") %>% 
  mutate(description_AgeDepRatio_young = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AgeDepRatio_young_Meta.csv")$SOURCE_NOTE)

# Electric power consumption.
dd_org_ElectPowerConsump = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_ElectPowerConsumpt.csv",
                                    skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "ElectPowerConsump") %>% 
  mutate(description_ElectPowerConsump = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_ElectPowerConsumpt_Meta.csv")$SOURCE_NOTE)

# GINI index.
dd_org_GINI = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_GINI.csv",
                       skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "GINI") %>% 
  mutate(description_GINI = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_GINI_Meta.csv")$SOURCE_NOTE)

# GINI index.
dd_org_TranspCorrupt = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_TranspCorrupt.csv",
                                skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "TranspCorrupt") %>% 
  mutate(description_TranspCorrupt = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_TranspCorrupt_Meta.csv")$SOURCE_NOTE)

# Counry metadata.
dd_org_meta = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_Metadata_Countries.csv",
                       skip = 0) %>% 
  select(., c("Country Code", "Region", "IncomeGroup"))

# Compile supplementary information.
dd_supple <- dd_org_TotalPop %>% 
  left_join(., dd_org_LifeExpect, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>%
  left_join(., dd_org_HealthExp_relGDP, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_HealthExp_perCapita, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_gdp, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_FertilityRate, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_AccessElect, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_AgeDepRatio_old, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_AgeDepRatio_young, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_ElectPowerConsump, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_GINI, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_TranspCorrupt, by = c("Country Name", "Country Code", "Year"), keep = FALSE) %>% 
  left_join(., dd_org_meta, by = c("Country Code" = "Country Code"), keep = FALSE)

# Export to .csv file.
write.csv(dd_supple, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Countries_SupplementaryInformation_TMM.csv")
