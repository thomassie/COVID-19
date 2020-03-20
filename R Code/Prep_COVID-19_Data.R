# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)



# Load CoViD-19 data from GitHub.
dd_org_confirmed = read_csv(url("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")) %>% 
  select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Lat", "Long"), names_to = "Date", values_to = "confirmed")

dd_org_deaths = read_csv(url("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")) %>% 
  select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Lat", "Long"), names_to = "Date", values_to = "deaths") 

dd_org_recovered = read_csv(url("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")) %>% 
  select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Lat", "Long"), names_to = "Date", values_to = "recovered") 

# Build combined dataset.
dd <- dd_org_confirmed %>% 
  mutate(., deaths = dd_org_deaths$deaths, recovered = dd_org_recovered$recovered) %>% 
  mutate(., active = confirmed - deaths - recovered) %>% 
  pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "Status", values_to = "Cases")

# Export to .csv file.
write.csv(dd, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/COVID_Time_series_TMM.csv")


## Load supplementary data from Worldbank from local folder.

# GDP (US$).
dd_org_gdp = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_GDP.csv",
                      skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "GDP_current_USDollar")

# Life expectancy at birth in years.
dd_org_LifeExpect = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_LifeExpectancy.csv",
                      skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "LifeExpectBirth")

# Health expenditure as % GDP.
dd_org_HealthExp_relGDP = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_HealthExpenditure.csv",
                             skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "HealthExp_relGDP")

# Health expenditure per capita.
dd_org_HealthExp_perCapita = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_HealthExpenditurePerCapita.csv",
                            skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "HealthExp_perCapita")

# Total population numbers.
dd_org_TotalPop = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_TotalPop.csv",
                                      skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "TotalPop")

# Fertility rare: briths per woman.
dd_org_FertilityRate = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_FertilityRate.csv",
                           skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "FertilityRate")

# Access to electricity in %.
dd_org_AccessElect = read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Demographic data/Worldbank_AccessElectricity.csv",
                                skip = 3) %>% 
  select(., -c("Indicator Code", "Indicator Name")) %>% 
  rename(., "2020" = "X65") %>% 
  pivot_longer(., cols = -c("Country Name", "Country Code"), names_to = "Year", values_to = "AccessElect")

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
  left_join(., dd_org_meta, by = c("Country Code" = "Country Code"), keep = FALSE)

# Export to .csv file.
write.csv(dd_supple, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Countries_SupplementaryInformation_TMM.csv")




