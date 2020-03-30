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
  mutate(active = confirmed - deaths - recovered,
         Date = mdy(Date)) %>% 
  pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "status", values_to = "cases") %>% 
  clean_names() 

str(dd)

# Export to .csv file.
write.csv(dd, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/NEW_COVID_Time_series_TMM.csv")





dd_pop <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/WorldPop_2018.csv")


# dd_test <- dd %>%
#   filter(., cases != 0) %>%
#   select(., -c(long, lat, province_state)) %>%
#   group_by(., country_region, status) %>% 
#   mutate(., time_ind = as.numeric(difftime(date, min(date), units = "days")))
#   # mutate(., time_diff = difftime(date, min(date), "days"))

param_crit <- 17

ddd_test <- dd %>% 
  group_by(., date, country_region, status) %>% 
  summarise(., cases_all = sum(cases)) %>% 
  left_join(., dd_pop, by = c("country_region" = "country")) %>% 
  mutate(., cases_all_diff = cases_all - lag(cases_all)) %>% 
  mutate(., cases_all_rel_ck = cases_all / population_total *100000) %>% 
  mutate(., cases_all_diff_rel_ck = cases_all_rel_ck - lag(cases_all_rel_ck)) %>%
  # filter(., cases_all != 0) %>%
  filter(., cases_all >= param_crit) %>%
  group_by(., country_region, status) %>% 
  mutate(., time_ind = as.numeric(difftime(date, min(date), units = "days"))) %>% 
  arrange(., country_region, date, status)

str(ddd_test)

p1 <- ggplot(ddd_test,
       aes(time_ind, cases_all, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void() +
  annotation_logticks() +
  scale_y_log10(); ggplotly(p1)

p1_lin <- ggplot(ddd_test,
             aes(time_ind, cases_all, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void(); ggplotly(p1_lin)

p2 <- ggplot(ddd_test,
             aes(time_ind, cases_all_diff, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void(); ggplotly(p2)

p3 <- ggplot(ddd_test,
             aes(time_ind, cases_all_rel_ck, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void() +
  scale_y_log10(); ggplotly(p3)

p3_lin <- ggplot(ddd_test,
             aes(time_ind, cases_all_rel_ck, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void(); ggplotly(p3_lin)

p4 <- ggplot(ddd_test,
             aes(time_ind, cases_all_diff_rel_ck, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void(); p4
ggplotly(p4)



# 
# lmodel_Germany <- lm(log(na.omit(filter(dd_Germany, status == "confirmed")$cases)) ~ na.omit(filter(dd_Germany, status == "confirmed")$date))

plot(filter(dd_Germany, status == "confirmed")$date, log(na.omit(filter(dd_Germany, status == "confirmed")$cases)))
plot(filter(dd_Germany, status == "confirmed")$date, filter(dd_Germany, status == "confirmed")$cases)
     