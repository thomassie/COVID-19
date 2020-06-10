# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(wesanderson)



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
head(dd)


# Quick graphical check.
p_01 <- ggplotly(dd %>% 
                   filter(., country_region == "Germany") %>% 
                   ggplot(aes(x = date, y = cases, colour = status)) +
                   geom_line() +
                   labs(title = "CoViD-19 cases for Germany") +
                   xlab("") +
                   ylab("") +
                   scale_colour_manual(values = c("#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                   theme_minimal() +
                   theme(legend.title = element_blank())); p_01


# Export to .csv file.
write.csv(dd, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- global/NEW_COVID_Time_series_TMM.csv")

sessionInfo()


single_mutate <- quote({
  dd <- dd_org_confirmed %>% 
    left_join(., dd_org_deaths, by = c("Country/Region", "Province/State", "Date", "Lat", "Long"), copy = FALSE, keep = FALSE) %>% 
    left_join(., dd_org_recovered, by = c("Country/Region", "Province/State", "Date", "Lat", "Long"), copy = FALSE, keep = FALSE) %>% 
    mutate(active = confirmed - deaths - recovered,
           Date = mdy(Date)) %>% 
           # country_check = ifelse("Country/Region" %in% "Germany", "Yes, it's Germany!", "Unfortunately not, sorry!"),
           # counry_check_diff = ifelse(grepl("not", country_check), "hot")) %>% 
    pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "status", values_to = "cases") %>% 
    clean_names() 
})

multiple_mutate <- quote({
  dd <- dd_org_confirmed %>% 
    left_join(., dd_org_deaths, by = c("Country/Region", "Province/State", "Date", "Lat", "Long"), copy = FALSE, keep = FALSE) %>% 
    left_join(., dd_org_recovered, by = c("Country/Region", "Province/State", "Date", "Lat", "Long"), copy = FALSE, keep = FALSE) %>% 
    mutate(active = confirmed - deaths - recovered) %>% 
    mutate(Date = mdy(Date)) %>% 
    # mutate(country_check = ifelse("Country/Region" %in% "Germany", "Yes, it's Germany!", "Unfortunately not, sorry!")) %>% 
    # mutate(counry_check_diff = ifelse(grepl("not", country_check), "hot")) %>% 
    pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "status", values_to = "cases") %>% 
    clean_names() 
})

microbenchmark::microbenchmark(single_mutate, multiple_mutate)

# 
# # Experimental... ;)
# library(gganimate)
# library(viridis)
# 
# plot <- dd %>% 
#   filter(., status == "confirmed" & date == "2020-03-22") %>% 
#   ggplot(aes(x = cases, fill = cases)) +
#   geom_histogram(bins = 100) +
#   theme_minimal() +
#   scale_y_log10() +
#   scale_x_continuous(trans = 'log2') 
# # scale_x_log10() 
# # scale_size(guide = FALSE) # no legend for size
# 
# plot <- dd %>% 
#   filter(., status == "confirmed") %>% 
#   ggplot(aes(x = cases, fill = cases)) +
#   geom_density() +
#   theme_minimal() +
#   scale_y_log10() +
#   scale_x_continuous(trans = 'log2')+
#   coord_cartesian(ylim = c(0.000001, 1))
# # scale_x_log10()
# 
# plot + transition_time(date) +
#   labs(title = "Date: {frame_time}", 
#        wrap = FALSE)
# 
# # 
# # plot + transition_states(states = date,
# #                          transition_length = 3,
# #                          state_length = 1,
# #                          wrap = FALSE)
# 
# ggplotly(plot)
# 
# 
