# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(forcats)
library(RcppRoll)
library(zoo)
library(plotly)
library(waffle)
library(htmlwidgets)
library(extrafont)
library(ggtextures)
library(grid)
library(magick)
library(wesanderson)


#Import all fonts from your working machine
font_import()

# check that Font Awesome is really imported
fonts()[grep("Awesome", fonts())]



## >>>>>>>>>>>>>>>>>>>>>>>
# Load CoViD-19 data from GitHub.

dd_org_confirmed = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>% 
  # select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Province/State", "Lat", "Long"), names_to = "Date", values_to = "confirmed") %>% 
  clean_names(.) %>% 
  # group_by(., country_region, date, lat, long) %>% 
  group_by(., country_region, date) %>% 
  summarise(., confirmed = sum(confirmed))

dd_org_deaths = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>% 
  # select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Province/State", "Lat", "Long"), names_to = "Date", values_to = "deaths") %>% 
  clean_names(.) %>% 
  # group_by(., country_region, date, lat, long) %>% 
  group_by(., country_region, date) %>% 
  summarise(., deaths = sum(deaths))

dd_org_recovered = read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>% 
  # select(., -c("Province/State")) %>% 
  pivot_longer(., cols = -c("Country/Region", "Province/State", "Lat", "Long"), names_to = "Date", values_to = "recovered") %>% 
  clean_names(.) %>% 
  # group_by(., country_region, date, lat, long) %>% 
  group_by(., country_region, date) %>% 
  summarise(., recovered = sum(recovered))


# 
# dd_ger_confirmed <- tail(filter(dd_org_confirmed, country_region == "Germany"), 20) 
# dd_ger_deaths <- tail(filter(dd_org_deaths, country_region == "Germany"), 20) 
# 
# 
# dd_test <- dd_org_confirmed %>% 
#   left_join(., dd_org_deaths, by = c("country_region", "date")) %>% 
#   filter(., country_region == "Germany") %>% 
#   tail(., 20)
# 
# dd_tester <- dd_ger_confirmed %>% 
#   left_join(., dd_ger_deaths, by = c("country_region", "date"), copy = FALSE, keep = FALSE)




dd <- dd_org_confirmed %>% 
  left_join(., dd_org_deaths, by = c("country_region", "date"), copy = FALSE, keep = FALSE) %>% 
  left_join(., dd_org_recovered, by = c("country_region", "date"), copy = FALSE, keep = FALSE) %>% 
  mutate(active = confirmed - deaths - recovered,
         date = mdy(date)) %>% 
  pivot_longer(., cols = c(confirmed, deaths, recovered, active), names_to = "status", values_to = "cases") %>% 
  clean_names(.) %>% 
  mutate_if(., is.character, as.factor)

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
write_csv(dd, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- global/NEW_COVID_Time_series_TMM.csv")

# Include demographic data
dd_pop <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Supplementary information/WorldPop_2018.csv") %>% 
  filter(., region != "Aggregates")



dd_plus <- dd %>% 
  group_by(., date, country_region, status) %>% 
  summarise(., cases_all = sum(cases)) %>% 
  ungroup(.) %>% 
  arrange(., country_region, status, date) %>% 
  left_join(., dd_pop, by = c("country_region" = "country")) %>%  # Check that all countries are matched!!!
  group_by(., status, country_region) %>%
  mutate(., cases_all_diff = cases_all - dplyr::lag(x = cases_all, n = 1, order_by = c(date))) %>% 
  ungroup(.) %>% 
  group_by(., country_region) %>% 
  mutate(., cases_all_rel_ck = cases_all / population_total *100000) %>% 
  mutate(., cases_all_diff_rel_ck = cases_all_rel_ck - lag(cases_all_rel_ck)) %>%
  ungroup(.) %>% 
  group_by(., status, country_region) %>% 
  mutate(., cases_all_rollmean = zoo::rollmean(cases_all, k = 7, fill = NA, align = "right")) %>% 
  mutate(., cases_all_diff_rollmean = zoo::rollmean(cases_all_diff, k = 7, fill = NA, align = "right")) %>% 
  ungroup(.)
  
# dd_ger <- filter(dd, country_region == "Germany")
  
p_02 <- dd_plus %>% 
  ggplot(data = .,
         aes(x = region,
             y = cases_all,
             fill = region)) +
  geom_col() +
  coord_flip() +
  labs(title = "CoViD-19 cases for Germany") +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c(wes_palette("GrandBudapest1"), wes_palette("BottleRocket2"))) +
  theme_minimal() +
  # theme(legend.title = element_blank())
  theme(legend.position = "none"); p_02

p_03 <- dd_plus %>% 
  ggplot(data = .,
         aes(x = income,
             y = cases_all,
             fill = income)) +
  geom_col() +
  coord_flip() +
  labs(title = "CoViD-19 cases for Germany") +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c(wes_palette("GrandBudapest1"), wes_palette("BottleRocket2"))) +
  theme_minimal() +
  # theme(legend.title = element_blank())
  theme(legend.position = "none"); p_03

# Export to .csv file.
write_csv(dd_plus, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- global/NEW_COVID_Time_series_TMM_plus.csv")


param_crit <- 50

dd_cut_crit <- dd_plus %>% 
  group_by(., country_region, status) %>% 
  filter(., cases_all >= param_crit) %>%
  mutate(., time_ind = as.numeric(difftime(date, min(date), units = "days"))) %>% 
  ungroup(.)

write_csv(dd_cut_crit, paste("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- global/NEW_COVID_Time_series_TMM_cut_crit_", param_crit, ".csv", sep = ""))








sessionInfo()






# p_02 <- dd %>% 
#   filter(., country_region == "Germany" & date == max(date)) %>%
#   geom_waffle(., aes)

# Preserve sum of rounded proprtions:
# (According to https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/)
round_preserve <- function(x, digits = 0) {
  up <-  10 ^ digits
  x <-  x * up
  y <-  floor(x)
  indices <-  tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <-  y[indices] + 1
  y / up
}

img <- image_read_svg("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/Graphical resources/Icons/man.svg")

dd_waffle_country <- dd %>% 
  filter(., country_region == "US" & date == max(date)) %>%
  select(., c(status, cases)) %>% 
  filter(., status != "confirmed") %>% 
  mutate(., cases = round_preserve(cases/sum(cases)*100),
         image = list(img)) 

selected_countries <- c("Brazil", "US", "Germany", "Italy", "United Kingdom", "France")

dd_waffle_countries <- dd %>% 
  filter(., country_region %in% selected_countries & date == max(date)) %>%
  group_by(., country_region, status) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup() %>% 
  mutate(., status = fct_relevel(status, "confirmed", "active", "recovered", "deaths")) %>% 
  filter(., status != "confirmed") %>% 
  group_by(., country_region) %>% 
  # mutate(., cases = signif(cases/sum(cases)*100, 4),
  mutate(., cases = round_preserve(cases/sum(cases)*100),
         image = list(img)) %>% 
  ungroup() 




# Pictogram plots...

# ...with ggtextures
ggplot(dd_waffle_countries, aes(country_region, cases, image = image)) +
  geom_isotype_col(
    img_width = grid::unit(1, "native"), img_height = NULL,
    ncol = NA, nrow = 1, hjust = 0, vjust = 0.5, fill = "#FFFFFF"
  ) +
  coord_flip() +
  theme_minimal()

ggplot(dd_waffle_country, aes(status, cases, image = image)) +
  geom_isotype_col(
    img_width = grid::unit(1, "native"), img_height = NULL,
    ncol = NA, nrow = 1, hjust = 0, vjust = 0.5, fill = "#FFFFFF"
  ) +
  coord_flip() +
  geom_text(aes(label = status), hjust = unit(-0.5, "mm")) +
  theme_void()

# ...with waffle
show_cases_country <- function(x) {
  
  dd_waffle_country_vec <- filter(dd_waffle_countries, country_region %in% x)$cases
  names(dd_waffle_country_vec) <- unique(dd_waffle_countries$status)
  
  iron(
    waffle(dd_waffle_country_vec, 
           rows = 5, 
           colors = c("#D8A94F", "#A44A51", "#6378AC"),
           use_glyph = "male", glyph_size = 6.0 , 
           # title = "Cases for Germany", 
           legend_pos="bottom") +
      # facet_wrap(~ status) +
      labs(title = paste("Case numbers for", x),
           subtitle = "How do case types compare to each other?",
           caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)") +
      theme(text = element_text(family = "SF Pro Rounded"),
            panel.spacing = unit(20, "mm"),
            legend.position = "none",
            legend.text = element_text(colour="#666666", size = 8),
            legend.key.size = unit(5, "mm"),
            plot.caption = element_text(size = 8, hjust = 1, vjust = -0.1, colour = "#7F8182"),
            axis.title.x = element_text(margin = margin(20, 0, 0, 0)), 
            axis.title.y = element_text(margin = margin(0, 20, 0, 0)), 
            plot.title = element_text(size = 18, margin = margin(0, 0, 5, 0)),
            plot.subtitle = element_text(size = 12, margin = margin(0 ,0 ,10 ,0)))
  )
}
selected_country <- "Germany"
p_02 <- show_cases_country(selected_country);p_02

ggsave(paste("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Pictogram for ", selected_country, " - logarithmic representation", ".png", sep = ""),
       plot = p_02,
       width = 30, height = 20, units = "cm", dpi = 300)

png(paste("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Pictogram for ", selected_country, " - logarithmic representation", ".png", sep = ""),
    width = 30, height = 20, units = "cm", res = 300)
print(p_02)
dev.off()


# And another waffle chart...
dd_waffle_country_vec <- filter(dd_waffle_countries, status == "deaths")$cases
names(dd_waffle_country_vec) <- unique(dd_waffle_countries$country_region)

iron(
  waffle(dd_waffle_country_vec, 
         rows = 10, 
         # colors = c("#A44A51", "#6378AC", "#D8A94F"),
         use_glyph = "male", glyph_size = 6.5 , 
         # title = "Cases for Germany", 
         legend_pos="bottom") +
    labs(title = "Cases for Germany",
         subtitle = "How do case types compare to each other?")
)












# dd_pop <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Supplementary information/WorldPop_2018_orig.csv")
# dd_pop <- dd_pop[, 2:length(dd_pop)] %>% 
#   write_csv(., "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Supplementary information/WorldPop_2018.csv")
# 









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
