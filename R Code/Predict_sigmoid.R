# Clear workspace
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(plotly)
library(drc)
library(growthmodels)



## >>>>>>>>>>>>>>>>>>>>>>>
# Load data from prepping file.

dd <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/NEW_COVID_Time_series_TMM.csv")
dd_pop <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Supplementary information/WorldPop_2018.csv")


param_crit <- 20

dd_base <- dd %>% 
  group_by(., date, country_region, status) %>% 
  summarise(., cases_all = sum(cases)) %>% 
  left_join(., dd_pop, by = c("country_region" = "country")) %>% 
  mutate(., cases_all_diff = cases_all - lag(cases_all)) %>% 
  mutate(., cases_all_rel_ck = cases_all / population_total *100000) %>% 
  mutate(., cases_all_diff_rel_ck = cases_all_rel_ck - lag(cases_all_rel_ck)) %>%
  # filter(., cases_all != 0) %>%
  group_by(., country_region, status) %>% 
  # filter(., cases_all >= min(filter(., country_region == "China")$cases_all)) %>%
  filter(., cases_all >= param_crit) %>%
  mutate(., time_ind = as.numeric(difftime(date, min(date), units = "days"))) %>% 
  arrange(., country_region, date, status) %>% 
  ungroup(.)

str(dd_base)




p1 <- ggplot(dd_base,
             aes(time_ind, cases_all, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void() +
  annotation_logticks() +
  scale_y_log10(); ggplotly(p1)

p1_lin <- ggplot(dd_base,
                 aes(time_ind, cases_all, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void(); ggplotly(p1_lin)

p2 <- ggplot(dd_base,
             aes(time_ind, cases_all_diff, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void(); ggplotly(p2)

p3 <- ggplot(dd_base,
             aes(time_ind, cases_all_rel_ck, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  facet_wrap(~ status) +
  theme_void() +
  scale_y_log10(); ggplotly(p3)




# Graphical settings...

# Colour pallettes
pal_01 <- c("", "", "", "", "")
pal_02 <- c("", "", "", "", "")
pal_03 <- c("", "", "", "", "")

# Custom theme
themes_TMM_01 <- theme_bw() +
  theme(
    # axis.text = element_text(family = "Varela Round"),
    axis.text.x = element_text(size = 11, colour = "#3C3C3C", face = "bold", vjust = 1),
    axis.text.y = element_text(size = 11, colour = "#3C3C3C", face = "bold", vjust = 0.5),
    axis.ticks = element_line(colour = "#D7D8D8", size = 0.2),
    axis.ticks.length = unit(5, "mm"),
    axis.line = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0, vjust = -0.8, colour = "#3C3C3C", size = 20),
    plot.subtitle = element_text(hjust = 0, vjust = -1, colour = "#3C3C3C", size = 11),
    plot.caption = element_text(size = 8, hjust = 1.6, vjust = -0.1, colour = "#7F8182"),
    panel.background = element_rect(fill = "#FAFAF2"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#FAFAF2", colour = "#FAFAF2"),
    panel.grid.major = element_line(colour = "#D7D8D8", size = 0.2),
    panel.grid.minor = element_line(colour = "#D7D8D8", size = 0.2),
    legend.title = element_blank(),
    legend.justification=c(0,1),
    legend.position=c(1.02, 0.3),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    complete = FALSE)



p_test <- ggplot(dd_base,
                 aes(time_ind, cases_all, group = as.factor(country_region))) +
  geom_line(aes(colour = status)) +
  scale_y_log10() +
  facet_wrap(~ status, scales = "free") +
  labs(x = "Days", 
       y = "Reported cases",
       title = "How do countries compare for each status?",
       subtitle = expression("Here goes the subtitle!"),
       caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The world Bank") +
  themes_TMM_01; p_test
ggplotly(p_test)







#Fit sigmoidal model!!!
t_max <- 80
selected_country <- "Spain"
selected_status <- "deaths"
time_pred <- seq(0, t_max, 1)

dd_model <- filter(dd_base, country_region == selected_country & status == selected_status) %>% 
  dplyr::select(., time_ind, cases_all)


# logistic growth (4 parameters)
model_L.4 <- drm(cases_all ~ time_ind, fct = L.4(fixed = c(NA, min(dd_model$cases_all), NA, NA)), data = dd_model)
summary(model_L.4)

plot(dd_model)
plot(model_L.4, add = TRUE, col = "red")

pred_L.4 <- as.data.frame(predict(
  model_L.4,
  newdata = data.frame(time_ind = time_pred),
  interval = "confidence", level = 0.95))
pred_L.4 <- pred_L.4 %>% 
  mutate(time_pred = time_pred,
         model_used = "L.4",
         model_name = "logistic (4 param.)")


# log-logistic growth (3 parameters)
model_LL.3 <- drm(cases_all ~ time_ind, fct = LL.3(fixed = c(NA, NA, NA)), data = dd_model)
summary(model_LL.3)

plot(dd_model)
plot(model_LL.3, add = TRUE, col = "blue")

pred_LL.3 <- as.data.frame(predict(
  model_LL.3,
  newdata = data.frame(time_ind = time_pred),
  interval = "confidence", level = 0.95))
pred_LL.3 <- pred_LL.3 %>% 
  mutate(time_pred = time_pred,
         model_used = "LL.3",
         model_name = "log-logistic (3 param.)")


# log-logistic growth (4 parameters)
model_LL.4 <- drm(cases_all ~ time_ind, fct = LL.4(fixed = c(NA, min(dd_model$cases_all), NA, NA)), data = dd_model)
summary(model_LL.4)

plot(dd_model)
plot(model_LL.4, add = TRUE, col = "blue")

pred_LL.4 <- as.data.frame(predict(
  model_LL.4,
  newdata = data.frame(time_ind = time_pred),
  interval = "confidence", level = 0.95))
pred_LL.4 <- pred_LL.4 %>% 
  mutate(time_pred = time_pred,
         model_used = "LL.4",
         model_name = "log-logistic (4 param.)")


# log-logistic growth (5 parameters)
model_LL.5 <- drm(cases_all ~ time_ind, fct = LL.5(fixed = c(NA, min(dd_model$cases_all), NA, NA, NA)), data = dd_model)
summary(model_LL.5)

plot(dd_model)
plot(model_LL.5, add = TRUE, col = "yellow")

pred_LL.5 <- as.data.frame(predict(
  model_LL.5,
  newdata = data.frame(time_ind = time_pred),
  interval = "confidence", level = 0.95))
pred_LL.5 <- pred_LL.5 %>% 
  mutate(time_pred = time_pred,
         model_used = "LL.5",
         model_name = "log-logistic (5 param.)")


# Weibull growth (type 1)
model_W1.4 <- drm(cases_all ~ time_ind, fct = W1.4(fixed = c(NA, min(dd_model$cases_all), NA, NA)), data = dd_model)
summary(model_W1.4)

plot(dd_model)
plot(model_W1.4, add = TRUE, col = "green")

pred_W1.4 <- as.data.frame(predict(
  model_W1.4,
  newdata = data.frame(time_ind = time_pred),
  interval = "confidence", level = 0.95))
pred_W1.4 <- pred_W1.4 %>% 
  mutate(time_pred = time_pred,
         model_used = "W1.4",
         model_name = "Weibull (type 1)")

# Weibull growth (type 2)
model_W2.4 <- drm(cases_all ~ time_ind, fct = W2.4(fixed = c(NA, min(dd_model$cases_all), NA, NA)), data = dd_model)
summary(model_W2.4)

plot(dd_model)
plot(model_W2.4, add = TRUE, col = "brown")

pred_W2.4 <- as.data.frame(predict(
  model_W2.4,
  newdata = data.frame(time_ind = time_pred),
  interval = "confidence", level = 0.95))
pred_W2.4 <- pred_W2.4 %>% 
  mutate(time_pred = time_pred,
         model_used = "W2.4",
         model_name = "Weibull (type 2)")


dd_predicted <- bind_rows(pred_L.4, pred_LL.3, pred_LL.4, pred_LL.5, pred_W1.4, pred_W2.4) %>% 
  clean_names()



plot_pred_log <- ggplot() +
  geom_point(data = dd_model,
             aes(x = time_ind, y = cases_all), colour = "#5A6D70") +
  geom_line(data = dd_predicted, 
            aes(x = time_pred, y = prediction, colour = model_name)) +
  scale_y_log10() +
  geom_hline(yintercept = max(dd_model$cases_all)) +
  # facet_wrap(~ model_name) +
  theme_TMM_01_light +
  scale_colour_manual(values = palettes_long$pals_01,
                      aesthetics = c("colour", "fill")) +
  labs(x = "Days", 
       y = "log(Reported cases)",
       title = "Comparing sigmoid models",
       subtitle = expression("How do logistic, log-logistic and Weibull perform in terms of matching cases?"),
       caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank"); plot_pred_log
ggplotly(plot_pred_log)

plot_pred_lin <- ggplot() +
  geom_point(data = dd_model,
             aes(x = time_ind, y = cases_all), colour = "#5A6D70") +
  geom_line(data = dd_predicted, 
            aes(x = time_pred, y = prediction, colour = model_name)) +
  geom_hline(yintercept = max(dd_model$cases_all)) +
  # facet_wrap(~ model_name) +
  # scale_colour_manual(dd_predicted$model_name, c("#45EE78", "#99EF12", "#01EEF9")) +
  themes_TMM_01 +
  labs(x = "Days", 
       y = "Reported cases",
       title = "Comparison of models",
       subtitle = expression("How do various sigmoid models describe reported cases?"),
       caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank"); plot_pred_lin
ggplotly(plot_pred_lin)



# ------------------------------------------------------------

plot_pred_lin <- ggplot() +
  geom_hline(yintercept = max(dd_model$cases_all)) +
  geom_line(data = dd_predicted, 
            aes(x = time_pred, y = prediction, colour = model_name)) +
  geom_point(data = dd_model,
             aes(x = time_ind, y = cases_all), 
             fill = "#3476A2", colour = "transparent", alpha = 0.5,
             shape = 19, size = 3) +
  # facet_wrap(~ model_name) +
  # scale_colour_manual(dd_predicted$model_name, c("#45EE78", "#99EF12", "#01EEF9")) +
  theme_TMM_01_light +
  scale_colour_manual(values = palettes_long$pals_01,
                      aesthetics = c("colour", "fill")) +
  labs(x = "Days", 
       y = "",
       title = "Comparison of models",
       subtitle = expression("How do various sigmoid models describe reported cases?"),
       caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank"); plot_pred_lin


ggsave("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Predicted deaths.png",
       plot = plot_pred_lin,
       width = 25, height = 20, units = "cm", dpi = 300,)




mselect(model_LL.4, list(L.4(), LL.3(), LL.5(), W1.4(), W2.4()))
mselect(model_LL.4)



dd_saturation <- filter(dd_base, time_ind >= 60)






dd_selected <- filter(dd_base, country_region == selected_country & status == selected_status) %>% 
  dplyr::select(., time_ind, cases_all)

step_size <- 5

dd_pred <- list()
dd_pred_max <-  tibble(model = "", step = 0, time = 0, max_value = 0, aic = 0)

model_predictions_temp <- list()
model_predictions      <- list()

dd_actual_cases_temp <-  list()


# for (n in 1:length(model_type)) {
for (n in 1:6) {
  
  for (i in 1:10) {
    
    # Define the set of models used.
    dd_temp <- filter(dd_selected, time_ind <= i*step_size)
    model_type <- list(m_L.4  = drm(cases_all ~ time_ind, fct = L.4(fixed = c(NA, min(dd_temp$cases_all), NA, NA)), data = dd_temp),
                       m_LL.3 = drm(cases_all ~ time_ind, fct = LL.3(fixed = c(NA, NA, NA)), data = dd_temp),
                       m_LL.4 = drm(cases_all ~ time_ind, fct = LL.4(fixed = c(NA, min(dd_temp$cases_all), NA, NA)), data = dd_temp),
                       m_LL.5 = drm(cases_all ~ time_ind, fct = LL.5(fixed = c(NA, min(dd_temp$cases_all), NA, NA, NA)), data = dd_temp),
                       m_W1.4 = drm(cases_all ~ time_ind, fct = W1.4(fixed = c(NA, min(dd_temp$cases_all), NA, NA)), data = dd_temp),
                       m_W2.4 = drm(cases_all ~ time_ind, fct = W2.4(fixed = c(NA, min(dd_temp$cases_all), NA, NA)), data = dd_temp))
    
    # The model used in for step i.
    model_temp <- model_type[[n]]
    
    dd_pred_max[i, "model"]     <- names(model_type[n])
    dd_pred_max[i, "time"]      <- max(dd_temp$time_ind)
    dd_pred_max[i, "max_value"] <- coef(model_temp)[2]
    dd_pred_max[i, "step"]      <- i
    dd_pred_max[i, "aic"]       <- mselect(model_temp)["IC"]
    
    # Make predictions for each model and each time step.
    # That is, this gives the time series based on the respective time step (i*step_size) and model applied.
    pred_model_temp <- as.data.frame(predict(
      model_temp,
      newdata = data.frame(time_ind = time_pred),
      interval = "confidence", level = 0.95))
    pred_model_temp <- pred_model_temp %>% 
      mutate(time_pred = time_pred,
             model_used = names(model_type[n]),
             step = i,
             time_passed = i*10,
             max_value = coef(model_temp)[2],
             aic = mselect(model_temp)["IC"],
             status = selected_status)
    
    # Collect all predicted time series in a list.
    model_predictions_temp[[i]] <- pred_model_temp
    
    # To get the actual cases for each time step.
    dd_actual_cases_temp[[i]] <- dd_temp %>% 
      mutate(step = i)
    
  }
  
  dd_pred[[names(model_type[n])]] <- dd_pred_max
  
  model_predictions[[n]] <- as_tibble(reduce(model_predictions_temp, rbind))
}


dd_pred_coef <- as_tibble(reduce(dd_pred, rbind)) %>% 
  clean_names()
write_csv(dd_pred_coef, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/pred_coefficients.csv")

dd_model_pred <- as_tibble(reduce(model_predictions, rbind)) %>% 
  clean_names()
write_csv(dd_model_pred, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/model_predictions.csv")

dd_actual_cases <- as_tibble(reduce(dd_actual_cases_temp, rbind)) %>% 
  clean_names()
write_csv(dd_actual_cases, "/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/actual_cases.csv")



ggplot(data = dd_model_pred) +
  geom_line(aes(x = time_pred, y = max_value),
            linetype = "dotted",
            size = 0.5, colour = "#3B4457") +
  geom_text(aes(x = min(time_pred), y = max_value, 
                label = paste("max =", round((max_value),0),"&", "AIC", "=", round((aic), 0))), 
            check_overlap = TRUE, size = 2, vjust = -0.8, hjust = -0.15) +
  geom_ribbon(aes(x = time_pred, y = prediction, ymin = lower, ymax = upper,
                  fill = model_used), 
              alpha = 0.5) +
  geom_point(data = dd_actual_cases,
             aes(x = time_ind, y = cases_all, group = step),
             fill = "#FFFFFF", colour = "transparent", alpha = 0.7,
             shape = 19, size = 3) +
  # geom_line(data = dd_actual_cases,
  #           aes(x = time_ind, y = cases_all, group = step), 
  #           colour = "#FFFFFF", colour = "transparent", alpha = 0.9,
  #           size = 3) +
  geom_line(aes(x = time_pred, y = prediction, colour = model_used)) +
  scale_y_log10() +
  # facet_wrap(~ model_name) +
  # scale_colour_manual(dd_predicted$model_name, c("#45EE78", "#99EF12", "#01EEF9")) +
  theme_TMM_01_light +
  scale_colour_manual(values = palettes_long$pals_01,
                      aesthetics = c("colour", "fill")) +
  facet_grid(model_used ~ step, scales='free') +
  theme(strip.background = element_blank(),
        legend.position = "none")






ggplotly(ggplot(data = ddd,
                aes(x = time,
                    y = max_value,
                    colour = model)) +
           geom_line() +
           themes_TMM_01)

ggplotly(ggplot(data = ddd,
                aes(x = time,
                    y = aic,
                    colour = model)) +
           geom_line() +
           themes_TMM_01)

ggplot(data = ddd,
       aes(x = time,
           y = aic,
           colour = model)) +
  geom_line() +
  theme_TMM_01_light +
  scale_colour_manual(values = palettes_long$pals_01,
                      aesthetics = c("colour", "fill"))



