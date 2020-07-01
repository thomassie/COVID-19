# Clear workspace.
rm(list = ls())


# Load packages
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(plotly)
library(drc)
library(forcats)
library(shiny)
library(waffle)





# Load basic data for case numbers.
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
    clean_names(.) %>% 
    mutate_if(., is.character, as.factor) %>% 
    mutate(., country_region = stringr::str_replace_all(country_region,
                                                        c("US" = "United States",
                                                          "Congo \\(Kinshasa\\)" = "Congo, Dem. Rep.",
                                                          "Congo \\(Brazzaville\\)" = "Congo, Rep.",
                                                          "Egypt" = "Egypt, Arab Rep.",
                                                          "Bahamas" = "Bahamas, The",
                                                          "Brunei" = "Brunei Darussalam",
                                                          "Czechia" = "Czech Republic",
                                                          "Gambia" = "Gambia, The",
                                                          "Iran" = "Iran, Islamic Rep.",
                                                          "Korea, South" = "Korea, Rep.",
                                                          "Kyrgyzstan" = "Kyrgyz Republic",
                                                          "Russia" = "Russian Federation",
                                                          "Saint Lucia" = "St. Lucia",
                                                          "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
                                                          "Slovakia" = "Slovak Republic",
                                                          "Venezuela" = "Venezuela, RB",
                                                          "Syria" = "Syrian Arab Republic",
                                                          "Laos" = "Lao PDR",
                                                          "Saint Kitts and Nevis" = "St. Kitts and Nevis",
                                                          "Burma" = "Myanmar",
                                                          "Yemen" = "Yemen, Rep.")))

# dd <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Time series --- global/NEW_COVID_Time_series_TMM.csv")

# Loading demographic from 'The World Bank'.
# dd_pop <- read_csv("/Users/thomasmassie/Library/Mobile Documents/com~apple~CloudDocs/COVID-19/R Code/Supplementary information/WorldPop_2018.csv")
dd_pop <- read_csv("https://raw.githubusercontent.com/thomassie/COVID-19/master/R%20Code/Supplementary%20information/WorldPop_2018.csv")


# Process data for analysis
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
    mutate(., cases_all_rel_ck_rollmean = zoo::rollmean(cases_all_rel_ck, k = 7, fill = NA, align = "right")) %>% 
    mutate(., cases_all_diff_rel_ck_rollmean = zoo::rollmean(cases_all_diff_rel_ck, k = 7, fill = NA, align = "right")) %>% 
    ungroup(.)

# dd_countries_notjoined_dd <- unique(as.character(dd$country_region)[as.character(dd$country_region) %in% dd_pop$country == FALSE])
# dd_countries_notjoined_dd_pop <- unique(as.character(filter(dd_pop, region != "Aggregates")$country)[as.character(filter(dd_pop, region != "Aggregate")$country) %in% dd$country_region == FALSE])


# Define visual parameters, e.g. ggplot theme.
palettes <- list(
    pal_01 = c("#262406", "#BAA135", "#CE9133", "#C95B26", "#C84A22"),
    pal_02 = c("#E16C80", "#546B72", "#EBC553", "#D4BEA0", "#EEDDCB"),
    pal_03 = c("#981F27", "#26343F", "#E27B30", "#E1662B", "#7E933B"),
    pal_04 = c("#B8BFB6", "#627F33", "#FFC941", "#263140", "#E56032"),
    pal_05 = c("#BF9648", "#7A787F", "#F9F9F9", "#3E4040", "#177CB5"),
    pal_06 = c("#5A6D70", "#26291F", "#A4A8A7", "#BD5152", "#918F75"),
    pal_07 = c("#ECD074", "#595652", "#848A91", "#B0AFAB", "#82412F"),
    pal_08 = c("#DE314A", "#C72B54", "#D7CDC1", "#CE8F54", "#CD837A"),
    pal_09 = c("#C84A4F", "#801921", "#C72B34", "#CF983A", "#E5C547"))

palettes_long <- list(
    pals_01 = c("#262406", "#BAA135", "#CE9133", "#C95B26", "#C84A22", "#E16C80", "#546B72", "#EBC553", "#D4BEA0", "#EEDDCB"),
    pals_02 = c("#981F27", "#26343F", "#E27B30", "#E1662B", "#7E933B", "#B8BFB6", "#627F33", "#FFC941", "#263140", "#E56032"),
    pals_03 = c("#BF9648", "#7A787F", "#F9F9F9", "#3E4040", "#177CB5", "#5A6D70", "#26291F", "#A4A8A7", "#BD5152", "#918F75"),
    pals_04 = c("#ECD074", "#595652", "#848A91", "#B0AFAB", "#82412F", "#DE314A", "#C72B54", "#D7CDC1", "#CE8F54", "#CD837A"))

cols_background_dark  <- c("#D7D8D8", "#7B8381", "#585550", "#", "#", "#", "#", "#", "#", "#", "#", "#")
cols_background_light <- c("#D8D6D1", "#F1EAE0", "#D7CDC1", "#D8D4CD", "#", "#", "#", "#", "#", "#", "#", "#")

col_background <- cols_background_light[4]
col_grid_ticks <- cols_background_light[3]

theme_TMM_01 <- function() {
    theme(
        # axis.text = element_text(family = "Varela Round"),
        # text = element_text(family = "Montserrat-Light"),
        # text = element_text(family = "Lato"),
        text = element_text(family = "SF Pro Rounded"),
        # axis.text.x = element_text(size = 9, colour = "#7F8182", face = "plain", vjust = 1),
        axis.text.x = element_text(size = 9, colour = "#7F8182", vjust = 1, face = "plain"),
        axis.text.y = element_text(size = 9, colour = "#7F8182", vjust = 0.5, face = "plain"),
        axis.ticks = element_line(colour = col_grid_ticks, size = 0.2),
        axis.ticks.length = unit(3, "mm"),
        axis.line = element_blank(),
        plot.title = element_text(face = "plain", hjust = 0, vjust = -0, colour = "#3C3C3C", size = 30, margin=margin(10,0,8,)),
        plot.subtitle = element_text(hjust = 0, vjust = -1, colour = "#3C3C3C", size = 12, margin=margin(0,0,30,0)),
        plot.caption = element_text(size = 8, hjust = 1, vjust = -0.1, colour = "#7F8182"),
        panel.background = element_rect(fill = col_background),
        panel.border = element_blank(),
        plot.background = element_rect(fill = col_background, colour = col_background),
        panel.grid.major = element_line(colour = col_grid_ticks, size = 0.2),
        panel.grid.minor = element_line(colour = col_grid_ticks, size = 0.2),
        legend.title = element_blank(),
        # legend.justification = c(0,0),
        legend.position = "none",
        # legend.position = c(0.0, 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 8))
    # complete = FALSE)
}


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


# Function to generate the data frame used for waffle chart (for country)
f_waffle_country <- function(c, d) {
    dd_waffle_country <- dd_plus %>% 
        filter(., country_region %in% c & date == d) %>%
        mutate(., status = fct_relevel(status, "confirmed", "active", "recovered", "deaths")) %>% 
        filter(., status != "confirmed") %>% 
        group_by(., country_region) %>% 
        mutate(., cases = round_preserve(cases_all/sum(cases_all)*100),
               image = list(img)) %>% 
        ungroup(.) 
}










# ----------------------------------------------------
# ------------------------ 1 -------------------------
# ----------------------------------------------------
# ------------------ User interface ------------------
# ----------------------------------------------------


ui <- fluidPage(
    
    # Include .css file to customise appearance.
    tags$head(
        includeCSS("www/styles.css")
    ),
    
    # dashboardHeader(title = "CoViD-19 data exploration"),
    
    # Application title
    titlePanel(paste("A visual exploration", "of global CoViD-19 data", sep = "\n")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("date",
                      label = "Select a date",
                      min = min(dd$date), max = max(dd$date),
                      format = "dd/mm/yy",
                      startview = 'year', weekstart = 1,
                      value = lubridate::today(),
            ),
            selectInput("country", 
                        "Select a country",
                        choices = unique(dd$country_region),
                        selected = "Germany"
            ),
            sliderInput("param_crit", 
                        "Select a minimum case number",
                        min = 0, max = 300,
                        step = 5,
                        value = 50, 
                        ticks = FALSE,
            ),
            radioButtons("check_type",
                         "Choose between absolute and relative numbers",
                         c("absolute values", "relative values (per 100'000)"),
                         "absolute values", inline = FALSE
            ),
            radioButtons("time_cat",
                         "Choose between date or days since first n cases",
                         c("date", "days"),
                         "date", inline = FALSE
            ),
            radioButtons("others_show",
                         "Show other countries?",
                         c("yes", "no"),
                         "yes", inline = FALSE
            ),
            actionButton("show_button", "Update")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Proportional", 
                         plotOutput("p00_PropWaffle", width = "537px", height = "650px"),
                         downloadButton("download_p00", "Download")),
                tabPanel("Total cases (linear)", 
                         plotOutput("p01_PlotCasesLin", width = "1100px", height = "850px"),
                         downloadButton("download_p01", "Download")),
                tabPanel("Total cases (log10)",
                         plotOutput("p02_PlotCasesLog", width = "1100px", height = "850px"),
                         downloadButton("download_p02", "Download")),
                tabPanel("Daily changes (linear)", 
                         plotOutput("p03_PlotCasesDiffLin", width = "1100px", height = "850px"),
                         downloadButton("download_p03", "Download"))
                # tabPanel("relative & logarithmic & relative", plotOutput("PlotCasesLinRel", width = "1100px", height = "850px"))
                # tabPanel("Absolute numbers", plotlyOutput("casesPlot"))
            )
        )
    )
)










# ----------------------------------------------------
# ------------------------ 2 -------------------------
# ----------------------------------------------------
# ---------------------- Server ----------------------
# ----------------------------------------------------

server <- function(input, output) {
    
    # Filter for a specific country.
    dd_country <- eventReactive(input$show_button, {
        dd_plus %>% 
            filter(., country_region == input$country)
    })
    
    # Filter for a specific country and date.
    dd_point <- eventReactive(input$show_button, {
        dd_plus %>% 
            filter(., date == input$date & country_region == input$country)
    })
    
    
    # For all plots using time since first time n cases; using a cut-off criteria.
    dd_cut_crit <- eventReactive(input$show_button, {
        dd_plus %>% 
            group_by(., country_region, status) %>% 
            filter(., cases_all >= input$param_crit) %>%
            mutate(., time_ind = as.numeric(difftime(date, min(date), units = "days"))) %>% 
            ungroup(.)
    }) 
    
    
    # Plotting the country selected against all other for all four case types.
    dd_base_viz <- eventReactive(input$show_button, {
        dd_cut_crit() %>%
            mutate(., col_selected_country = factor(ifelse(country_region == input$country, status, "all"),
                                                    levels = c("confirmed", "active", "recovered", "deaths", "all"))) %>%
            mutate(., status = factor(status, levels = c("confirmed", "active", "recovered", "deaths"))) %>%
            mutate(., size_viz = ifelse(country_region == input$country, 0.8, 1.5))
    })
    
    
    # Plotting circles at the end of each line plot (max(time_ind)).
    dd_base_viz_selected <- eventReactive(input$show_button, {
        dd_cut_crit() %>%
            filter(., country_region == input$country) %>%
            group_by(., status) %>%
            filter(., time_ind == max(time_ind)) %>%
            ungroup()
    })
    
    
    # Allow selecting/highlighting a specific date (input$date).
    dd_base_viz_selected_date <- eventReactive(input$show_button, {
        dd_cut_crit() %>%
            filter(., country_region == input$country) %>%
            group_by(., status) %>%
            filter(., date == input$date) %>%
            ungroup()
    })
    
    
    # Vector used for waffle plot (for country).    
    dd_waffle_country_vec <- eventReactive(input$show_button, {
        f_waffle_country(input$country, input$date) %>%
            dplyr::select(., c(status, cases)) %>% 
            deframe()
    })
    
    
    # To show top 5 countries according to aggregated case numbers.
    dd_base_viz_ranked <- eventReactive(input$show_button, {
        if (input$check_type == "absolute values") {
            dd_base_viz() %>% 
                filter(., country_region != input$country) %>%
                group_by(., country_region, status) %>% 
                filter(., time_ind == max(time_ind)) %>% 
                ungroup(.) %>% 
                group_by(., status) %>% 
                top_n(., 5, cases_all) %>% 
                ungroup(.) %>% 
                arrange(., status)
        } else {
            dd_base_viz() %>% 
                filter(., country_region != input$country) %>%
                group_by(., country_region, status) %>% 
                filter(., time_ind == max(time_ind)) %>% 
                ungroup(.) %>% 
                group_by(., status) %>% 
                top_n(., 5, cases_all_rel_ck) %>% 
                ungroup(.) %>% 
                arrange(., status)  
        }
    })
    
    
    # To show top 5 countries according to daily changes in case numbers.
    dd_base_viz_ranked_diff <- eventReactive(input$show_button, {
        if (input$check_type == "absolute values") {
            dd_base_viz() %>% 
                filter(., country_region != input$country) %>%
                group_by(., country_region, status) %>% 
                filter(., time_ind == max(time_ind)) %>% 
                ungroup(.) %>% 
                group_by(., status) %>% 
                top_n(., 5, cases_all_diff) %>% 
                ungroup(.) %>% 
                arrange(., status)
        } else {
            dd_base_viz() %>% 
                filter(., country_region != input$country) %>%
                group_by(., country_region, status) %>% 
                filter(., time_ind == max(time_ind)) %>% 
                ungroup(.) %>% 
                group_by(., status) %>% 
                top_n(., 5, cases_all_diff_rel_ck) %>% 
                ungroup(.) %>% 
                arrange(., status)  
        }
    })
    
    
    # Create an object for storing reactive values.
    data <- reactiveValues()
    
    
    # All objects that are dependend on an event ('Update' button).
    observeEvent(input$show_button, {
        
        # -------------------------------------------------------   
        # PLOT 0: Proportions (waffle chart) --------------------
        
        data$p00 <- {
            iron(
                waffle(dd_waffle_country_vec(), 
                       rows = 10, 
                       colors = c("#D8A94F", "#A44A51", "#6378AC"),
                       use_glyph = "male", glyph_size = 12.0 , 
                       legend_pos="bottom") +
                    labs(title = paste("Case numbers for", input$country),
                         subtitle = "How do case types compare to each other?",
                         caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)") +
                    theme_void() +
                    theme(text = element_text(family = "SF Pro Rounded"),
                          panel.spacing = unit(20, "mm"),
                          legend.position = "none",
                          legend.text = element_text(colour="#666666", size = 8),
                          legend.key.size = unit(5, "mm"),
                          axis.title.x = element_text(margin = margin(20, 0, 0, 0)), 
                          axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
                          plot.background = element_rect(fill = col_background, colour = col_background),
                          plot.title = element_text(face = "plain", hjust = 0.05, vjust = -0.0, colour = "#3C3C3C", size = 30, margin = margin(10,0,8,)),
                          plot.subtitle = element_text(hjust = 0.05, vjust = -1, colour = "#3C3C3C", size = 12, margin = margin(0,0,30,0)),
                          plot.caption = element_text(size = 8, hjust = 1, vjust = -0.1, colour = "#7F8182", margin = margin(10,0,10,0)))
            )
        }
        
        
        # -------------------------------------------------------   
        # PLOT 1: Aggregated case numbers, linear ---------------
        
        data$p01 <- if (input$check_type == "absolute values") {
            ggplot() +
                geom_line(data = dd_base_viz(),
                          aes(x = time_ind, y = cases_all, group = as.factor(country_region),
                              colour = "#FFFFFF"), 
                          alpha = 0.5,
                          size = 0.5) +
                geom_text(data = dd_base_viz_ranked(),
                          aes(x = time_ind, y = cases_all, group = as.factor(country_region),
                              label = country_region,
                              colour = "#FFFFFF"),
                          hjust = unit(-0.2, "mm"),
                          size = 3,
                          alpha = 0.7) +
                geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                          aes(x = time_ind, y = cases_all, colour = status),
                          size = 1.0,
                          alpha = 0.9) +
                geom_point(data = dd_base_viz_selected(), 
                           aes(x = time_ind, y = cases_all,colour = status),
                           size = 1.4) +
                geom_text(data = dd_base_viz_selected(), 
                          aes(x = time_ind, y = cases_all, 
                              label = paste(paste(round(cases_all/1000, 1), "K"), status, sep = "\n"), 
                              colour = status,
                              fontface= 2),
                          vjust = unit(1.2, "mm"),
                          hjust = unit(-0.2, "mm"),
                          size = 3.0) +
                scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                facet_wrap(~ status, scales = "free") +
                coord_cartesian(xlim = c(0, 160)) +
                # scale_y_continuous(labels = scales::comma) +
                scale_y_continuous(labels = scales::comma_format(big.mark = "'",
                                                                 decimal.mark = ".")) +
                labs(x = "", 
                     y = "",
                     title = paste("Case numbers for", input$country),
                     subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                     caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                theme(strip.background = element_blank(),
                      strip.text = element_blank(),
                      panel.spacing = unit(1.5, "lines")) +
                theme_TMM_01()
        } else {
            ggplot() +
                geom_line(data = dd_base_viz(),
                          aes(x = time_ind, y = cases_all_rel_ck, group = as.factor(country_region),
                              colour = "#FFFFFF"), 
                          alpha = 0.5,
                          size = 0.5) +
                geom_text(data = dd_base_viz_ranked(),
                          aes(x = time_ind, y = cases_all_rel_ck, group = as.factor(country_region),
                              label = country_region,
                              colour = "#FFFFFF"),
                          hjust = unit(-0.2, "mm"),
                          size = 3,
                          alpha = 0.7) +
                geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                          aes(x = time_ind, y = cases_all_rel_ck, colour = status),
                          size = 1.0,
                          alpha = 0.9) +
                geom_point(data = dd_base_viz_selected(), 
                           aes(x = time_ind, y = cases_all_rel_ck,colour = status),
                           size = 1.4) +
                geom_text(data = dd_base_viz_selected(), 
                          aes(x = time_ind, y = cases_all_rel_ck, 
                              label = paste(round(cases_all_rel_ck, 2), status, sep = "\n"), 
                              colour = status,
                              fontface= 2),
                          vjust = unit(1.2, "mm"),
                          hjust = unit(-0.2, "mm"),
                          size = 3.0) +
                scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                facet_wrap(~ status, scales = "free") +
                coord_cartesian(xlim = c(0, 160)) +
                # scale_y_continuous(labels = scales::comma) +
                scale_y_continuous(labels = scales::comma_format(big.mark = "'",
                                                                 decimal.mark = ".")) +
                labs(x = "", 
                     y = "",
                     title = paste("Case numbers for", input$country),
                     subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                     caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                theme(strip.background = element_blank(),
                      strip.text = element_blank(),
                      panel.spacing = unit(1.5, "lines")) +
                theme_TMM_01()
        }
        
        
        # -------------------------------------------------------   
        # PLOT 2: Aggregated case numbers, logarithmic ----------
        
        data$p02 <- if (input$check_type == "absolute values") {
            ggplot() +
                geom_line(data = dd_base_viz(),
                          aes(x = time_ind, y = cases_all, group = as.factor(country_region),
                              colour = "#FFFFFF"), 
                          alpha = 0.5,
                          size = 0.5) +
                geom_text(data = dd_base_viz_ranked(),
                          aes(x = time_ind, y = cases_all, group = as.factor(country_region),
                              label = country_region,
                              colour = "#FFFFFF"),
                          hjust = unit(-0.2, "mm"),
                          size = 3,
                          alpha = 0.7) +
                geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                          aes(x = time_ind, y = cases_all, colour = status),
                          size = 1.0,
                          alpha = 0.9) +
                geom_point(data = dd_base_viz_selected(), 
                           aes(x = time_ind, y = cases_all,colour = status),
                           size = 1.4) +
                geom_text(data = dd_base_viz_selected(), 
                          aes(x = time_ind, y = cases_all, 
                              label = paste(paste(round(cases_all/1000, 1), "K"), status, sep = "\n"), 
                              colour = status,
                              fontface= 2),
                          vjust = unit(1.2, "mm"),
                          hjust = unit(-0.2, "mm"),
                          size = 3.0) +
                scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                facet_wrap(~ status, scales = "free") +
                coord_cartesian(xlim = c(0, 160)) +
                # scale_y_continuous(labels = scales::comma) +
                scale_y_log10(labels = scales::comma_format(big.mark = "'",
                                                            decimal.mark = ".")) +
                labs(x = "", 
                     y = "",
                     title = paste("Case numbers for", input$country),
                     subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                     caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                theme(strip.background = element_blank(),
                      strip.text = element_blank(),
                      panel.spacing = unit(1.5, "lines")) +
                theme_TMM_01()
            
        } else {
            
            ggplot() +
                geom_line(data = dd_base_viz(),
                          aes(x = time_ind, y = cases_all_rel_ck, group = as.factor(country_region),
                              colour = "#FFFFFF"), 
                          alpha = 0.5,
                          size = 0.5) +
                geom_text(data = dd_base_viz_ranked(),
                          aes(x = time_ind, y = cases_all_rel_ck, group = as.factor(country_region),
                              label = country_region,
                              colour = "#FFFFFF"),
                          hjust = unit(-0.2, "mm"),
                          size = 3,
                          alpha = 0.7) +
                geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                          aes(x = time_ind, y = cases_all_rel_ck, colour = status),
                          size = 1.0,
                          alpha = 0.9) +
                geom_point(data = dd_base_viz_selected(), 
                           aes(x = time_ind, y = cases_all_rel_ck,colour = status),
                           size = 1.4) +
                geom_text(data = dd_base_viz_selected(), 
                          aes(x = time_ind, y = cases_all_rel_ck, 
                              label = paste(round(cases_all_rel_ck, 2), status, sep = "\n"), 
                              colour = status,
                              fontface= 2),
                          vjust = unit(1.2, "mm"),
                          hjust = unit(-0.2, "mm"),
                          size = 3.0) +
                scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                facet_wrap(~ status, scales = "free") +
                coord_cartesian(xlim = c(0, 160)) +
                # scale_y_continuous(labels = scales::comma) +
                scale_y_log10(labels = scales::comma_format(big.mark = "'",
                                                            decimal.mark = ".")) +
                labs(x = "", 
                     y = "",
                     title = paste("Case numbers for", input$country),
                     subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                     caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                theme(strip.background = element_blank(),
                      strip.text = element_blank(),
                      panel.spacing = unit(1.5, "lines")) +
                theme_TMM_01()
        }
        
        
        # -------------------------------------------------------   
        # PLOT 3: Daily changes in case numbers, linear ---------
        
        data$p03 <-  if (input$time_cat == "days") {
            
            # Use date as x-axis.
            if (input$check_type == "absolute values") {
                
                # A function which accounts for whether other countries than the selected one are shown, too.
                plot_others <- function() {
                    if (input$others_show == "yes" ) {
                        ggplot() +
                            geom_line(data = dd_base_viz(),
                                      aes(x = time_ind, y = cases_all_diff, group = as.factor(country_region),
                                          colour = "#FFFFFF"), 
                                      alpha = 0.5,
                                      size = 0.5) +
                            geom_text(data = dd_base_viz_ranked(),
                                      aes(x = time_ind, y = cases_all_diff, group = as.factor(country_region),
                                          label = country_region,
                                          colour = "#FFFFFF"),
                                      hjust = unit(-0.2, "mm"),
                                      size = 3,
                                      alpha = 0.7) +
                            scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC"))
                    } else {
                        ggplot() +
                            scale_colour_manual(values = c("#4E4E4C", "#D8A94F", "#6378AC", "#A44A51"))
                    }
                }
                
                plot_others() +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = time_ind, y = cases_all_diff, colour = status),
                              size = 0.5,
                              alpha = 0.9) +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = time_ind, y = cases_all_diff_rollmean, colour = status),
                              size = 1.0,
                              alpha = 0.9) +
                    # geom_point(data = dd_base_viz_selected(), 
                    #            aes(x = time_ind, y = cases_all_diff,colour = status),
                    #            size = 1.4) +
                    geom_hline(data = dd_base_viz_selected_date(), 
                               aes(yintercept = dd_base_viz_selected_date()$cases_all_diff, group = status),
                               size = 0.5,
                               linetype = "dashed",
                               colour = "#7F8182") +
                    geom_point(data = dd_base_viz_selected_date(), 
                               aes(x = time_ind, y = cases_all_diff,colour = status),
                               size = 2.5) +
                    geom_text(data = dd_base_viz_selected(), 
                              aes(x = time_ind, y = cases_all_diff, 
                                  label = paste(cases_all_diff, status, sep = "\n"), 
                                  colour = status,
                                  fontface= 2),
                              vjust = unit(1.2, "mm"),
                              hjust = unit(-0.2, "mm"),
                              size = 3.0) +
                    # scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                    facet_wrap(~ status, scales = "free") +
                    coord_cartesian(xlim = c(0, 160)) +
                    # scale_y_continuous(labels = scales::comma) +
                    scale_y_continuous(labels = scales::comma_format(big.mark = "'",
                                                                     decimal.mark = ".")) +
                    labs(x = "", 
                         y = "",
                         title = paste("Daily changes in case numbers for", input$country),
                         subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (as of ", dd_base_viz_selected()$date[1], ").", sep = ""),
                         caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                    theme(strip.background = element_blank(),
                          strip.text = element_blank(),
                          panel.spacing = unit(1.5, "lines")) +
                    theme_TMM_01()
                
                
            } else {    # if values should be relative
                
                # A function which accounts for whether other countries than the selected one are shown, too.
                plot_others <- function() {
                    if (input$others_show == "yes" ) {
                        ggplot() +
                            geom_line(data = dd_base_viz(),
                                      aes(x = time_ind, y = cases_all_diff_rel_ck, group = as.factor(country_region),
                                          colour = "#FFFFFF"), 
                                      alpha = 0.5,
                                      size = 0.5) +
                            geom_text(data = dd_base_viz_ranked(),
                                      aes(x = time_ind, y = cases_all_diff_rel_ck, group = as.factor(country_region),
                                          label = country_region,
                                          colour = "#FFFFFF"),
                                      hjust = unit(-0.2, "mm"),
                                      size = 3,
                                      alpha = 0.7) +
                            scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC"))
                    } else {
                        ggplot() +
                            scale_colour_manual(values = c("#4E4E4C", "#D8A94F", "#6378AC", "#A44A51"))
                    }
                }
                
                plot_others() +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = time_ind, y = cases_all_diff_rel_ck, colour = status),
                              size = 0.5,
                              alpha = 0.9) +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = time_ind, y = cases_all_diff_rel_ck_rollmean, colour = status),
                              size = 1.0,
                              alpha = 0.9) +
                    geom_hline(data = dd_base_viz_selected_date(), 
                               aes(yintercept = dd_base_viz_selected()$cases_all_diff_rel_ck, group = status),
                               size = 0.5,
                               linetype = "dashed",
                               colour = "#7F8182") +
                    geom_point(data = dd_base_viz_selected_date(), 
                               aes(x = time_ind, y = cases_all_diff_rel_ck,colour = status),
                               size = 2.5) +
                    geom_text(data = dd_base_viz_selected(), 
                              aes(x = time_ind, y = cases_all_diff_rel_ck, 
                                  label = paste(cases_all_diff_rel_ck, status, sep = "\n"), 
                                  colour = status,
                                  fontface= 2),
                              vjust = unit(1.2, "mm"),
                              hjust = unit(-0.2, "mm"),
                              size = 3.0) +
                    # scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                    facet_wrap(~ status, scales = "free") +
                    coord_cartesian(xlim = c(0, 160)) +
                    # scale_y_continuous(labels = scales::comma) +
                    scale_y_continuous(labels = scales::comma_format(big.mark = "'",
                                                                     decimal.mark = ".")) +
                    labs(x = "", 
                         y = "",
                         title = paste("Daily changes in case numbers for", input$country),
                         subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                         caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                    theme(strip.background = element_blank(),
                          strip.text = element_blank(),
                          panel.spacing = unit(1.5, "lines")) +
                    theme_TMM_01()
            }
            
        } else {    # if x-axis should display the date instead of the days past since first n occurrences
            
            # A function which accounts for whether other countries than the selected one are shown, too.
            plot_others <- function() {
                if (input$others_show == "yes" ) {
                    ggplot() +
                        geom_line(data = dd_base_viz(),
                                  aes(x = date, y = cases_all_diff, group = as.factor(country_region),
                                      colour = "#FFFFFF"), 
                                  alpha = 0.5,
                                  size = 0.5) +
                        geom_text(data = dd_base_viz_ranked(),
                                  aes(x = date, y = cases_all_diff, group = as.factor(country_region),
                                      label = country_region,
                                      colour = "#FFFFFF"),
                                  hjust = unit(-0.2, "mm"),
                                  size = 3,
                                  alpha = 0.7) +
                        scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC"))
                } else {
                    ggplot() +
                        scale_colour_manual(values = c("#4E4E4C", "#D8A94F", "#6378AC", "#A44A51"))
                }
            }
            
            # Use 'date' as x-axis.
            if (input$check_type == "absolute values") {
                
                plot_others() +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = date, y = cases_all_diff, colour = status),
                              size = 0.5,
                              alpha = 0.9) +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = date, y = cases_all_diff_rollmean, colour = status),
                              size = 1.0,
                              alpha = 0.9) +
                    geom_hline(data = dd_base_viz_selected_date(), 
                               aes(yintercept = dd_base_viz_selected_date()$cases_all_diff, group = status),
                               size = 0.5,
                               linetype = "dashed",
                               colour = "#7F8182") +
                    geom_point(data = dd_base_viz_selected_date(), 
                               aes(x = date, y = cases_all_diff,colour = status),
                               size = 2.5) +
                    geom_text(data = dd_base_viz_selected(), 
                              aes(x = date, y = cases_all_diff, 
                                  label = paste(cases_all_diff, status, sep = "\n"), 
                                  colour = status,
                                  fontface= 2),
                              vjust = unit(1.2, "mm"),
                              hjust = unit(-0.2, "mm"),
                              size = 3.0) +
                    # scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                    facet_wrap(~ status, scales = "free") +
                    # scale_x_date(limits = c(min(date), max(date))) +
                    scale_x_date(date_labels = "%m/%y",
                                 expand = c(0, 50)) +
                    # scale_y_continuous(labels = scales::comma) +
                    scale_y_continuous(labels = scales::comma_format(big.mark = "'",
                                                                     decimal.mark = ".")) +
                    labs(x = "", 
                         y = "",
                         title = paste("Daily changes in case numbers for", input$country),
                         subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                         caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                    theme(strip.background = element_blank(),
                          strip.text = element_blank(),
                          panel.spacing = unit(1.5, "lines")) +
                    theme_TMM_01()
                
            } else {    # if values should be relative
                
                # A function which accounts for whether other countries than the selected one are shown, too.
                plot_others <- function() {
                    if (input$others_show == "yes" ) {
                        ggplot() +
                            geom_line(data = dd_base_viz(),
                                      aes(x = date, y = cases_all_diff_rel_ck, group = as.factor(country_region),
                                          colour = "#FFFFFF"), 
                                      alpha = 0.5,
                                      size = 0.5) +
                            geom_text(data = dd_base_viz_ranked_diff(),
                                      aes(x = date, y = cases_all_diff_rel_ck, group = as.factor(country_region),
                                          label = country_region,
                                          colour = "#FFFFFF"),
                                      hjust = unit(-0.2, "mm"),
                                      size = 3,
                                      alpha = 0.7) +
                            scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC"))
                    } else {
                        ggplot() +
                            scale_colour_manual(values = c("#4E4E4C", "#D8A94F", "#6378AC", "#A44A51"))
                    }
                }
                
                plot_others() +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = date, y = cases_all_diff_rel_ck, colour = status),
                              size = 0.5,
                              alpha = 0.9) +
                    geom_line(data = filter(dd_base_viz(), country_region %in% input$country), 
                              aes(x = date, y = cases_all_diff_rel_ck_rollmean, colour = status),
                              size = 1.0,
                              alpha = 0.9) +
                    geom_hline(data = dd_base_viz_selected_date(), 
                               aes(yintercept = dd_base_viz_selected()$cases_all_diff_rel_ck, group = status),
                               size = 0.5,
                               linetype = "dashed",
                               colour = "#7F8182") +
                    geom_point(data = dd_base_viz_selected_date(), 
                               aes(x = date, y = cases_all_diff_rel_ck,colour = status),
                               size = 2.5) +
                    geom_text(data = dd_base_viz_selected(), 
                              aes(x = date, y = cases_all_diff_rel_ck, 
                                  label = paste(cases_all_diff_rel_ck, status, sep = "\n"), 
                                  colour = status,
                                  fontface= 2),
                              vjust = unit(1.2, "mm"),
                              hjust = unit(-0.2, "mm"),
                              size = 3.0) +
                    # scale_colour_manual(values = c("#FFFFFF", "#D8A94F", "#4E4E4C", "#A44A51", "#6378AC")) +
                    facet_wrap(~ status, scales = "free") +
                    # scale_x_date(limits = c(min(date), max(date))) +
                    scale_x_date(date_labels = "%B/%y",
                                 expand = c(0, 50)) +
                    # scale_y_continuous(labels = scales::comma) +
                    scale_y_continuous(labels = scales::comma_format(big.mark = "'",
                                                                     decimal.mark = ".")) +
                    labs(x = "", 
                         y = "",
                         title = paste("Daily changes in case numbers for", input$country),
                         subtitle = paste("Temporal course of case numbers when first time exceeded ", input$param_crit, " (", dd_base_viz_selected()$date[1], ").", sep = ""),
                         caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) & The World Bank") +
                    theme(strip.background = element_blank(),
                          strip.text = element_blank(),
                          panel.spacing = unit(1.5, "lines")) +
                    theme_TMM_01()
                
            }   # closes if "absolute" or else
            
        }   # closes if "days" or else
        
        # -------------------------------------------------------   
        # PLOT 4: Ranked ---------
        
    })
    
    
    # Linear plot to compare daily changes in case numbers for countries.
    
    # Render 'p00' for Shiny app.
    # output$p00_PropWaffle <- renderPlot({ data$p00 })
    
    output$p00_PropWaffle <- renderPlot({
        iron(
            waffle(dd_waffle_country_vec(),
                   rows = 10,
                   colors = c("#D8A94F", "#A44A51", "#6378AC"),
                   use_glyph = "male", glyph_size = 12.0 ,
                   # title = "Cases for Germany",
                   legend_pos="bottom") +
                # facet_wrap(~ status) +
                labs(title = paste("Case numbers for", input$country),
                     subtitle = "How do case types compare to each other?",
                     caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)") +
                # theme_TMM_01() +
                theme_void() +
                theme(text = element_text(family = "SF Pro Rounded"),
                      panel.spacing = unit(20, "mm"),
                      legend.position = "none",
                      legend.text = element_text(colour="#666666", size = 8),
                      legend.key.size = unit(5, "mm"),
                      axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                      axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
                      plot.background = element_rect(fill = col_background, colour = col_background),
                      plot.title = element_text(face = "plain", hjust = 0.05, vjust = -0, colour = "#3C3C3C", size = 30, margin = margin(10,0,8,)),
                      plot.subtitle = element_text(hjust = 0.04, vjust = -1, colour = "#3C3C3C", size = 12, margin = margin(0,0,30,0)),
                      plot.caption = element_text(size = 8, hjust = 0.95, vjust = -0.1, colour = "#7F8182", margin = margin(10,0,10,0)))
        )
    })
    
    # Make 'p00' for download.
    output$download_p00 <- downloadHandler(
        filename = function() { paste(paste("CoViD-19-ProportionsIso", input$country, lubridate::today(), sep = "_"), "png", sep = ".") },
        content = function(file) {
            ggsave(file, 
                   plot = iron(
                       waffle(dd_waffle_country_vec(),
                              rows = 10,
                              colors = c("#D8A94F", "#A44A51", "#6378AC"),
                              use_glyph = "male", glyph_size = 12.0 ,
                              # title = "Cases for Germany",
                              legend_pos="bottom") +
                           # facet_wrap(~ status) +
                           labs(title = paste("Case numbers for", input$country),
                                subtitle = "How do case types compare to each other?",
                                caption = "Source: Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)") +
                           # theme_TMM_01() +
                           theme_void() +
                           theme(text = element_text(family = "SF Pro Rounded"),
                                 panel.spacing = unit(20, "mm"),
                                 legend.position = "none",
                                 legend.text = element_text(colour="#666666", size = 8),
                                 legend.key.size = unit(5, "mm"),
                                 axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                                 axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
                                 plot.background = element_rect(fill = col_background, colour = col_background),
                                 plot.title = element_text(face = "plain", hjust = 0.05, vjust = -0, colour = "#3C3C3C", size = 30, margin = margin(10,0,8,)),
                                 plot.subtitle = element_text(hjust = 0.04, vjust = -1, colour = "#3C3C3C", size = 12, margin = margin(0,0,30,0)),
                                 plot.caption = element_text(size = 8, hjust = 0.95, vjust = -0.1, colour = "#7F8182", margin = margin(10,0,10,0)))
                   ),
                   # scale = 1,
                   width = 200,
                   height = 300,
                   units = c("mm"),
                   dpi = 300,
                   device = "png")
        }
    )
    
    
    # Linear plot to compare case numbers for countries.
    
    # Render 'p01' for Shiny app.
    output$p01_PlotCasesLin <- renderPlot({ data$p01 })
    
    # Make 'p01' for download.
    output$download_p01 <- downloadHandler(
        filename = function() { paste(paste("CoViD-19-AggrCasesLin", input$country, lubridate::today(), sep = "_"), "png", sep = ".") },
        content = function(file) {
            ggsave(file, 
                   plot = data$p01,
                   # scale = 1,
                   width = 400,
                   height = 300,
                   units = c("mm"),
                   dpi = 300,
                   device = "png")
        }
    )
    
    
    # Logarithmic plot to compare case numbers for countries.
    
    # Render 'p02' for Shiny app.
    output$p02_PlotCasesLog <- renderPlot({ data$p02 })
    
    # Make 'p02' for download.
    output$download_p02 <- downloadHandler(
        filename = function() { paste("CoViD-19-AggrCasesLog", "png", sep = ".") },
        content = function(file) {
            ggsave(file, 
                   plot = data$p02,
                   # scale = 1,
                   width = 400,
                   height = 300,
                   units = c("mm"),
                   dpi = 300,
                   device = "png")
        }
    )
    
    
    # Linear plot to compare daily changes in case numbers for countries.
    
    # Render 'p03' for Shiny app.
    output$p03_PlotCasesDiffLin <- renderPlot({ data$p03 })
    
    # Make 'p03' for download.
    output$download_p03 <- downloadHandler(
        filename = function() { paste("CoViD-19-ChangesCasesLin", "png", sep = ".") },
        content = function(file) {
            ggsave(file, 
                   plot = data$p03,
                   # scale = 1,
                   width = 400,
                   height = 300,
                   units = c("mm"),
                   dpi = 300,
                   device = "png")
        }
    )
    
    
    
    
    output$PlotWaffleCountry <- renderPlot({
        
    })
    
    # output$casesPlot <- renderPlot({
    #     ggplot(data = dd()) +
    #         geom_line(aes(x = date, y = cases, colour = status)) +
    #         geom_point(data = dd_point(), aes(x = date, y = cases, colour = status))  +
    #         theme_void() +
    #         # theme_TMM_01_light +
    #         scale_y_log10() +
    #         scale_colour_manual(values = palettes$pal_09,
    #                             aesthetics = c("colour", "fill"))
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
