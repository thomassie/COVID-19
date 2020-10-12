#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)











# ====================================================================================
# =====================================   UI  ========================================
# ====================================================================================


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Visual exploration of global CoViD-19 pandemic"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        # fluidRow(
        #     box(plotOutput("plot1", height = 250)),
        #     
        #     box(
        #         title = "Controls",
        #         sliderInput("slider", "Number of observations:", 1, 100, 50)
        #     )
        # )
    )
)



# ====================================================================================
# ===================================   SERVER  ======================================
# ====================================================================================

server <- function(input, output, session) {

    # Global Variables ---------------------------------------
    # Reactive Lists ------------------------------------------
    # Reactive Values ---------------------------------------
    # General Observers -----------------------------------
    
    # Tab 1 -----------------------------------------------------
    # Reactive Values ---------------------------------------
    # Observers -----------------------------------------------
    # Event Observers --------------------------------------
    # Output Elements --------------------------------------
    # Download Handlers ----------------------------------
    
    # Repeat the above structure for each tab/grouping
    
    # Output Options
}

# Run the application 
shinyApp(ui = ui, server = server)
