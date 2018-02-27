#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cerulean"),
    tags$style(HTML(type="text/css"," 
    ")),
   # Application title
   titlePanel("Climate modeller"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         h3("Set parameters"),
         numericInput("alpha", "Alpha value", value = 1.1),
         numericInput("c_u", "Thermal inertia (upper depth)", value = 421800000),
         numericInput("c_d", "Thermal inertia (deep depth)", value = 3796200000),
         numericInput("gamma", "Gamma", value = 1.05),
         actionButton("plot", "Run calculations"),
         br(),
    
         hr(),
         h3("Project temperature change"),
         sliderInput("Year", label  = "Single entity climate model", min = 1850, max = 2050, value = 1850, step = 10, animate = TRUE, sep = "", ticks = TRUE),
         sliderInput("Year_double", label  = "2 layer climate model", min = 1850, max = 2050, value = 1850, step = 10, animate = TRUE, sep = "", ticks = TRUE),
         tableOutput("done")
      ),
      
      # Show a plot of the generated climate model
      mainPanel(
         plotOutput("climateplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # single entity model
  
  forcing <- read.csv("./Data/RCP8.5_Forcing.txt", sep="")
  
  # strip out forcing years into vector
  years <- rownames(forcing)
  
  # format forcing_data dataframe for single entity
  forcing_data <- data.frame(years,forcing$Total_Forcing)
  colnames(forcing_data) <- c("Year","Forcing")
  forcing_data$Temperature <- forcing_data$Forcing/1.1
  forcing_data$Year <- as.character(forcing_data$Year)
  forcing_data$Year <- as.numeric(forcing_data$Year)

  # set up vectors for 2 layer model
  Temp_u <- c(0, rep(NA, 649))
  Temp_d <- c(0, rep(NA, 649))
  Temp <- c(0, rep(NA, 649))
  timestep <- 3e8
  Forcing <- forcing_data$Forcing
  
 # two layer model
  
  observeEvent(input$run, {
    
  })
  
  observeEvent(input$plot, {
    
    for (i in 2:649) {
      diff <- input$gamma * (Temp_u[i-1] - Temp_d[i-1])
      dTu_dt <- timestep * (Forcing[i-1] - diff - input$alpha * (Temp_u[i-1]))/input$c_u
      Temp_u[i] <- Temp_u[i-1] + dTu_dt
      dTd_dt <- timestep * diff/input$c_d
      Temp_d[i] <- Temp_d[i-1] + dTd_dt
    }
    
    values <- data.frame(Temp_u)
    colnames(values) <- c("Temperature")
    values$Year <- 1850:2499
    
    output$climateplot <- renderPlot({
      filtered <-
        forcing_data %>%
        filter(Year <= input$Year)
      
      layer_filtered <-
        values %>%
        filter(Year <= input$Year_double)
      
      ggplot() + geom_hline(yintercept = 0, size = 1.5) +
        geom_hline(yintercept = 2, colour = "red", size = 1.5, linetype = "dashed") +
        geom_path(aes(x = Year, y = Temperature), data = filtered, stat  = "identity", size = 1.5) +
        geom_path(aes(x = Year, y = Temperature), data = layer_filtered, size = 1.5, na.rm = TRUE, colour = "blue") + 
        geom_label_repel(aes(x = tail(Year, 1), y = tail(Temperature, 1), label = round(tail(Temperature, 1), digits = 3)), data = tail(layer_filtered, 1), stat  = "identity", fontface = "bold", color = "blue", nudge_x = 5) +
        geom_label_repel(aes(x = tail(Year, 1), y = tail(Temperature, 1), label = round(tail(Temperature, 1), digits = 3)), data = tail(filtered, 1), stat  = "identity", fontface = "bold", color = "black", nudge_x = 5) +
        scale_x_continuous(limits = c(1850, 2050), breaks = c(1850, 1875, 1900, 1925, 1950, 1975, 2000, 2025, 2050)) + 
        scale_y_continuous(limits = c(-1.5, 12), breaks = c(-2, 0, 2, 4, 6, 8, 10, 12)) +
        ylab(expression(paste("Average global temperature change  "(degree~C)))) +
        theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", colour = "black"), axis.text = element_text(size = 17), axis.title.x = element_text(size = 20, margin = margin(12)), axis.title.y = element_text(size = 20, margin = margin(12)))
    }, height = 800, width = 800) 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

