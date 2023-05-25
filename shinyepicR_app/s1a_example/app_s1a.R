# load packages

library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)

# load data

load("s1a_res.RData")
s1a_res_filtered <- s1a_res %>% 
  filter(case_detection != 0)

# Define UI 
ui <- fluidPage(
  titlePanel("S1a data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_uptake",
                  label = "Yearly increase in uptake (initial: 5%)",
                  min = 0,
                  max = 0.1,
                  value = 0.05, 
                  step = 0.01),
      selectInput(
        inputId = "y",
        label = "Y variable",
        choices = c(
          "Total costs" = "cost_total", 
          "Case detection costs" = "cost_case_detection", 
          "Treatment costs" = "cost_treat", 
          "Hospitalization costs" = "cost_hosp", 
          "Outpatient care costs" = "cost_maint"
        ),
        selected = "cost_total"
      ),
      selectInput(
        inputId = "scenario", 
        label = "Cost detection strategy", 
        choices = c(
          "S1a", "S1b", "S1c", "S2a", "S3a", "S3b", "S3c", "S3d"
        ), 
        selected = "S1a"
      )
      
    ),
    
    mainPanel(
      plotOutput("costs_graph")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$costs_graph <- renderPlot({
    ggplot(data = s1a_res_filtered, aes_string(x = "year", y = input$y)) + 
      geom_point(color = "red", size = 3) + 
      geom_line(color = "red", linewidth = 1.25) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
