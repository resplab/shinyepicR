# load packages

library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)

# load data

load("results_s1a.RData")
results_s1a_filtered <- results_s1a %>% 
  filter(case_detection != 0) %>% 
  mutate(cost_total_cumul = cumsum(cost_total)) %>% 
  mutate(cost_case_detection_cumul = cumsum(cost_case_detection)) %>%
  mutate(cost_treat_cumul = cumsum(cost_treat)) %>%
  mutate(cost_hosp_cumul = cumsum(cost_hosp)) %>%
  mutate(cost_maint_cumul = cumsum(cost_maint))


# Define UI 
ui <- fluidPage(
  titlePanel("S1a data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "uptake",
                  label = "Uptake (decimal)",
                  min = 0,
                  max = 1,
                  value = 0.05),
      selectInput(
        inputId = "y",
        label = "Y variable",
        choices = c(
          "Total costs" = "cost_total_cumul", 
          "Case detection costs" = "cost_case_detection_cumul", 
          "Treatment costs" = "cost_treat_cumul", 
          "Hospitalization costs" = "cost_hosp_cumul", 
          "Outpatient care costs" = "cost_maint_cumul"
        ),
        selected = "cost_total_cumul"
      ),
      
    ),
    
    mainPanel(
      plotOutput("cumul_costs_graph")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$cumul_costs_graph <- renderPlot({
    ggplot(data = results_s1a_filtered, aes_string(x = "year", y = input$y)) + 
      geom_point(color = "red", size = 3) + 
      geom_line(color = "red", size = 1.5) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
