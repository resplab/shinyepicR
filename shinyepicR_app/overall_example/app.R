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
library(tools)
library(shinythemes)

load("overall.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Overall data"),
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
                "true_positive", 
                "true_positive_rate", 
                "false_positive", 
                "false_positive_rate"
              ),
              selected = "true_positive"
            ),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("bargraph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bargraph <- renderPlot({
      ggplot(data = overall, aes_string(x = "scenario", y = input$y)) + 
        geom_bar(stat = "identity")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
