library(shiny)
library(shinyBS)

ui <- fluidPage(
  titlePanel("Tooltip Example"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("myRadio", "Choose an option:",
                   choices = c("Option 1", "Option 2", "Option 3"))
    ),
    
    mainPanel(
      h4("Selected option:"),
      verbatimTextOutput("selectedOption")
    )
  )
)

server <- function(input, output) {
  # Apply bstooltip to the radio buttons
  observe({
    bsTooltip(paste0("#myRadio input[type='radio'][value='", input$myRadio, "']"),
              title = "Radio Button Tooltip",
              content = paste("You selected", input$myRadio))
  })
  
  # Output the selected option
  output$selectedOption <- renderText({
    input$myRadio
  })
}

shinyApp(ui, server)
