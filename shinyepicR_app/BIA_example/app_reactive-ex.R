library(shiny)

ui <- fluidPage(
  # Input UI elements
  numericInput("value", "New Value", value = 0),
  actionButton("updateBtn", "Update Data"),
  
  # Output UI elements
  tableOutput("dataTable")
)

server <- function(input, output) {
  # Define a reactive function to hold and update the dataframe
  reactiveData <- reactive({
    # Create or update the dataframe based on inputs or conditions
    df <- data.frame(A = c(1, 2, 3),
                     B = c(4, 5, 6))
    
    # Update the dataframe based on the new value input
    if (input$updateBtn > 0) {
      new_value <- input$value
      df$C <- new_value
    }
    
    # Return the updated dataframe
    df
  })
  
  # Render the updated dataframe in the output
  output$dataTable <- renderTable({
    reactiveData()
  })
}

shinyApp(ui, server)
