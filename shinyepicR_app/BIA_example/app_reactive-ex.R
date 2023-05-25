library(shiny)

ui <- fluidPage(
  # Input UI elements
  numericInput("value", "New Value", value = 0),
  actionButton("updateBtn", "Update Data"),
  
  # Output UI elements
  tableOutput("dataTable")
)

server <- function(input, output) {
  # Define an eventReactive function to update the dataframe when the button is clicked
  reactiveData <- eventReactive(input$updateBtn, {
    df <- data.frame(A = c(1, 2, 3),
                     B = c(4, 5, 6))
    
    new_value <- input$value
    df$C <- new_value
    
    # Return the updated dataframe
    df
  })
  
  # Render the updated dataframe in the output
  output$dataTable <- renderTable({
    reactiveData()
  })
}

shinyApp(ui, server)
