# Load packages ----------------------------------------------------------------

library(shiny)


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("An image"),
  tags$img(src = "Kim, D 380946.jpg", width = "400px", height = "300px"),
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {}

# Create the Shiny app ---------------------------------------------------------

shinyApp(ui = ui, server = server)