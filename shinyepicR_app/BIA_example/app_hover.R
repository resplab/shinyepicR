library(shiny)
library(shinyBS)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .my-tooltip {
        cursor: help;
        text-decoration: underline;
      }
    "))
  ),
  mainPanel(
    p("Hover over the word ", tags$a(class = "my-tooltip", "here"), " for a tooltip.")
  )
)

server <- function(input, output) {
  observe({
    bsTooltip("a.my-tooltip", "This is a tooltip messasge.")
  })
}

shinyApp(ui, server)