# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB RATING" = "imdb_rating", 
                                 "# IMDB VOTES" = "imdb_num_votes", 
                                 "CRITICS SCORE" = "critics_score", 
                                 "AUDIENCE SCORE" = "audience_score", 
                                 'MOVIE TIME (MIN)' = "runtime"), 
                  selected = "audience_score"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("IMDB RATING" = "imdb_rating", 
                              "# IMDB VOTES" = "imdb_num_votes", 
                              "CRITICS SCORE" = "critics_score", 
                              "AUDIENCE SCORE" = "audience_score", 
                              'MOVIE TIME (MIN)' = "runtime"), 
                  selected = "critics_score"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color:",
                  choices = c("TITLE" = "title_type", 
                              "Genre" = "genre", 
                              "MPAA RATING" = "mpaa_rating", 
                              "CRITICS RATING" = "critics_rating", 
                              'AUDIENCE RATING' = "audience rating"),
                  selected = "mpaa_rating"),
      
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y,
                                     color = input$z)) +
      geom_point()
  })
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)