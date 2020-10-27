library(shiny)

source("analysis.r")



ui <- fluidPage(
  titlePanel("Plots"),
  

)

server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

