library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output){
  output$hist <- renderPlot({
    hist(rnorm(input$num), 
         main = "100 random normal values")
    })
}

shinyApp(ui = ui, server = server)

ui <- fluidPage(
  titlePanel("Plots"),
  

)

server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

