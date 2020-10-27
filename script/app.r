library(shiny)
source("analysis.r")


ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput(outputId = "hist"),
  dataTableOutput(outputId = "table")
)

server <- function(input, output){
  output$hist <- renderPlot({
    hist(rnorm(input$num), 
         main = "100 random normal values")
    })
  output$table <- renderDataTable({
    longTable()
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

