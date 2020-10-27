library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  checkboxGroupInput(inputId = "check",
                     label = "Checkbox Group",
                     choiceNames = c("True", "False", "Both"),
                     )
  #submitButton(inputId = "act",
   #            label = "Remove")
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)

ui <- fluidPage(
  titlePanel("Plots"),
  

)

server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

