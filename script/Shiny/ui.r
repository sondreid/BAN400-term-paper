library(shiny)
library(ggplot2)

load("data/totaldata.Rda")

data <- totaldata


ui <- fluidPage(
  
  fluidRow(
    column(width = 1, tags$a(href = "https://olavik17.shinyapps.io/shiny/?_ga=2.219468638.671758994.1603877082-1160339763.1603877082", tags$img(height = 70, width = 125, src = "covid_19.png"))),
    column(width = 10, align = "center", headerPanel(tags$h1("Covid-19")))
  ),
  
  
  fluidRow(
    column(width = 12, align = "center", tags$p("This is the story about", tags$em("Covid-19"), "in", tags$strong("Shiny"), "app.") )
  ),
  
  tabsetPanel(
    tabPanel(title = "Main",
             
             tags$br(),
             
             fluidRow(
               column(width = 3, tags$a(href = "https://sondreid.shinyapps.io/shiny/?fbclid=IwAR1ql4x9akYJ-kHedYXDVWtsnNnG8o7UrTIAS9IdjNoVj15aP9qleHOu4YM", tags$img(height = 50, width = 132, src = "shiny.png")),
                      tags$a(href = "https://sondreid.shinyapps.io/shiny/?fbclid=IwAR1ql4x9akYJ-kHedYXDVWtsnNnG8o7UrTIAS9IdjNoVj15aP9qleHOu4YM", "Sondres App"),
               )
             ),
             
             tags$hr(),
             
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput(inputId = "week",
                             label = "Range of weeks",
                             min = min(data$week),
                             max = max(data$week),
                             value = c(min(data$week), max(data$week))),
                 
                 checkboxGroupInput(inputId = "countries",
                                    label = "Select countries",
                                    choices = levels(data$country),
                                    choiceNames = names(data),
                                    selected = levels(data$country)),
                 
                 selectInput(inputId = 'x', 
                             label = 'X', 
                             choices = names(data),
                             selected = "week"),
                 
                 selectInput(inputId = 'y', 
                             label = 'Y', 
                             choices = names(data), 
                             selected = "deaths"),
                 
                             
                selectInput(inputId = 'color', 
                            label = 'Color', 
                            choices = c('None', names(data)),
                            selected = "gender"),
                 
                 checkboxInput(inputId = 'jitter', 
                               label = 'Jitter'),
                 
                checkboxInput(inputId = 'smooth', 
                              label = 'Smooth',
                              value = TRUE),
                
                checkboxInput(inputId = "actual",
                             label = "Actual deaths",
                             value = TRUE),
                
                 checkboxInput(inputId = "expected",
                               label = "Expected deaths",
                               value = TRUE),
                 
                 selectInput(inputId = 'facet_row', 
                             label = 'Facet Row', 
                             choices = c(None='.', names(data)),
                             selected = "agegroup"),
                 
                 selectInput(inputId ='facet_col', 
                             label = 'Facet Column', 
                             choices = c(None='.', names(data)),
                             selected = "country"),
                
               ),
               mainPanel(
                 plotOutput('plot')
               ))
             ),
    
    # Partners         
    tabPanel(title = "Table", 
             fluidRow(
               
               br(),
               p("By filtering data in the table, the plot will adjust" ,style="text-align:justify;color:white;background-color:gray;padding:15px;border-radius:10px"),
               br()),
             
             fluidRow(  
               column(6,DT::dataTableOutput("tableALL"), style = "color:white; "),
               column(6, plotlyOutput('ggplotTable', height = 500), style = "color:white; ")
             )),
    tabPanel(title = "Prediction by machine learning",
             fluidrow(
               br(),
               p("Prediction"),
               br())
             )
  ),

)







