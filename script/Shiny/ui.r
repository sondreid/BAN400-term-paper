library(shiny)
library(ggplot2)
library(plotly)
library("formattable")

load("data/totaldata.Rda")
load("data/MLModel.Rda")


data <- totaldata



ui <- fluidPage(
  
  fluidRow(
    #column(width = 1, tags$a(href = "https://olavik17.shinyapps.io/shiny/?_ga=2.219468638.671758994.1603877082-1160339763.1603877082", tags$img(height = 70, width = 125, src = "www/covid_19.png"))),
    column(width = 12, align = "center", headerPanel(tags$h1("Covid-19 dashboard")))
  ),
  
  
  fluidRow(
    column(width = 12, align = "center", tags$p("A dashboard of excess deaths statistics in the Covid-19 pandemic", tags$em("Covid-19"), "in", tags$strong("Shiny"), "app.") )
  ),
  
  tabsetPanel(
    tabPanel(title = "Main",
             
             br(),
             p("Try to make your prefered plotoutput" ,style="text-align:justify;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
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
                 
                 # selectInput(inputId = 'x', 
                 #             label = 'X', 
                 #             choices = c("week"),
                 #             selected = "week"),
                 
                 selectInput(inputId = 'y', 
                             label = 'Y', 
                             choices = c(Deaths = "deaths", "Excess Deaths" = "excess_deaths", "Expected Deaths" = "expected_deaths"), 
                             selected = "deaths"),
                 
                             
                selectInput(inputId = 'color', 
                            label = 'Color', 
                            choices = c('None', Gender = "gender", Agegroup = "agegroup", Week = "week", Country = "country", Deaths = "deaths", "Expected Deaths" = "expected_deaths", "Excess Deaths" = "excess_deaths"),
                            selected = "gender"),
                 
                 #checkboxInput(inputId = 'jitter', 
                  #             label = 'Jitter'),
                 
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
                             choices = c(None='.', Gender = "gender", Agegroup = "agegroup", Country = "country"),
                             selected = "agegroup"),
                 
                 selectInput(inputId ='facet_col', 
                             label = 'Facet Column', 
                             choices = c(None='.', Gender = "gender", Agegroup = "agegroup", Country = "country"),
                             selected = "country"),
                
               ),
               mainPanel(
                 plotlyOutput('plot', height = 700)
                  
               ))
             ),
    
    # Plot adjusted by table         
    tabPanel(title = "Table", 
             br(),
             p("By filtering data in the table, the plot will adjust" ,style="text-align:justify;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
               
                 DT::dataTableOutput("tableALL"),
                 
                 width = 7
               ),
             
               mainPanel(
                 plotlyOutput('ggplotTable', height = 500),

                 width = 5
               
               ))
             
               
             ),
    # ML prediction
    tabPanel(title = "Prediction",
             br(),
             p("Prediction by machine learning", style="text-align:justify;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
          
             sidebarLayout(
                 sidebarPanel(
                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
                   
                   selectInput(inputId = "country", 
                             label = h3("Country"),
                             choices = levels(MLdata$country),
                             selected = "France"),
                   selectInput(inputId = "gender", 
                             label = h3("Gender"),
                             choices = levels(MLdata$gender),
                             selected = "F"),
                   selectInput(inputId = "agegroup", 
                             label =h3("Agegroup"),
                             choices = levels(MLdata$agegroup),
                             selected = "0-64"),
            
                   numericInput(inputId = "deaths", 
                                h3("Deaths"),
                                value = 0)
                   ),
               mainPanel(
                 br(),
                 h3("Prediction"),
                 p("Based on the data of five european countries we split our data into train data and test data in 80/20 ratio.\n"),
                 p("We define several models based on the features country, gender, agegroup and the number of deaths in a given week. The best
                   model based is chosen based on its RMSE (Root-mean-square error)"),
                 br(),
                 h4("Excess deaths based on input parameters:", style="text-align:center"),
                 h3(textOutput("prediction_excess_deaths"), style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
                 br()

               ),
              )
             ),
    
    #Short tabel 
    tabPanel(title = "Short Tabel",
             br(),
             p("Short tabel", style="text-align:justify;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 h3(formattableOutput("shortable"))
               ),
             )
             
             
            
  ),

)







