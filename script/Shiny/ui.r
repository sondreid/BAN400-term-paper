#setwd("/Users/olaiviken/OneDrive - Norges Handelshøyskole/MASTER/BAN400/Term paper/BAN400-term-paper/script/Shiny")
library(shiny)
library(ggplot2)

load("totaldata.Rda")

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
                 
                 checkboxInput('jitter', 'Jitter'),
                 checkboxInput(inputId = 'smooth', 
                               label = 'Smooth'),
                 
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
             
    tabPanel(title = "Plot", "Interactive plots")
  ),

)







