######################### UI script for shiny app #################################

## Libraries -------------
library(shiny)
library(ggplot2)
library(rsconnect)
library(kableExtra) 
library(tidyverse)    
library(tidygraph)    
library(hrbrthemes)   
library(viridis)      
library(dygraphs)     
library(xts)          
library(lubridate)    
library(plotly)     
library(types)        
library(pdftools)     
library(gt)           
library(scales)       
library(formattable)  
library(DT)
library(magrittr)
library(caret)
library(randomForest)
library(gbm)
library(forecastML)

## Load RDA files from data folder
load(file = "data/totaldata.Rda")
load(file = "data/MLModel.Rda")
load(file = "data/tableData.Rda")

# Store totaldata as data
data <- totaldata

# Creating a user interface
ui <- fluidPage(
  
  # Creating text separated by rows with fluidRow. These fluidRows can be
  # seen on each page. 
  fluidRow(
    column(width = 12, 
           align = "center", 
           headerPanel(tags$h1("Covid-19 Dashboard")))
  ),
  
  fluidRow(
    column(width = 12, 
           align = "center", 
           tags$p(tags$strong("BAN400 Home Exam"), 
                  tags$em("created by candidate: 3 and 55")))
  ),
  
  # tabsetPanel function organize all the tabPanel in a common structure. 
  tabsetPanel(
    
    # Main page with short table
    tabPanel(title = "Main",
             
             br(),
             p("Short table", 
               style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             # Creates a sidebar and a main panel, but in this case we use the sidepanel with full width in order to print the short table.
             sidebarLayout(
               
               sidebarPanel(
                   h5(formattableOutput("shortable")),
                   width = 12
                 ),
               
                 mainPanel(
                   width = 0
                 )
             ),
    
             p("The data sets used in this analysis are gathered from a statistics bureaus of five european countries.", br(), 
               "The countries inhibit a common geographic area, but have opted for very different response patterns to combat the spread of covid19.
               The goal of this dashboard is to give a precise as possible view of the state of the excess mortality observed in our selection of countries.
               We note that excess mortality is not to be confused with actual deaths caused by Covid-19.", br(),br(),
               " The sources of the data sets are as follows:",br(), br(),
               "UK:     https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths",br(),
               "France: https://www.insee.fr/en/statistiques/4493808?sommaire=4493845", br(),
               "Denmark: https://www.statbank.dk/dodc2", br(),
               "Sweden: https://www.scb.se/en/finding-statistics/statistics-by-subject-area/population/population-composition/population-statistics/", br(),
               "Norway: https://www.ssb.no/statbank/table/07995/", style="text-align:justify;color:black;background-color:#F0F0F0;padding:15px;border-radius:10px")
    ),
    # New tab with interactive plot 
    tabPanel(title = "Interactive Plot",
             
             br(),
             p("Try to make your prefered plotoutput" ,
               style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             
             sidebarLayout(
               # Creating a sidebar in order to create preferred plots
               sidebarPanel(
                 
                 # This code styles the sidepanel with grey background
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
                 
                 # Creates a slider in interval [min, max] of the week in data
                 sliderInput(inputId = "week",
                             label = "Range of weeks",
                             min = min(data$week),
                             max = max(data$week),
                             value = c(min(data$week), max(data$week))),
                 
                 # Checkbox in order to filter the countries
                 checkboxGroupInput(inputId = "countries",
                                    label = "Select countries",
                                    choices = levels(data$country),
                                    choiceNames = names(data),
                                    selected = levels(data$country)),
                 
                 # Creates a dropdown menu where the user can choose which Y-output should be. 
                 selectInput(inputId = 'y', 
                             label = 'Y', 
                             choices = c(Deaths = "deaths", 
                                         "Excess Deaths" = "excess_deaths", 
                                         "Expected Deaths" = "expected_deaths"), 
                             selected = "deaths"),
                 
                 # Creates a dropdown menu where the user can filter the plots category with color.
                 selectInput(inputId = 'color', 
                             label = 'Color', 
                             choices = c('None', 
                                         Gender = "gender", 
                                         Agegroup = "agegroup", 
                                         Week = "week", 
                                         Country = "country", 
                                         Deaths = "deaths", 
                                         "Expected Deaths" = "expected_deaths", 
                                         "Excess Deaths" = "excess_deaths"),
                             selected = "gender"),
                 
                 # The opportunity for user to plot a smoothed curve in the graph
                 checkboxInput(inputId = 'smooth', 
                               label = 'Smooth',
                               value = TRUE),
                 
                 # The opportunity for user to plot the actual deaths as points
                 checkboxInput(inputId = "actual",
                               label = "Actual deaths",
                               value = TRUE),
                 
                 # The opportunity for user to plot a curve with expected deaths.
                 checkboxInput(inputId = "expected",
                               label = "Expected deaths",
                               value = TRUE),
                 
                 # Dropdown menu that splits the data by categorical data in different groups by rows
                 selectInput(inputId = 'facet_row', 
                             label = 'Facet Row', 
                             choices = c(None='.', 
                                         Gender = "gender", 
                                         Agegroup = "agegroup", 
                                         Country = "country"),
                             selected = "agegroup"),
                 
                 # Dropdown menu that splits the data by categorical data in different groups by columns
                 selectInput(inputId ='facet_col', 
                             label = 'Facet Column', 
                             choices = c(None='.', 
                                         Gender = "gender", 
                                         Agegroup = "agegroup", 
                                         Country = "country"),
                             selected = "country"),
                 
               ),
               
               # Plots the preferred filtered data into a interactiv plot by plotly
               mainPanel(
                 plotlyOutput('plot', height = 700)
                 
               ))
    ),
    
    # Plot adjusted by table         
    tabPanel(title = "Interactive Table",
             
             br(),
             p("By filtering data in the table, the plot will adjust",
               style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             sidebarLayout(
               
               # Makes an interactive tabel where the user can choose data to be shown in the interactive plot in the mainpanel
               sidebarPanel(
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
                 DT::dataTableOutput("tableALL"),
                 width = 7
               ),
               
               mainPanel(
                 plotlyOutput('ggplotTable', 
                              height = 500),
                 width = 5
               )
               
               )
             
               
             ),
    
    # Machine Learning of excess deaths
    tabPanel(title = "Prediction",
             
             br(),
             p("Prediction by machine learning",
               style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             sidebarLayout(
               
               # Creates a sidebarpanel in order to select which country, gender, agegroup and how manu known deaths in order 
               # to print the outputted excess deaths with machine learning
               sidebarPanel(
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
                 
                 # Dropdown menu of countries to be selected
                 selectInput(inputId = "country", 
                             label = h3("Country"),
                             choices = levels(MLdata$country),
                             selected = "France"),
                 
                 # Dropdown menu of genders
                 selectInput(inputId = "gender", 
                             label = h3("Gender"),
                             choices = levels(MLdata$gender),
                             selected = "F"),
                 
                 # Dropdown menu of agegroups
                 selectInput(inputId = "agegroup", 
                             label =h3("Agegroup"),
                             choices = levels(MLdata$agegroup),
                             selected = "0-64"),
                 
                 # A numrical input of known deaths
                 numericInput(inputId = "deaths", 
                              h3("Deaths"),
                              value = 0)
               ),
               mainPanel(

                 p("Based on the data of five european countries we split our data into train data and test data in 80/20 ratio.\n", br(),
                   "We define several models based on the features country, gender, agegroup and the number of deaths in a given week. The best
                   model based is chosen based on its RMSE (Root-mean-square error)",style="text-align:justify;color:black;background-color:#F0F0F0;padding:15px;border-radius:10px"),
                 br(),
                 h4("Excess deaths based on input parameters:", style="text-align:center"),
                 
                 # Printing out the machine learned prediction of excess deaths.
                 h3(textOutput("prediction_excess_deaths"), style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
                 br()
                 
               )
             )
    ),
    
    # Forecasting the next 10 weeks by forecastML package. 
    tabPanel(title = "Forecast",
             
             br(),
             p("10 week forecast by forecastML", 
               style="text-align:center;color:white;background-color:#0269A4;padding:15px;border-radius:10px"),
             br(),
             
             sidebarLayout(
               
               # Sidebarpanel where the user can choose the country to forecast. 
               sidebarPanel(
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: gray}")),
                 
                 # Dropdown menu with different countries
                 selectInput(inputId = "countryname", 
                             label = h3("Country"),
                             choices = levels(MLdata$country),
                             selected = "Norway")
               ),
               
               # Plots the excess deaths forecast of 10 weeks.
               mainPanel(
                 p("We train a model based on the forecastML cran package.
                   A random forest model is used to predict the number of excess deaths for a horizon of 10 weeks",
                   style="text-align:justify;color:black;background-color:#F0F0F0;padding:15px;border-radius:10px"),
                 plotlyOutput("forecast_plot")
               )
             )
             
    )
    
  )
  
)
  













