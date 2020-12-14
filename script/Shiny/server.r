library(shiny)
library(ggplot2)
library(rsconnect)
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
library(formattable)

load(file = "data/totaldata.Rda")
load(file = "data/MLModel.Rda")

source("ui.r") # Load UI as per defined in ui.r
server <- function(input, output) {
  
  longTable_data <- totaldata %>%
    select(-year)
  # Datatable
  output$tableALL  <- DT :: renderDataTable({
    #load(file="data/longTable_data.Rda")
    datatable(data = longTable_data, 
              colnames = c("Gender", 
                           "Agegroup", 
                           "Week", 
                           "Country", 
                           "Deaths", 
                           "Expected Deaths", 
                           "Excess Deaths"),
              filter = "top",
              options = list(pageLength = 10, autoWidth = 5, lengthMenu = c(5,10,15,50,100,nrow(longTable_data)), scrollX = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#0269A4', 'color': '#fff'});",
                               "}")),
              class = 'cell-border stripe')
    
    #longTable(5, TRUE, c(5, 10, 15, 20, 50, 100, 200, 500))}
  })
  
  # Plot corresponding to datatable
  output$ggplotTable <- renderPlotly ({
    longTable_data_selection <- longTable_data[, c("week", "excess_deaths")] # Plot selection data
    s1 <- input$tableALL_rows_all  # All filtered rows
    df1 <- longTable_data_selection 
    
    if (length(s1)) {
      df2 <- longTable_data_selection[s1, , drop = FALSE] 
      dat <- rbind(df1, df2) 
      dat$Group <- rep(factor(c("Original data", "Filtered data")),times = c(nrow(df1),nrow(df2)))
    }
    else {
      dat <- df1
      dat$Group <- rep("Original data", times = nrow(df1))
    }
    cols = c("Firebrick")
    plot <- dat %>%
      ggplot() +
      geom_smooth(aes(x = week,
                      y = excess_deaths,
                      colour = Group)) +
      labs(x = "Weeks", y = "Excess deaths") +
      theme(plot.background=element_rect()) +
      ggtitle("Filtered plot")
    ggplotly(plot)

    
  })
  # Data treatment for plot at page 1
  data <- reactive({
    totaldata[totaldata$week >= input$week[1] & totaldata$week <= input$week[2] & totaldata$country %in% input$countries,]
  })
  #' Plot at page 1
  output$plot <- renderPlotly({
    
    p <- ggplot(data(), aes_string(x="week", y=input$y))
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    #if (input$jitter)
     # p <- p + geom_jitter()
    
    if (input$smooth)
      p <- p + geom_smooth()
    
    if (input$actual)
      p <- p + geom_point()
    
    if (input$expected)
      p <- p + geom_smooth(data = data(), 
                           aes(x = week, y = expected_deaths))
    ggplotly(p)
    
  })
  
  # Predicted excess deaths in ML tab
  output$prediction_excess_deaths <- renderText({
    paste(predict_excess_deaths(country = input$country,  
                                gender = input$gender, 
                                agegroup = input$agegroup, 
                                deaths = input$deaths))
  })
  output$shortable <- renderFormattable({shortTable()})
  
}


# Remove comment for live test
#shinyApp(ui, server)


