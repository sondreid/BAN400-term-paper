######################### Server script for shiny app #################################

source("ui.r") # Load UI as per defined in ui.r

# Creating a server
server <- function(input, output) {
  
  # Deselects the column year in totaldata and store it as longTable_data
  longTable_data <- totaldata %>%
    select(-year)
  
  # Creating an output which renders a data table with columnnames from longTable_data
  # Creates a dropdown menu in datatable which defines the length of the datatable with 5, 10, 15, 50, 100 or length of the 
  # dataframe recordings. 
  output$tableALL  <- DT :: renderDataTable({

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
  
  # Data treatment for plot at Interactive Plot tab
  # This filters out the weeks according to input min and max week from the slider.
  data <- reactive({
    totaldata[totaldata$week >= input$week[1] & totaldata$week <= input$week[2] & totaldata$country %in% input$countries,]
  })
  
  # Generates a plot including different preferred option selected in the sidebar. 
  output$plot <- renderPlotly({
    
    p <- ggplot(data(), aes_string(x="week", y=input$y))
    
    # Changes color according to the dropdown menu inputted by user. 
    if (input$color != 'None') 
      p <- p + aes_string(color=input$color)
    
    # Variables that stores the row and column facets chosen by user.
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    # Creates the new grid if the user has chosen this in the menu
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    # Plotting a smoothed regression curve in the plot if user choose this
    if (input$smooth)
      p <- p + geom_smooth()
    
    # Plotting the actual data points if user choose this
    if (input$actual)
      p <- p + geom_point()
    
    # Plotting the expected deaths as a smooth regression curve
    if (input$expected)
      p <- p + geom_smooth(data = data(), 
                           aes(x = week, y = expected_deaths))
    
    # Creating a interactive plot with ggplotly-function
    ggplotly(p)
    
  })
  
  # Predicted excess deaths in ML tab
  output$prediction_excess_deaths <- renderText({
    paste(predict_excess_deaths(country = input$country,  
                                gender = input$gender, 
                                agegroup = input$agegroup, 
                                deaths = input$deaths))
  })
  
  # Plotting the forecast
  output$forecast_plot <- renderPlotly ({
    generate_forecast_plot(countryname = input$countryname)
  })
  
  # Printing the shorttable. 
  output$shortable <- renderFormattable({shortTable()})
  
}



