######################### ML script/prediction script #################################
"Support for utf-8 encoding"
options(encoding="utf-8") 


"Libraries"
library(caret)
library(dplyr)
library(magrittr)
library(gbm)
library(docstring)
library(ggplot2)
library(doParallel)
library(randomForest)
library(plotly)
library(forecastML)

"Loading data frames retrieved from standardisation.r"


"load totaldata"
load("Shiny/data/totaldata.Rda")


#### Data selection --------------------------------------------
aggregateColumn <- function(df = totaldata,
                              column) {
  #' Function that aggregates a column of an input dataframe by grouping all columns except the chosen column
  #' @param df: input dataframe, per default totaldata
  #' @param column: column to be aggregated
  #' @return : a modified dataframe
  columns <- c("week", "country", "gender", "agegroup")
  columns %<>% setdiff(., column) 
  print(columns)
  df %<>% 
    group_by_at(columns) %>% 
    summarise(
      deaths = sum(deaths),
      excess_deaths = sum(excess_deaths)) %>% 
    mutate(!!column := "All")
  return (df)
}


totaldata %<>% 
  transform(
            country = as.factor(country),
            year   = as.factor(year)) %>%
  select(week, country, gender, agegroup, deaths, excess_deaths) %>%
  rbind(., totaldata %>%  ## Add aggregate for gender and agegroup (deaths and excess deaths for all genders and agegroups for a week)
          group_by(country, week) %>%
          summarise(gender = "All", agegroup = "All", deaths = sum(deaths), excess_deaths = sum(excess_deaths))) %>%
  rbind(., aggregateColumn(df = totaldata, "agegroup")) %>%
  rbind(., aggregateColumn(df = totaldata, "gender"))

forecastData <- totaldata # Store totaldata with week feature

totaldata %<>% #Keep totaldata as ML prediction dataframe, but without week feature
  select(-week)


                  
intrain <- createDataPartition(y = totaldata$excess_deaths,
                                    p = 0.8,
                                    list = FALSE)
# Splitting data ---------------------------------------------

training_data <- totaldata[intrain,] #%>% select(excess_deaths,week)
test_data     <- totaldata[-intrain,]



## Model creation -----------------------------------------

#Make clusters
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)


control <- trainControl(method = "repeatedcv",
                 n = 15,
                 repeats = 10)


RFmodel <- train(excess_deaths ~.,
                 data = training_data,
                 method = "rf",
                 metric = "RMSE",
                 trcontrol = control)

SVMmodel <- train(excess_deaths ~.,
                  data = training_data,
                  method = "svmPoly",
                  metric = "RMSE",
                  trcontrol = control,
                  tunelength = 4)
SVMRadialmodel <- train(excess_deaths ~.,
                  data = training_data,
                  method = "svmRadial",
                  metric = "RMSE",
                  trcontrol = control,
                  tunelength = 4)

GBMModel <-      gbm(
                 formula = excess_deaths~.,
                 distribution = "gaussian",
                 data = training_data,
                 n.trees = 10000,
                 interaction.depth = 1,
                 shrinkage = 0.001,
                 cv.folds = 5,
                 verbose = FALSE
) 

### Evaluation -----------------------------------------------------------------------

modelList <- list(SVMmodel, RFmodel, SVMRadialmodel, GBMModel)
evaluateModels <- function(modelList, testData, feature) {
  #' Returns the best fitted model based on RMSE ( )
  #' Starts with a best RMSE of infinity, and stores iteratively the best observed RMSE
  #' Returns the best model
  #'@param modelList: list of potential models
  #'@param testData: dataframe, test data in which the model is trained
  #'@param feature: outcome feature
  bestRMSE <- Inf 
  bestModel <- modelList[1]
  for (model in modelList) {
    prediction <- predict(model, newdata = testData)
    RMSE <- postResample(pred = prediction, obs = testData[[feature]])[[1]]
    if (RMSE < bestRMSE) {
      assign("bestRMSE", RMSE,envir = .GlobalEnv)
      assign("bestModel", model,envir = .GlobalEnv)
    }
  }
  return (bestModel)
}


bestModel <- evaluateModels(modelList, test_data, "excess_deaths") # Store best model av bestModel


### Prediction ---------------------------------------------...
predict_excess_deaths <- function(model = bestModel, country,gender,agegroup,deaths){
    #'Returns a prediction based on an input regression model and attributes of country, gender, agegroup and number of deaths
    #'As per default the bestModel is chosen
    #'@param model: input ML model, the best Model determined by evaluateModels() per default
    #'@param country: String, country name
    #'@param gender: String, gender name
    #'@param agegroup: String, interval of ages(predetermined agegroups)
    #'@param deaths : Integer, number of deaths (per week for a given set of attributes)
    df <- 
      data.frame(
        country = as.factor(country),
        gender = as.factor(gender),
        agegroup = as.factor(agegroup),
        deaths = as.numeric(deaths)
      )
    return (as.integer(predict(model,
                    newdata = df)))
  }


#### Forecast ----------------------------------------------------

"Aggregate features
We only want to keep the aggregate of gender and agegroup "
forecastData %<>%
  filter(gender == "All",
         agegroup == "All") %>%
  select(week, country, deaths, excess_deaths)



model_function <- function(df) {
  #' Model function
  #' @param df: input dataframe
  model <- randomForest::randomForest(excess_deaths ~., data = df, ntree = 200)
  return(model)
}
pred_function <- function(model, data_features) {
  #' Prediction function
  #' @param model: a forecast model
  #' @param data_features: features in dataframe (except outcome feature)
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}


train_model_country <- function(countryname, outcome_column = 4, horizon = 10) {
  #' Train a forecast model with
  #' @param countryname: string, name of country
  #' @param outcome_column: column index of outcome feature, default 4
  #' @param horizon: maximum time period to be forecasted
  df <- forecastData %>%
    filter(country == countryname)
  forecast_data_list <- create_lagged_df(df, 
                                         type = "train", 
                                         method = "direct", 
                                         outcome_col = outcome_column, 
                                         lookback = 1:15, 
                                         horizons = 1:horizon,
                                         dynamic_features ="law")
  
  windows <- create_windows(lagged_df = forecast_data_list, 
                            window_length = 0)
  forecast_model <- forecastML::train_model(forecast_data_list, windows, model_name = "RF", 
                                            model_function, use_future = FALSE)
  
  data_forecast_list <- create_lagged_df(df, 
                                         type = "forecast", 
                                         method = "direct", 
                                         outcome_col = outcome_column, 
                                         lookback = 1:15, 
                                         frequency = "week",
                                         horizons = 1:10)
  data_forecasts <- predict(forecast_model, 
                            prediction_function =list(pred_function),
                            data = data_forecast_list) 
  data_forecasts <- forecastML::combine_forecasts(data_forecasts)
  return (data_forecasts)
}



forecast_country <- function(df = forecastData, countryname) {
  #' Function that combines the estimates of excess deaths found in a dataframe (forecastData)
  #' and row binds them to a forecas made by the train_country function
  #'@param df: dataframe with all country data
  #'@param countryname: String name of country
  latestweek <- tail(df[order(df$week),]$week, n =  1)
  country_forecast <- train_model_country(countryname = countryname)
  country_forecast %<>% 
    rename("week" = horizon, "excess_deaths" = excess_deaths_pred) %>% 
    mutate(country = countryname,
           week = week + latestweek,
           type = "Forecast") %>% 
    select(week, country, type, excess_deaths) 
  df %<>% 
    filter(week <= latestweek,
           country == countryname) %>%
    mutate(type = "Estimate") %>%
    select(week, country, type, excess_deaths) 
  
  df %<>% 
    rbind(., country_forecast) %>%
    transform(type = as.factor(type))
  return(df)
  
}


generate_forecast_plot <- function(countryname) {
  #' Function that generates a plot based on the forecast generated by 'forecast_country'
  #' Based on the dataframe generated, display a coloured plot of estimated and forecasted excess_deaths
  #' @param countryname: string, name of country
  df <- forecast_country(countryname = countryname) 
  plot <- df %>%
    ggplot() +
    geom_smooth(aes(x = week,
                    y = excess_deaths,
                    colour = type)) +
    labs(x = "Weeks", y = "Excess deaths") +
    theme(plot.background=element_rect()) +
    ggtitle(paste(countryname, "estimated and forecasted excess deaths"))
  ggplotly(plot)
}

generate_forecast_plot(countryname = "UK")



stopCluster(cl) #Stop cluster 

MLdata <- totaldata #Change name of totaldata
# Save model -----------------------------------------
"Save the prediction ML model and necessary forecast data and functions in a Rda file
 for quick loading into memory for use in other rscripts (such as the shiny application)"
save(bestModel, 
     MLdata, 
     predict_excess_deaths, 
     forecastData, 
     model_function, 
     pred_function,
     generate_forecast_plot,
     forecast_country,
     train_model_country,
     file = "Shiny/data/MLModel.Rda")


