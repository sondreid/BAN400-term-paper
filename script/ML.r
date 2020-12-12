######################### ML script/prediction script #################################
"Support for utf-8 encoding"
options(encoding="utf-8") 


"Libraries"
library(caret)
library(dplyr)
library(magrittr)
library(gbm)
library(docstring)
library(doParallel)
library(randomForest)
library(forecastML)

"Loading data frames retrieved from standardisation.r"

#load("../datasett/processed_data_all_countries.Rda")

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
            year   = as.factor(year),
            week = as.factor(week)) %>%
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

#
linnearreg <- train(excess_deaths ~ deaths,
                 data = training_data,
                 method = "lm")


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

#Print relevant statistics
print(linnearreg)
summary(linnearreg)
## RF model
print(RFmodel)

##SVM model
print(SVMmodel)
print(SVMRadialmodel)

stopCluster(cl) #Stop cluster 

### Evaluation -----------------------------------------------------------------------

modelList <- list(SVMmodel, RFmodel, SVMRadialmodel)
evaluateModels <- function(modelList, testData, feature) {
  #' Returns the best fitted model based on RMSE ( )
  #'@param modelList: list of potential models
  bestRMSE <- Inf
  bestModel <- modelList[1]
  for (model in modelList) {
    prediction <- predict(model, newdata = testData)
    RMSE <- postResample(pred = prediction, obs = testData[[feature]])[[1]]
    if (RMSE < bestRMSE) {
      bestRMSE <- RMSE
      bestModel <- model
    }
  }
  return (bestModel)
}


bestModel <- evaluateModels(modelList, test_data, "excess_deaths")
print(bestModel)
predict(linearreg, test_data)

rfPrediction <- predict(RFmodel, newdata = test_data)
svmPrediction <- predict(SVMmodel, newdata = test_data)

bestPred <- predict(bestModel, newdata = test_data)
 
postResample(pred = rfPrediction, obs = test_data$excess_deaths) #Evaluate

#### Forecast ----------------------------------------------------
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
date_frequency <- "1 week"
outcome_column <- 6


forecast_intrain <- createDataPartition(y = totaldata$excess_deaths,
                               p = 0.8,
                               list = FALSE)

forecast_train_data <- forecastData[forecast_intrain]
forecast_test_data <-  forecastData[-forecast_intrain]
forecast_data_list <- create_lagged_df(forecastData, 
                                        type = "train", 
                                        method = "direct", 
                                        outcome_col = outcome_column, 
                                        lookback = 1:15, 
                                        horizons = 1:10,
                                       #groups?
                                        dynamic_features ="law")


windows <- create_windows(lagged_df = forecast_data_list, 
                          window_length = 1)

model_function <- function(data) {
  #'
  
  outcome_names <- names(data)[outcome_column]
  #print(paste("outcome names", outcome_names))
  model_formula <- formula(paste0(outcome_names,  "~ ."))
 # print(paste("formula:", model_formula))
  
  model <- randomForest::randomForest(model_formula, data = data, ntree = 200)
  return(model)
}

forecast_model <- forecastML::train_model(forecast_data_list, windows, model_name = "RF", 
                                           model_function, use_future = FALSE)

#pred <- predict(forecast_model, newdata = forecast_test_data)

pred_function <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}


data_results <- predict(forecast_model,
                        prediction_function = list(pred_function),
                        data = forecast_data_list)



stopCluster(cl) #Stop cluster 







model_function <- function(df) {
  x <- as.matrix(df[, -1, drop = FALSE])
  print(x)
  y <- as.matrix(df[, 1, drop = FALSE])
  model <- glmnet::cv.glmnet(x, y)
}
testmodel <- model_function(data_train)

forecast_model <- forecastML::train_model(forecast_train_data, windows, model_name = "LASSO", model_function = model_function)

### Prediction ---------------------------------------------...
predict_excess_deaths <- function(model = bestModel, country,gender,agegroup,deaths){
    #'
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
#Test prediction

predict_excess_deaths(model = RFmodel, country = "UK", gender = "All", agegroup = "All", deaths = 14276)
#Best model
predict_excess_deaths(country = "France", gender = "F", agegroup = "85+", deaths = 5600)


MLdata <- totaldata
# Save model -----------------------------------------
#'Save the model in a Rda file for quick loading into memory for use in other rscripts (such as the shiny application)
save(bestModel, MLdata, predict_excess_deaths, file = "Shiny/data/MLModel.Rda")


