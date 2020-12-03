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


"Loading data frames retrieved from standardisation.r"

load("../datasett/processed_data_all_countries.Rda")

"load totaldata"
load("Shiny/data/totaldata.Rda")


#### Data selection --------------------------------------------
aggregateAgegroup <- function() {
  totaldata %<>% 
    transform(week = as.factor(week),
              country = as.factor(country),
              year   = as.factor(year)) %>%
    group_by(week, year, gender, country) %>% 
    summarise(deaths = sum(deaths),
      expected_deaths = sum(expected_deaths),
      excess_deaths = sum(excess_deaths))

}
 
totaldata %<>% 
  transform(
            country = as.factor(country),
            year   = as.factor(year)) %>%
  select(country, gender, agegroup, deaths, excess_deaths)                  
                  
      
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

modelList <- list(SVMmodel, RFmodel)
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

 
postResample(pred = rfPrediction, obs = test_data$excess_deaths) #Evaluate



### Prediction #---------------------------------------------
get_predicted_score <- function(model = bestModel, country,gender,agegroup,deaths){
    #'
    #'Returns a prediction based on an input regression model and attributes of country, gender, agegroup and number of deaths
    #'As per default the bestModel is chosen
    df <- 
      data.frame(
        country = as.factor(country),
        gender = as.factor(gender),
        agegroup = as.factor(agegroup),
        deaths = as.numeric(deaths)
      )
    return (predict(model,
                    newdata = df))
  }
#Test prediction

get_predicted_score(model = RFmodel, country = "France", gender = "F", agegroup = "85+", deaths = 5600)
#Best model
get_predicted_score(country = "France", gender = "F", agegroup = "85+", deaths = 5600)



# Save model -----------------------------------------
save(bestModel, get_predicted_score, file = "data/MLModel.Rda")
load("test.Rda")
