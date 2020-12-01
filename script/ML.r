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
  transform(week = as.factor(week),
            country = as.factor(country),
            year   = as.factor(year)) %>%
  select(country, week, gender, agegroup, deaths, excess_deaths)                  
                  
      
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
linnearreg <- train(excess_deaths ~ week,
                 data = training_data,
                 method = "lm")


control <- trainControl(method = "repeatedcv",
                 n = 10,
                 repeats = 5)


RFmodel <- train(excess_deaths ~.,
                 data = training_data,
                 method = "rf",
                 metric = "RMSE",
                 trcontrol = control)




#Print relevant statistics
print(linnearreg)
summary(linnearreg)
print(RFmodel)

stopCluster(cl) #Stop cluster 

### Prediction #---------------------------------------------
predict(linearreg, test_data)

rfPrediction <- predict(RFmodel, newdata = test_data)
 
postResample(pred = rfPrediction, obs = test_data$excess_deaths) #Evaluate


get_predicted_score <- function(model = RFmodel, week,gender,agegroup,deaths){
    
    df <- 
      data.frame(
        week = as.factor(week),
        gender = as.factor(gender),
        agegroup = as.factor(agegroup),
        deaths = as.numeric(deaths)
      )
    
    
    df <- totaldata
    
    data.frame(
      scoring_date = as.Date(Sys.Date()),
      prob = 
        predict(
          model, 
          newdat = df,
          type = "prob")$Default)
  }
