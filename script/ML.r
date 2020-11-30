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


training_data <- totaldata[intrain,] #%>% select(excess_deaths,week)
test_data     <- totaldata[-intrain,]

.f <- function() {
  gbmModel <- train(excess_deaths ~.,
                  distribution = "bernoulli",
                  data = training_data,
                  n.trees = 500,
                  interaction.depth = 4,
                  shrinkage = 0.01)

}


## Make a model-----------------------------------------

linnearreg <- train(excess_deaths ~ week,
                 data = training_data,
                 method = "lm")

cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
RFmodel <- train(excess_deaths ~.,
                   data = training_data,
                   method = "rf")
print(linnearreg)
summary(linnearreg)
print(RFmodel)


### Prediction #--------------------------
predict(linearreg, test_data)
predict(RFmodel, test_data)
