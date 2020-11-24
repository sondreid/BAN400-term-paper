######################### ML script/prediction script #################################
"Support for utf-8 encoding"
options(encoding="utf-8") 


"Libraries"
library(caret)
library(dplyr)
library(magrittr)
library(gbm)
library(docstring)


"Loading data frames retrieved from standardisation.r"

load("../datasett/processed_data_all_countries.Rda")

"load totaldata"
load("Shiny/data/totaldata.Rda")

totaldata %<>% transform(week = as.factor(week),
                         country = as.factor(country),
                         year   = as.factor(year))
intrain <- createDataPartition(y = totaldata$excess_deaths,
                                    p = 0.8,
                                    list = FALSE)


training_data <- totaldata[intrain,] %>% select(excess_deaths,week)
test_data     <- totaldata[-intrain,]

.f <- function() {
  gbmModel <- train(excess_deaths ~.,
                  distribution = "bernoulli",
                  data = training_data,
                  n.trees = 500,
                  interaction.depth = 4,
                  shrinkage = 0.01)

}
#gbmModel <- glm(excess_deaths ~., data = training_data, family = binomial)
linearreg <- train(excess_deaths ~.,
                   data = training_data,
                   method = "lm")
print(linearreg)
summary(linearreg)


### Prediction #--------------------------
