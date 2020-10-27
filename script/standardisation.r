###################### Data standardisation script ##########################

"Libraries "
library(readxl)
library(readr)
library(tidyverse)
library(types)
library(lubridate)
library(janitor)  



" Retrieve data standard excel sheet "
getStandard <- function() {return("../datasett/Datastandard/data_standard.xlsx") }



#### Gender -----------------------------


standardiseGender <- function(df, genderVec) {
  standardGender <- read_excel(getStandard()) %>% select(Gender)
  for (k in 1:nrow(df)) {
    x <- df[["gender"]][k]
    for (i in 1:length(genderVec)) {
      if (x == genderVec[i]) df[["gender"]][k] <- standardGender[[1]][i]
    }
   
  }
  df <- df%>% drop_na()
  return (df)
}


#Tests
genderVec <- c("Menn", "Kvinner", "Begge kjønn")
testDF <- standardiseGender(data_norway, genderVec)


 
# Transformations should be done in standardisation

# Norway

norway_standard <- data_norway  %>% standardiseGender(., genderVec) %>%
  transform(gender = as.factor(gender),  
            agegroup = as.factor(agegroup),
            week = as.numeric(week),
            year = as.numeric(year),
            deaths = as.numeric(deaths),
            country = as.factor(country)) # Standardized data type for each col.
