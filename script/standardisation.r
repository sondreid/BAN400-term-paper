###################### Data standardisation script ##########################

"Libraries "
library(readxl)
library(readr)
library(tidyverse)
library(types)
library(lubridate)
library(janitor)  
library(docstring)


" Retrieve data standard excel sheet "
getStandard <- function() {return("../datasett/Datastandard/data_standard.xlsx") }



#### Gender -----------------------------


standardiseRows <- function(df, genderVec) {
  #' In column gender, the function loops through all rows.
  #' If it finds a match between the genderVector and standardGender it will return the standardGender
  #' NA otherwise
  #' @param df input dataframe
  #' @param genderVec vector of original gender formatting in df
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



standardiseGender <- function(df, genderVec) {
  #' Standardizes a dataframe according to the standards present in getStandard() sheet
  #' @param df input dataframe
  #' @param genderVec vector of original gender formatting in df
  df <- df %>% standardiseRows(., genderVec) %>% 
    drop_na() %>%
    transform(gender = as.factor(gender),  
              agegroup = as.factor(agegroup),
              week = as.numeric(week),
              year = as.numeric(year),
              deaths = as.numeric(deaths),
              country = as.factor(country)) 
  return (df)
}



# Norway

norway_standard <- standardiseGender(data_norway, c("Menn", "Kvinner", "Begge kjønn"))
save(data_norway, file = "../results/data_norway.Rda")  # Save to .rda file
