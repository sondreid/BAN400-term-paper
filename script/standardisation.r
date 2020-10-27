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
              year = as.factor(year),
              deaths = as.numeric(deaths),
              country = as.factor(country)) 
  return (df)
}



# Norway
"Saving the data frame to a .Rda-file for the purpose of analysis.r.
This is done for every data frames we have done in this file, the main purpose 
is the the running time issue caused by large datasets, and this will reduce
the loading time significantly."
norway_standard <- standardiseGender(data_norway, c("Menn", "Kvinner", "Begge kjønn"))
save(norway_standard, file = "../results/data_norway.Rda")  # Save to .rda file



# Sweden
sweden_standard <- standardiseGender(data_sweden, c("M", "K"))
save(sweden_standard, file = "../results/data_sweden.Rda")
rm(data_sweden_2015_2019) 
rm(data_sweden_2020) 


# Denmark
denmark_standard <- standardiseGender(data_denmark, c("M", "W"))
save(denmark_standard, file = "../results/data_denmark.Rda")


#UK
uk_standard <- standardiseGender(data_denmark, c("M", "W"))
save(uk_standard, file = "../results/data_uk.Rda")
rm(data_uk_2014)
rm(data_uk_2015)
rm(data_uk_2016)
rm(data_uk_2017)
rm(data_uk_2018)
rm(data_uk_2019)
rm(data_uk_2020)
