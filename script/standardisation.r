###################### Data standardisation script ##########################

"Libraries "
library(readxl)
library(readr)
library(tidyverse)
library(types)
library(lubridate)
library(janitor)  



standardiseCountry <- function(country) {
  
}

getFormat <- function(country) {
  country <- tolower(country)
  path <- paste("../datasett/", country, "/", country, "_format.xlsx")
  path <- path %>% gsub(" ", "",.)
  return (path)
}

getStandard <- function() {return("../datasett/Datastandard/data_standard.xlsx") }


"Flyttes trolig"
"Change to allow for multiple datasets, (such as the case with France"
getCountry <- function(country) {
  country <- tolower(country)
  path <- paste("../datasett/", country, "/", country, "_data.xlsx")
  path <- path %>% gsub(" ", "",.)
  return (path)
}

"Ikke ferdig"
genderFunc <- function(country) {
  
  countryFormat <- read_excel(getFormat(country))
  standardMale <- read_excel(getStandard()) %>% select(Gender)
  country <- getCountry(country)
  genderCol <- colnames(country)[countryFormat[[2]][1]]
  country %>% 
    mutate(genderCol = lapply(standardiseGender()))
  print(standard)
}

standardiseGender <- function(gender, standardGender) {
  
  
}