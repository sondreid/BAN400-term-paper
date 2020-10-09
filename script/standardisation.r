###################### Data standardisation script ##########################

"Libraries "
library(readxl)
library(readr)
library(tidyverse)
library(types)
library(lubridate)
library(janitor)  


getFormat <- function(country) {
  country <- tolower(country)
  path <- paste("../datasett/", country, "/", country, "_format.xlsx")
  path <- path %>% gsub(" ", "",.)
  print(path)
  return (path)
}

getStandard <- function() {return("../datasett/Datastandard/data_standard.xlsx") }

genderFunc <- function(country) {
  countryFormat <- read_excel(getFormat(country)) %>% select(Gender)
  standard <- read_excel(getStandard()) %>% select(Gender)
  print(standard)
}