###################### Data standardisation script ##########################

"Libraries "
library(readxl)
library(readr)
library(tidyverse)
library(magrittr)
library(types)
library(lubridate)
library(janitor)  
library(docstring)

"In case of sourcing the retrieval file directly to this file"
source("data_retrieval.r")


getStandard <- function() {
  #' Retrieve data standard excel sheet 
  #' @returns standard dataframe
  return(read_csv("../datasett/Datastandard/data_standard.csv")) }


getFormat <- function(country) {
  #' Returns the format file of a given country
  #' @param country : 
  #' @returns new dataframe
  path <- paste("../datasett/", country, "/") %>% gsub(" ", "",.)
  formatFiles <- list.files(path = path, 
                            pattern = "format", 
                            full.names = TRUE) 
  ListofDF <- lapply(formatFiles, read_csv)
  df <- bind_rows(ListofDF)
  formatCorrectness(df) # Check for correctness
  return (df)
}


formatCorrectness <- function(df) {
  #' Checks correctness of format dataframe (imported as csv file)
  #' @returns Error messsage if error found 
  colnames <- colnames(df)
  if (!("Gender" %in% colnames) | !("Agegroups" %in% colnames) | !("AgeStandardGroup" %in% colnames)) {
    stop("Format error: Missing one or more required columns")
  }
  if (length(which(!is.na(df$Agegroups))) != length(which(!is.na(df$AgeStandardGroup)))) {
    stop("Format error: Missing values in agegroup or agegroupstandard")
  }
  
}


#### Gender -----------------------------------------


standardiseGender <- function(df, country) {
  #' In column gender, the function loops through all rows.
  #' If it finds a match between the genderVector and standardGender it will return the standardGender
  #' NA otherwise
  #' @param df input dataframe
  #' @param genderVec vector of original gender formatting in df
  #' @returns a gender-standardized dataframe
  standard <- getStandard() %>% filter(!is.na(Gender))
  format <- getFormat(country) %>% filter(!is.na(Gender))
  for (k in 1:nrow(df)) {
    changedGender <- "No"
    for (i in 1:length(format$Gender)) {
      if (df$gender[k] == format$Gender[i]) {
        df$gender[k] <- standard$Gender[i]
        changedGender <- "Yes"
      }
    }
    if (changedGender == "No") df$gender[k] <- NA
  }
  df <- df %>% 
    drop_na()
  return (df)
}


#### Age -------------------------------------------

getAge <- function(agestring, index) {
  #' Function to convert a string og
  #' @param agestring : string containing ages
  #' @param index : index of the string to be returned e.g 2 -> returns "84" if agestring "80-84"
  agestring <- (agestring %>% 
                  strsplit(., "-"))[[1]] %>% 
    gsub("\\+", "", .)
  if (length(agestring) == 1) return (agestring) 
  return (agestring[index])
}



ageToGroup <- function(df) {
  #' Function to be used if the input dataframe does not have predetermined agegroups, but rather age as numeric valus.
  #' Categorizes the dataframe ages as per defined in the standard format file 
  #' @param df : input dataframe
  standard <- getStandard() %>% filter(!is.na(Agegroups))
  for (k in 1:nrow(df)) {
    for (i in 1:length(standard$Agegroups)) {
      upperlimit <- getAge(standard$Agegroups[i], 2)
      if (df$agegroup[k] <= upperlimit) {
        df$agegroup[k] <- standard$Agegroups[i]
        break
      }
      else if(i == length(standard$Agegroups)) df$agegroup[k] <- standard$Agegroups[i]
      
    }
  }
  return (df)
}


standardiseAge <- function(df, country) {
  #' Returns an age-standardised dataframe
  #' We will end up with at the most the same number of unique agegroups, but if we end up with fewer,
  #' a group_by -summarise operation is necessary to preserve the number of rows.
  #'@param df : input dataframe
  #'@param country: name of country to be standardised, string
  #'@returns age-standardized dataframe
  standard <- getStandard()
  format <- getFormat(country)
  if (format$Agegroups[1] == "Age") df <- ageToGroup(df) 
  else {
    for (k in 1:nrow(df)) {
      for (i in 1:length(format$Agegroups)) {
        if (df$agegroup[k] == format$Agegroups[i]) {
          standardIndex <-  format$AgeStandardGroup[i]
          df$agegroup[k] <- standard$Agegroups[standardIndex]
        }
      }
      
    }
  }
  
  df <-  df %>%
    group_by(gender, agegroup,year,week) %>% #Preserve number of rows
    summarise(deaths = sum(deaths),
              country = country) %>% unique()
  return (df)
}


############################################################################

standardiseCountry <- function(df, country) {
  #' Standardizes a dataframe with regard to age and gender
  #' @param df input dataframe
  #' @param country name of country, string
  df <- df %>% 
    standardiseGender(., country) %>%
    standardiseAge(., country) %>%
    drop_na() %>%
    transform(gender = as.factor(gender),  
              agegroup = as.factor(agegroup),
              week = as.numeric(week),
              year = as.character(year),
              deaths = as.numeric(deaths),
              country = as.factor(country)) 
  return (df)
}





# Data storage ------------------------------------------------------------------------

data_norway  %<>% standardiseCountry(., "Norway")
data_sweden  %<>% standardiseCountry(., "Sweden")
data_denmark %<>% standardiseCountry(., "Denmark")
data_uk      %<>% standardiseCountry(., "UK")
data_france  %<>% standardiseCountry(., "France")

"Saving the data frame to a .Rda-file for the purpose of analysis.r.
This is done for every data frames we have done in this file, the main purpose 
is the the running time issue caused by large datasets, and this will reduce
the loading time significantly."

save(data_norway,data_sweden,data_denmark,data_uk,data_france, file = "../datasett/processed_data_all_countries.Rda")  

