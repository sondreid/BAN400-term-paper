###################### Data standardisation script ##########################

"Libraries "
library(readxl)
library(readr)
library(tidyverse)
library(types)
library(lubridate)
library(janitor)  
library(docstring)


"In case of sourcing the retrieval file directly to this file"
#source("data_retrieval.r")


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
  if (length(which(!is.na(testFormat$Agegroups))) != length(which(!is.na(testFormat$AgeStandardGroup)))) {
    stop("Format error: Missing values in agegroup or agegroupstandard")
  }
  
}


#### Gender -----------------------------


standardiseRows <- function(df, genderVec) {
  #' In column gender, the function loops through all rows.
  #' If it finds a match between the genderVector and standardGender it will return the standardGender
  #' NA otherwise
  #' @param df input dataframe
  #' @param genderVec vector of original gender formatting in df
  standardGender <- getStandard() %>% select(Gender)
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
              year = as.character(year),
              deaths = as.numeric(deaths),
              country = as.factor(country)) 
  return (df)
}


#### Gender -----------------------------



standardiseAge <- function(df, country) {
  standard <- getStandard()
  for (k in 1:nrow(df)) {
    for (i in 1:length(format$Agegroups)) {
      if (df$agegroup[k] == format$Agegroups[i]) {
        index <-  format$AgeStandardGroup[i]
        print(paste("Index", index))
        df$agegroup[k] <- standard$Agegroups[index]
      }
    }
    
  }
  return (df)
}
# test
norway_standard <- standardiseAge(data_norway, "Norway")


# Norway
"Saving the data frame to a .Rda-file for the purpose of analysis.r.
This is done for every data frames we have done in this file, the main purpose 
is the the running time issue caused by large datasets, and this will reduce
the loading time significantly."
data_norway <- standardiseGender(data_norway, c("Menn", "Kvinner", "Begge kjønn"))
save(data_norway, file = "../results/data_norway.Rda")  # Save to .rda file


# Sweden
data_sweden <- standardiseGender(data_sweden, c("M", "K"))
save(data_sweden, file = "../results/data_sweden.Rda")
rm(data_sweden_2015_2019) 
rm(data_sweden_2020) 

# Denmark
data_denmark <- standardiseGender(data_denmark, c("M", "W"))
save(data_denmark, file = "../results/data_denmark.Rda")

#UK
data_uk <- standardiseGender(data_uk, c("M", "W"))
save(data_uk, file = "../results/data_uk.Rda")
rm(data_uk_2014)
rm(data_uk_2015)
rm(data_uk_2016)
rm(data_uk_2017)
rm(data_uk_2018)
rm(data_uk_2019)
rm(data_uk_2020)