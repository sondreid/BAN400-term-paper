######################### Data Analysis Script #################################
"Support for utf-8 encoding"
options(encoding="utf-8") 


"Libraries for the analysis"
library(kableExtra) 
library(tidyverse)    
library(tidygraph)    
library(hrbrthemes)   
library(viridis)      
library(dygraphs)     
library(xts)          
library(lubridate)    
library(plotly)     
library(types)        
library(pdftools)     
library(gt)           
library(scales)       
library(formattable)  
library(DT)
library(magrittr)

"In case of sourcing the retrieval file directly to this file"
source("standardisation.r")

"Loading data frames retrieved from standardisation.r"
load("../datasett/processed_data_all_countries.Rda")

#### Functions -----------------------------------------------------------------
"Function that finds the number of weeks we have data accounted for in 2020"
weeksin2020 <- function(df) {
  #"Function that finds the number of weeks we have data accounted for in 2020"
  return (nrow(df %>% 
                 filter(year == 2020) %>% 
                 select(week) %>% 
                 unique()))
}

minimalweeks <- 52 # Store the lowest common week for which we have data in the current year (2020)


addExpectedDeaths <- function(df) {
  #'Function that aggregates deaths (as mean of years prior to 2020) as expected 
  #'data, and adds a column for excess deaths to the existing dataframe
  #'@param df: input dataframe
  weeks2020 <- weeksin2020(df)
  if (weeks2020 <  minimalweeks) assign("minimalweeks", weeks2020,envir = .GlobalEnv) # Change global variable minimalweeks
  expected_deaths <- df %>% 
    filter(year < 2020,
           week <= weeks2020) %>%  
    group_by(gender, 
             agegroup, 
             week) %>% 
    summarise(deaths = as.integer(mean(deaths))) 
  
  new_df <- df %>% 
    filter(year == 2020) %>% 
    mutate(expected_deaths = expected_deaths$deaths,
           excess_deaths = deaths - expected_deaths) %>%
    select(gender, 
           agegroup, 
           year, 
           week, 
           country, 
           deaths, 
           expected_deaths, 
           excess_deaths)
  
  return (new_df)
}



assembleAllData <- function(dfVector = c("data_norway", 
                                         "data_sweden", 
                                         "data_denmark", 
                                         "data_france", 
                                         "data_uk")) {
  #' Rowbind all datasets to a combined dataset, calling addExpectedDeaths()
  #' @param dfVector: vector of string names of each country dataframe name
  totaldata <- addExpectedDeaths(eval(as.name(dfVector[1])))
  for (i in 2:length(dfVector)) {
    totaldata <- rbind(totaldata, 
                       addExpectedDeaths(eval(as.name(dfVector[i]))))
  }
  totaldata %<>% filter(week <= minimalweeks)
  return (totaldata)
}

#### Total Data ----------------------------------------------------------------
"Totaldata is a dataframe which contains all the created data frames to
a consolidated data frame"
totaldata <- assembleAllData() 


##### Test statistics-----------------------------------------------------------
"Is the difference in deaths and expected deaths statistically significant?"



t_testFunc <- function(df, 
                       agegroupvector = c("0-64", "65-79", "80-84", "85+"), 
                       countryparam =? character ) {
  #'Function that conducts a two sided welch t.test to test whether 2020 
  #'deaths are statistically different than expected deaths. Compares x_mean and 
  #'y_mean to find out whether deaths are higher or lower. Tags the p.value along 
  #'with a string indicating whether it is above or below the estimated deaths, and 
  #' returns them as a list"
  #' @param df: input dataframe
  #' @param agegroupvector: vector of strings, agegroups to be included
  #' @param countryparam: string, name of country
  testset <- df %>% 
    filter(country == countryparam,
           agegroup %in% agegroupvector,
           week >= 12) %>% 
    group_by(week) %>% 
    summarise(deaths = sum(deaths),
              expected_deaths = sum(expected_deaths)) %>%
    select(deaths, 
           expected_deaths)
  
  
  t_test <- t.test(testset$deaths, 
                   testset$expected_deaths,
                   alternative = "two.sided",
                   var.equal = FALSE)
  
  # Determine whether the mean estimate mean deaths are higher or 
  # lower than the expected deaths
  
  x_mean <- as.integer(t_test$estimate[1])
  y_mean <- as.integer(t_test$estimate[2])
  if (x_mean < y_mean) {return(list("Below", t_test$p.value)) }
  else return(list("Above", t_test$p.value))
}



tableTests <- function(country) {
  #'Function that returns a string 
  #'Calls t_testFunc on a given country, uses totaldata as df as opposed to tabledata.
  #'@param country: string, name of country
  #'Returns a string based on the result from t_test_func."
  test <- t_testFunc(totaldata, 
                     agegroupvector = c("0-64", "65-79", "80-84", "85+"), 
                     countryparam = country)
  
  p.value <- test[[2]]
  outputlist <- list(test[[1]], p.value)
  
  if (p.value > 0.15) outputlist[[1]] <- "Normal"
  else if (test[[1]] == "Above") {
    if      (p.value < 0.025) outputlist[[1]] <- "Above ***"
    else if (p.value < 0.05)  outputlist[[1]] <- "Above **"
    else if (p.value < 0.15)  outputlist[[1]] <- "Above *"
  }
  else if (test[[1]] == "Below")  {
    if      (p.value < 0.025) outputlist[[1]] <- "Below ***"
    else if (p.value < 0.05)  outputlist[[1]] <- "Below **"
    else if (p.value < 0.15)  outputlist[[1]] <- "Below *"
  }
  
  return (outputlist)
}


getTableTestFirst <- function(country) {
  #'Returns the first element of a list generated by tableTests func
  #'@param country: string, name of country
  return (tableTests(country)[[1]])
}
getTableTestFirst <- Vectorize(getTableTestFirst)



getTableTestPvalue <- function(country) {
  #'Returns the second element of a list generated by tableTests func
  #'@param country: string, name of country
  return (round(tableTests(country)[[2]], 4))
}
getTableTestPvalue <- Vectorize(getTableTestPvalue)



## Tables  ---------------------------------------------------------------------
totaldataFilter <- totaldata %>% filter(week >= 12)
shortTable_data <- totaldataFilter  %>%
  group_by(country) %>%
  summarise(excess_deaths = sum(excess_deaths)) %>%
  ## Max excess deaths per week
  cbind(.,
        totaldataFilter %>%
          group_by(country, week) %>%
          summarise(excess_deaths = sum(excess_deaths)) %>% 
          group_by(country) %>%
          filter(excess_deaths == max(excess_deaths)) %>%
          rename(max_deaths_perweek = excess_deaths )) %>% 
  select(-3) %>%
  ## Percentage above normal
  cbind(., 
        totaldataFilter %>% filter(week >= 12) %>%
          group_by(country) %>%
          summarise(percentage_above_normal = percent((
            sum(deaths)/sum(expected_deaths)))) %>%
          select(-country)) %>%
  mutate(statistically_significant = getTableTestFirst(country),
         p.value = getTableTestPvalue(country)) %>%
  rename("Country"                          = country,
         "Total Excess Deaths"              = excess_deaths,
         "Highest Excess Deaths in a Week"  = max_deaths_perweek,
         "Week: Highest Excess deaths"      = week,
         "Percentage of Normal Deaths"      = percentage_above_normal,
         "Statistically Significant"        = statistically_significant,
         "P-value"                          = p.value)






shortTable <- function() {
  #' "Function that returns shortTableData using the formattable package"
  lightred = "#ff7f7f"
  outputTable <- formattable(shortTable_data, list(
    "Total Excess Deaths"              = color_tile("white", lightred),
    "Highest Excess Deaths in a Week"  = color_bar(lightred),
    "Percentage of Normal Deaths"      = color_bar(lightred),
    "Statistically Significant"        = 
      formatter("span", style = x ~ ifelse(substring(x, 1, 5) == "Above", 
                                           style(color = "red", 
                                                 font.weight = "bold"),
                                           NA))))
  return (outputTable)
}




longTable <- function() {
  #'Function that returns a sortable table which contains weekly deaths, expected 
  #'deaths and excess deaths according to agegroup and gender
  outputtable <- datatable(totaldata %>%
                             select(-year), 
                           colnames = c("Gender", 
                                        "Agegroup", 
                                        "Week", 
                                        "Country", 
                                        "Deaths", 
                                        "Expected Deaths", 
                                        "Excess Deaths"),
                           filter = "top",
                           options = list(pageLength = 20, autoWidth = TRUE),
                           class = 'cell-border stripe'
  )
  return (outputtable)
}

save(shortTable, shortTable_data, file = "Shiny/data/tableData.Rda") 
save(totaldata, file = "Shiny/data/totaldata.Rda")

