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


"In case of sourcing the retrieval file directly to this file"
source("standardisation.r")



"Loading data frames retrieved from standardisation.r"

load("../datasett/processed_data_all_countries.Rda")


#### Functions -----------------------------------------------------------------
"This is a function that takes a given data frame, single agegrouping, gender,
which by default is Male 'M' and Female 'F' and also given countries. The
mutation of min_reg and max_reg is done in order to fill the area inbetween 
the actual and expected regression lines."
plotfunction_diff <- function(df, 
                              agegrouping =? character, 
                              gendervector = c("M", "F"), 
                              countryparam) {
  
  df <- df %>% 
    filter(agegroup == agegrouping, 
           gender %in% gendervector,
           country == countryparam)
  
  regline_fitted <- loess(df$deaths ~ df$week)
  regline_expected <- loess(df$expected_deaths ~ df$week)
  
  plot <- df %>% 
    mutate(min_reg = pmin(regline_fitted$fitted, 
                          regline_expected$fitted),
           max_reg = pmax(regline_fitted$fitted, 
                          regline_expected$fitted)) %>%
    ggplot() +
    geom_smooth(aes(x = week, 
                    y = deaths,
                    color = agegroup),
                se = FALSE) +             # Confidence interval
    geom_smooth(aes(x = week, 
                    y = expected_deaths, 
                    color = "Expected Deaths")) +
    geom_ribbon(aes(week, 
                    ymin = min_reg, 
                    ymax = max_reg),
                fill = "red",
                alpha = 0.5) +
    scale_color_viridis(discrete = TRUE) +
    ylab("Deaths") +
    xlab ("Weeks") +
    ggtitle(paste("Expected vs. Actual Deaths in 2020", 
                  "[", countryparam, "]", 
                  "[Agegroup: ", agegrouping, "]")) +
    theme_ipsum(base_family = "Helvetica", 
                plot_title_size = 12, 
                base_size = 14,
                axis_title_size = 12)
  
  return (ggplotly(plot)) # ggplotly for interactivity
}




"Function that finds the number of weeks we have data accounted for in 2020"
weeksin2020 <- function(df) {
  return (nrow(df %>% 
                 filter(year == 2020) %>% 
                 select(week) %>% 
                 unique()))
}

minimalweeks <- 52 # Store the lowest common week for which we have data in the current year (2020)

"Function that aggregates deaths (as mean of years prior to 2020) as expected 
data, and adds a column for excess deaths to the existing dataframe"
addExpectedDeaths <- function(df) {
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


"Rowbind all datasets to a combined dataset, calling addExpectedDeaths()"
assembleAllData <- function(dfVector = c("data_norway", 
                                         "data_sweden", 
                                         "data_denmark", 
                                         "data_france", 
                                         "data_uk")) {
  totaldata <- addExpectedDeaths(eval(as.name(dfVector[1])))
  for (i in 2:length(dfVector)) {
    totaldata <- rbind(totaldata, 
                       addExpectedDeaths(eval(as.name(dfVector[i]))))
  }
  totaldata %<>% filter(week <= minimalweeks)
  return (totaldata)
}



"Function that takes one data frame, set of agegroups, given genders by 
default, one country, selfconstructed .pdf path to store the plot and 
prefered y-axis interval. Much of the same description as for the 
plotfunction_diff except from the fill-part. At the end it returns an
interactive plot which is facet wraped given by agegroup."
plotfunction <- function(df, 
                         agevector = c("0-64", "65-79", "80-84", "85+"), 
                         gendervector = c("M", "F"), 
                         countryparam,
                         filename,
                         ymin = NA,
                         ymax = NA) {
  
  plot <- df %>% 
    filter(agegroup %in% agevector, 
           gender %in% gendervector,
           country %in% countryparam) %>%
    ggplot() +
    geom_smooth(aes(x = week, 
                    y = deaths,
                    color = agegroup),
                se = FALSE) +
    geom_smooth(aes(x = week, 
                    y = expected_deaths, 
                    color = "Expected Deaths"),
                fill = "lightblue") +
    scale_color_viridis(discrete = TRUE) +
    ylab("Deaths") +
    xlab ("Weeks") +
    ggtitle(paste("Expected vs. Actual Deaths in 2020", 
                  "[", countryparam, "]")) +
    theme_ipsum(base_family = "Helvetica", 
                plot_title_size = 12, 
                base_size = 14,
                axis_title_size = 12)

  
  
  if (!is.na(ymin) | !is.na(ymax)){
    plot <- plot + ylim(ymin, ymax)
  }
  
  plot <- plot + 
    facet_wrap(~agegroup, nrow = 1) # Wraps number of agegroups on a single row 
  
  ggsave(paste("../results/", filename, sep = ""))
  
  return (ggplotly(plot))           # ggplotly for interactivity
}



"Plots all the agegroups in a country in single plots"
plotter <- function(df, gender = c("M", "F"), country){
  for (agegroup in levels(df$agegroup)){
    print(plotfunction_diff(df, 
                            agevector = agegroup, 
                            gendervector = gender,  
                            country = country))
  }
}



"Stores created plots in pdf-file"
plot_pdf <- function(filename, plot){
  pdf(paste("../results/",filename, sep=""), onefile = TRUE)
  plotter(plot)
  dev.off()
}



#### Total Data ----------------------------------------------------------------
"Totaldata is a dataframe which contains all the created data frames to
a consolidated data frame"
totaldata <- assembleAllData() 


##### Test statistics-----------------------------------------------------------
"Is the difference in deaths and expected deaths statistically significant?"

"Function that conducts a two sided welch t.test to test whether 2020 
deaths are statistically different than expected deaths. Compares x_mean and 
y_mean to find out whether deaths are higher or lower. Tags the p.value along 
with a string indicating whether it is above or below the estimated deaths, and 
returns them as a list"

t_testFunc <- function(df, 
                       agegroupvector = c("0-64", "65-79", "80-84", "85+"), 
                       countryparam =? character ) {
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



"Function that returns a string 
Calls t_testFunc on a given country, uses totaldata as df as opposed to tabledata.
Returns a string based on the result from t_test_func."
tableTests <- function(country) {
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

" Returns the first element of a list generated by tableTests func"
getTableTestFirst <- function(country) {
  return (tableTests(country)[[1]])
}
getTableTestFirst <- Vectorize(getTableTestFirst)


" Returns the second element of a list generated by tableTests func"
getTableTestPvalue <- function(country) {
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



              
              
"Function that returns shortTableData using the formattable package"
shortTable <- function() {
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



"Function that returns a sortable table which contains weekly deaths, expected 
deaths and excess deaths according to agegroup and gender"
longTable <- function() {
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


save(totaldata, file = "Shiny/data/totaldata.Rda")

