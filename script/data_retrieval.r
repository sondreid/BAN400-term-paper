###################### Data retrieval/cleaning script ##########################
"Support for utf-8 encoding"
options(encoding="utf-8")


"Libraries for retrieval and cleaning"
library(readxl)
library(readr)
library(tidyverse)
library(types)
library(lubridate)
library(janitor)  

#### Download last updated version of data -------------------------------------
download.file(url = "https://www.scb.se/en/finding-statistics/statistics-by-subject-area/population/population-composition/population-statistics/pong/tables-and-graphs/preliminary-statistics-on-deaths/",
              destfile = "../datasett/Sweden/sweden_scb.xlsx")

download.file(url = "https://www.ssb.no/statbank/sq/10044673",
              destfile = "../datasett/Norway/norway_ssb.xlsx")

download.file(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek48202007122020155900.xlsx",
              destfile = "../datasett/UK/UK_2020.xlsx")

#download.file(url = "", destfile = "../datasett/France/france_2020.csv")

#download.file(url = "", destfile = "../datasett/Denmark/danmark_statbank_2013-2020.xlsx")



#### Functions -----------------------------------------------------------------
"Records group-element for a given column and fills empty elements with the 
stored element for each new group. The function has following parameters:
df (data frame), column index with datatype numeric and groupnameInterval, data-
type numeric, such that function know the frequency in which the groupname
occurs. The if statement evaluates the occurance of the groupname by using
modulo-operator such that the remainder is equal zero and is not an empty
value NA, then set the new groupname equals name variable. Else, fill the rest 
of the given column with stored groupname. Returns the data frame (df)."
changeNonRecurringRow <- function(df, 
                                  column =? numeric, 
                                  groupnameInterval =? numeric) {
  name <- ""
  for (i in 1:nrow(df)) {
    if ((((i-1) %% groupnameInterval) == 0) && !is.na(df[i, column]) ){ 
      name <- df[i, column]
    } else {
      df[i, column] <- name 
    }
  }
  return (df)
}


"Function spesific to the UK dataset (2014-2019) that cleans the data.
The function itself transposes the dataset into two separated data frames
of male and female, and rowbinds them at the end. 
* pivot_longer() is a function that decreases selected columns and increases
number of rows."

clean_data <- function(df, 
                       year =? chr, 
                       selectColMale, 
                       selectColFemale){
  
  data_male <- as.data.frame(t(df))       # t() transposes the data frame
  data_female <- as.data.frame(t(df))
  
  data_male %<>%
    row_to_names(row_number = 1) %>%      # First row to column name (janitor)
    select(selectColMale) %>% 
    mutate("Males 5" = replace_na("M")) %>% 
    rename(gender = "Males 5",
           week = "Week number",
           "1-" = "Under 1 year") %>%     # Simplifying it for getAge-function
    pivot_longer(cols = !c(week, gender), # *
                 names_to = "agegroup",
                 values_to = "deaths")
  
  data_female %<>%
    row_to_names(row_number = 1) %>%
    select(selectColFemale) %>% 
    mutate("Females 5" = replace_na("W")) %>% 
    rename(gender = "Females 5",
           week = "Week number",
           "1-" = "Under 1 year") %>% 
    pivot_longer(cols = !c(week, gender),   # *
                 names_to = "agegroup",
                 values_to = "deaths")
  
  data <- rbind(data_male, data_female) %>% # rowbinds given data frames
    mutate(year = rep(year),
           country = "United Kingdom") %>% 
    transform(., 
              week = as.numeric(week),
              deaths = as.integer(deaths))
  
  return(data)
}



"Function spesific to UK dataset 2020 that cleans the data. Is almost an 
exact copy of the functon above, except from some specific arguments caused 
of different formatting in the year 2020 for the given dataset. Alternativly,
the function above could be modified in order to fit these formatting issues, 
but that would increase the number of paramters. Else, there is no new functions
introduced in this section."
custom_clean_data_2020 <- function(df, 
                                   year =? chr, 
                                   selectColMale, 
                                   selectColFemale){
  
  data_male <- as.data.frame(t(df))
  data_female <- as.data.frame(t(df))
  
  data_male %<>%
    row_to_names(row_number = 1) %>%
    select(selectColMale) %>% 
    mutate("Males 6" = replace_na("M")) %>% 
    rename(gender = "Males 6",
           week = "Week number",
           "1-" = "<1") %>% 
    pivot_longer(cols = !c(week, gender),
                 names_to = "agegroup",
                 values_to = "deaths")
  
  data_female %<>%
    row_to_names(row_number = 1) %>%
    select(selectColFemale) %>% 
    mutate("Females 6" = replace_na("W")) %>% 
    rename(gender = "Females 6",
           week = "Week number",
           "1-" = "<1") %>% 
    pivot_longer(cols = !c(week, gender),
                 names_to = "agegroup",
                 values_to = "deaths")
  
  
  data <- rbind(data_male, data_female) %>% 
    mutate(year = rep(year), 
           country = "United Kingdom") %>% 
    transform(., 
              week = as.numeric(week),
              deaths = as.integer(deaths))
  
  return(data)
}



#### Data Norway ---------------------------------------------------------------
"Reading excel file with given range of rows and columns"
data_norway <- read_excel("../datasett/Norway/norway_ssb.xlsx", 
                          range = "Dode1!a4:AA1594")


"Changing the empty columns by replicating gender names (1) and agegroups (2)"
data_norway  <- changeNonRecurringRow(data_norway, 1, 53) %>% # For gender
             changeNonRecurringRow(., 2, 53) # For agegroups


"Manipulating the norway's data frame.
** sapply() iterates through a give vector (column) and performs strsplit for 
each element, the specified integer argument in strsplit() decide which element
in the list to retrieve." 
data_norway  %<>%
  pivot_longer(cols = starts_with("20"),   # Check description for clean_data()
               names_to = "year",
               values_to = "deaths") %>% 
  rename(gender = "...1",
         agegroup =  "...2", 
         values = "...3") %>% 
  mutate(week =   sapply(strsplit(values, " "), `[`, 2)) %>%     
  filter(!is.na(gender), 
         year >= 2014, 
         deaths != 0) %>%
  group_by(gender, 
           agegroup, 
           year, 
           week) %>%                  # In order to sum deaths into groups
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%                       # Ungrouped such that mutation is allowed  
  mutate(country = "Norway") 



#### Data Sweden ---------------------------------------------------------------
"Reading excel file with given range of rows and columns, and selected sheet"
 data_sweden <- 
   read_excel("../datasett/Sweden/sweden_scb.xlsx", 
              sheet = "Tabell 7", 
              range = "Tabell 7!a9:s62")


"Creates a new data frame containing data from 2015-2019.
Comment: Check genderFunc_vector description in function section, and
sapply() description in description for data_norway (**)"
data_sweden_2015_2019 <- 
  data_sweden %>% 
  select(1:10, -"2015-2019") %>%    # Deselecting with '-' operator
  pivot_longer(
    cols = !"Vecka",
    names_to = "values",
    values_to = "deaths") %>%
  mutate(year = "2015-2019",
         gender = sapply(strsplit(values ," "), `[`, 1), 
         agegroup = sapply(strsplit(values ," "), `[`, 2)) %>%
  select(-"values") %>%
  rename(week = "Vecka")


"Creates a new data frame containing data from 2020"
data_sweden_2020 <- data_sweden %>% 
  select("Vecka", 12:19 ) %>%
  pivot_longer(
    cols = !"Vecka",
    names_to = "values",
    values_to = "deaths") %>%
  mutate(year = "2020",
         gender = sapply(strsplit(values ," "), `[`, 1),
         agegroup = sapply(strsplit(values ," "), `[`, 2)) %>% 
  select(-"values") %>% 
  rename(week = "Vecka") %>% 
  filter(deaths != 0.0) # In order to remove zero-deaths-week
     

"Combines data frames to a single data frame, mutates agegroup column to contain
the standardized agegrouping, selects useful columns and coerce these to 
prefered datatypes"
data_sweden <- rbind(data_sweden_2015_2019, 
                     data_sweden_2020) %>% 
  mutate(country = "Sweden") %>%
  select(gender, 
         agegroup, 
         year, 
         week, 
         deaths, 
         country) 

rm(data_sweden_2015_2019)
rm(data_sweden_2020)

#### Data Denmark --------------------------------------------------------------
"Reads csv-file with read_delim() and separete it by ';', alternative:
read_csv2."
data_denmark <- read_delim("../datasett/Denmark/danmark_statbank_2013-2020.csv",
                           delim = ";",
                           skip = 2)



"Manipulates the data frame by selecting useful columns with [], 
and mutatating gender column in to standardized gender name."
data_denmark <- 
  data_denmark[-1,] %>%
  rename(gender = " _1",
         agegroup = " _2") %>%
  mutate(gender = substr(gender, 1, 1)) %>%
  changeNonRecurringRow(., 2, 22) # Fills non recurring rows with given genders 



"Further manipulation of data_denmark data frame. Deselecting irrelevant
columns, transpose all columns that starts with '20' in order to create
two column names to values column and values to the deaths column. Mutating 
values column in to two columns named, year and week, by executing strsplitt()
for each rows in the values vector with sapply(). Grouping before summing 
deaths for given grouping, then coerce given columns in data frame to 
prefered datatype."
data_denmark <- 
  data_denmark[-c(1,2,24,25),] %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "values",
               values_to = "deaths") %>% 
  mutate(year = sapply(strsplit(values, "U"),'[', 1),
         week = sapply(strsplit(values, "U"), '[', 2),
         country = "Denmark") %>% 
  filter(!is.na(agegroup), 
         year >= 2014) %>%
  group_by(gender, 
           agegroup,
           year, 
           week) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%                       # Ungroup such that mutation is allowed
  mutate(country = "Denmark")



#### Data UK ----------------------------------------------------------------
"UK datasets differ in the way they categorize age groups in the periods 
2014-2019 and 2020. As a result of that getAgeVector transforms the agegroup 
and returns the lower bound of the agegroup as a numeric value. The ageuniformiy 
function then places the age in a fitting agegroup."

data_uk_2014 <- read_xls("../datasett/UK/UK_2014.xls", 
                         sheet = "Weekly Figures 2014",
                         range = "A2:BA40") %>%
  clean_data(.,                    # Function call to clean_data
             2014,
             selectColMale = c(1, 21, 23:29), 
             selectColFemale =  c(1, 30, 32:38))


data_uk_2015 <- read_xls("../datasett/UK/UK_2015.xls", 
                         sheet = "Weekly Figures 2015",
                         range = "A3:BB40") %>%
  clean_data(.,
             2015,
             selectColMale = c(1, 20, 22:28), 
             selectColFemale =  c(1, 29, 31:37))


data_uk_2016 <- read_xls("../datasett/UK/UK_2016.xls", 
                         sheet = "Weekly figures 2016",
                         range = "A3:BB40") %>% 
  unite(., x, 1:2, na.rm = TRUE) %>%
  clean_data(., 
             2016,
             selectColMale = c(1, 20, 22:28), 
             selectColFemale =  c(1, 29, 31:37))


data_uk_2017 <- read_xls("../datasett/UK/UK_2017.xls", 
                         sheet = "Weekly figures 2017",
                         range = "A3:BB40") %>% 
  unite(., x, 1:2, na.rm = TRUE) %>%
  clean_data(.,
             2017,
             selectColMale = c(1, 20, 22:28), 
             selectColFemale =  c(1, 29, 31:37))

data_uk_2018 <- read_xls("../datasett/UK/UK_2018.xls", 
                         sheet = "Weekly figures 2018",
                         range = "A3:BB40") %>% 
  unite(., x, 1:2, na.rm = TRUE) %>%
  clean_data(., 
             2018,
             selectColMale = c(1, 20, 22:28), 
             selectColFemale =  c(1, 29, 31:37))

data_uk_2019 <- read_xls("../datasett/UK/UK_2019.xls", 
                         sheet = "Weekly figures 2019",
                         range = "A3:BB40") %>% 
  unite(., x, 1:2, na.rm = TRUE) %>%
  clean_data(.,
             2019,
             selectColMale = c(1, 20, 22:28), 
             selectColFemale =  c(1, 29, 31:37))

data_uk_2020 <- read_xlsx("../datasett/UK/UK_2020.xlsx", 
                          sheet = "Weekly figures 2020",
                          range = "A4:BC85") %>% 
  unite(., x, 1:2, na.rm = TRUE) %>%
  custom_clean_data_2020(., 
                         2020,
                         selectColMale = c(1, 38, 40:59), 
                         selectColFemale =  c(1, 60, 62:81))



# Binding all data frames created except from 2020
data_uk <- do.call("rbind", list(data_uk_2014, 
                                 data_uk_2015, 
                                 data_uk_2016, 
                                 data_uk_2017, 
                                 data_uk_2018, 
                                 data_uk_2019, 
                                 data_uk_2020)) %>% 
                      select(gender, 
                             agegroup, 
                             year, 
                             week , 
                             deaths, 
                             country) %>% 
                      group_by(gender, 
                               agegroup,
                               year, 
                               week) %>%
                      summarise(deaths = sum(deaths)) %>%
                      ungroup() %>%
                      mutate(country = "UK")
rm(data_uk_2014)
rm(data_uk_2015)
rm(data_uk_2016)
rm(data_uk_2017)
rm(data_uk_2018)
rm(data_uk_2019)
rm(data_uk_2020)

##### Data France -----------------------------------------------------------
"Gather csv filename from the France-folder in to a list"
datafiles_france <- list.files(path = "../datasett/France/", 
                    pattern = "*.csv", 
                    full.names = TRUE) 



"Sapply() to read each file with read_csv2 with sep = ';',
then bind rows by id. The rest of the code does the cleaning"    
data_france <- sapply(datafiles_france, read_csv2, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  rename(day_dead = "JDEC",
         month_dead = "MDEC",
         year = "ADEC",
         day_born = "JNAIS",
         month_born = "MNAIS",
         year_born = "ANAIS",
         gender = "SEXE") %>% 
  transform(day_born = as.numeric(day_born),
            month_born = as.numeric(month_born))  %>% # Replace missing values with NA
  mutate(day_born = replace_na(day_born, 1), # Replace NA with 1
         month_born = replace_na(month_born, 1),
         date_born = ymd(paste(year_born, month_born, day_born)), ### Convert year month to single date of birth
         date_dead = ymd(paste(year, month_dead, day_dead)),      ### Convert year month to single date of death
         week = week(ymd(paste(year, month_dead, day_dead))),     ## Week of death
         agegroup = as.period(interval(date_born, date_dead), unit = "year")$year) %>% ## Age at death
         #agegroup = ageGroup_vector(age)) %>%
  filter(!is.na(agegroup)) %>% 
  group_by(gender,
           agegroup,
           year,
           week) %>% 
  summarise(deaths = n()) %>%
  ungroup() %>%
  mutate(country = "France")



#### Remove functions that are no longer needed in memory ----------------------

rm(changeNonRecurringRow)
rm(clean_data)
rm(custom_clean_data_2020)
rm(datafiles_france)

