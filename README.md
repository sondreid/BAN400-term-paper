# BAN400-term-paper: COVID-19 dashboard
For this assignment we have developed a dashboard showing relevant statistics, tables, interactive plots as well as predictive and forecasting use of machine learning.
The dashboard is live, and is hosted use the free service provided by shinyapps. All supporting data wrangling and analysis scripts are containing in the scripts folder. 
The dashboard itself is found in script/shiny. To provide efficient communication with the shiny app, the models, plots and relevant data is stored in several Rda files located in the data folder of the shiny app.

A short summary of each R script is found below:

Data retrieval:
The manual data wrangling is conducted in this script. The goal of this script is to handle as much of the peculiarities of each data source and pass it to the standardisation script.

Standardisation:
The standardisation script loads a single data standard csv file, as well as a unique format csv file for each data source. Standardisation compares the format found in the country format file, and
the standard format file, and performs necessary operations to standardise each country's dataframe.
These dataframes are then passed to analysis

Analysis:
The analysis script compiles all country data into a joint dataframe 'totaldata'. The script provides the basis use of the data compiled, like signficance tests, tables and tests.
Finally the script saves our joint dataframe to be used in ML, server and ui scripts. 

ML:
The ML script features the training and evaluation of machine learning models used for prediction and forecasting in the Shiny dashboard.

Server:
The server script handles server-side logic and calls on relevant functions found in the stored Rda files located in the data folder

UI:
The ui script handles the aesthetics of the shiny dashboards. The dashboard layout is a tab-based interface.




Github:
https://github.com/sondreid/BAN400-term-paper

Live shiny app:
https://olavik17.shinyapps.io/shiny/


