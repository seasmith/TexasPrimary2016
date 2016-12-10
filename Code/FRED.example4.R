
# Load Dependencies -------------------------------------------------------

library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)


# Data to analyze ---------------------------------------------------------

### All data found in fred.cat.list

### These are the values I will be focusing on:
    ### Unemployment
    ### Civilian Labor Force / Residential Population
    ### Per Capita Personal Income
    ### Estimate of Median Household Income
    ### High School Graduate or Higher (5-year estimate) / Civilian Labor Force
    ### Bachelor's Degree or Higher (5-year estimate) / Civilian Labor Force
    ### Bachelor's Degree or Higher (5-year estimate) / High School Graduate or Higher (5-year estimate)

### Limitations
    ### Last date of record
        ### 2013-01-01: Per Capita Personal Income 
        ### 2014-01-01


# What my functions should do ---------------------------------------------

### Individual time-series
    ### Call 'county'
    ### Specify 'start.date' and 'end.date'
    ### Specify 'aggregation' (i.e. Annual, Rolling Average)
