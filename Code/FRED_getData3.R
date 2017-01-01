
# ---- Load Dependencies
# Load Dependencies -------------------------------------------------------

# Packages
library(plyr)
library(lubridate)
library(tidyverse)

# Data
load("~/R/TexasPrimary2016/Data/FRED/fred.master.RData")
load("~/R/TexasPrimary2016/Data/FRED/fred.cat.list.RData")
load("~/R/TexasPrimary2016/Data/FRED/fred.series.RData")


# ---- Misc
# Misc --------------------------------------------------------------------

# UR_2015 <- fred.cat.list$`Unemployment Rate` %>%
#   ldply(function(x) filter(x, lubridate::year(Date) == 2015) %>%
#                 summarise(UR_2015 = mean(Value))) %>%
#   as_tibble()


# ---- Helpers
# Helpers -----------------------------------------------------------------

# Data frames need to be filtered and aggregated w/ mean()
# and then joined with a column (CountyName) that can be 
# used later to merge all data frames together.
assemble_list <- function(n){
  fred.cat.list %>%  # this is a list
  lapply(function(x) {
    x %>%  # this is another list (Category)
    ldply(function(y) {
      y %>%  # this is a data frame (an individual county w/in the Category)
        filter(year(Date) == n) %>%
        summarise(Value = mean(Value))
      }) %>%
      as_tibble()
    }) %>%
  lapply(function(x) {
    x %>%
      left_join(y  = select(fred.series, SeriesID, CountyName),
                by = c(".id" = "SeriesID"))
    })
  }

# Data frames need to be merged (assembled) into a single
# data frame. This process also assembles the list.
assemble_tbl <- . %>%
  assemble_list() %>%
  Reduce(function(...) left_join(..., by = "CountyName"), .) %>%
  select(-starts_with(".id"))

# ---- FRED_2012
# FRED_2012 ---------------------------------------------------------------

# All data sets for 2012 are complete
FRED_2012 <- 2012 %>% assemble_tbl()
new.names <- c("CLF", "UnRate", "ResPop", "PCPI", "PI", "BachDegree",
               "HSDegree", "LowerCI_EMHI", "HigherCI_EMHI", "EMHI")
names(FRED_2012)[-2] <- new.names
FRED_2012 <- FRED_2012 %>% select(CountyName, CLF, UnRate:EMHI)


# ---- FRED_2013
# FRED_2013 ---------------------------------------------------------------

# Some data sets missing for 2013
FRED_2013 <- 2013 %>% assemble_tbl()
new.names <- c("CLF", "UnRate", "ResPop", "PCPI", "PI", "BachDegree",
               "HSDegree", "LowerCI_EMHI", "HigherCI_EMHI", "EMHI")
names(FRED_2013)[-2] <- new.names
FRED_2013 <- FRED_2013 %>% select(CountyName, CLF, UnRate:EMHI)


# ---- FRED_2014
# FRED_2014 ---------------------------------------------------------------

# Some data sets missing for 2014
FRED_2014 <- 2014 %>% assemble_tbl()
new.names <- c("CLF", "UnRate", "ResPop", "PCPI", "PI", "BachDegree",
               "HSDegree", "LowerCI_EMHI", "HigherCI_EMHI", "EMHI")
names(FRED_2014)[-2] <- new.names
FRED_2014 <- FRED_2014 %>% select(CountyName, CLF, UnRate:EMHI)

# ---- FRED_2015
# FRED_2015 ---------------------------------------------------------------

# Some data sets missing for 2015
FRED_2015 <- 2015 %>% assemble_tbl()
new.names <- c("CLF", "UnRate", "ResPop", "PCPI", "PI", "BachDegree",
               "HSDegree", "LowerCI_EMHI", "HigherCI_EMHI", "EMHI")
names(FRED_2015)[-2] <- new.names
FRED_2015 <- FRED_2015 %>% select(CountyName, CLF, UnRate:EMHI)

# ---- Save
# Save --------------------------------------------------------------------

# Save FRED_2014.
save(FRED_2014, file = "Data/FRED/FRED_2014.RData")

# Save FRED_2012.
save(FRED_2012, file = "Data/FRED/FRED_2012.RData")
