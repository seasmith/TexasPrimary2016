
# Load Dependencies -------------------------------------------------------

library(plyr, quietly = T)
library(dplyr, quietly = T)
library(rvest, quietly = T)
library(choroplethrMaps, quietly = T)
library(lubridate, quietly = T)
source("cat.functions.R")
load("Data/fred.series")
load("Data/fred.obs")
data("county.regions")


# Load Geo Data -----------------------------------------------------------

tx.regions <- filter(county.regions, state.name == "texas") %>%
              select(region, "CountyName" = county.name)


# Create Master Table -----------------------------------------------------

fred.master <- cat.info(fred.series)


# All in one --------------------------------------------------------------


# These functions will:
# 1. Group data frames by category
# 2. Create a single data frame from and for each group of data frames
# 3. Save each of the data frames to a text file

# This function groups data frames by 'category'
# It returns a list of 10 elements (corresponding to the 10 'category' values)
# Each element has 254 data frames (one for each county)
    fred.cat.list <- lapply(seq_along(fred.master$category), function(x){
        obs.catcher(series.table = fred.series,
                    obs.list     = fred.obs,
                    cat          = fred.master$category[x])
    })
names(fred.cat.list) <- fred.master$category
save(fred.cat.list, file = "Data/fred.cat.list.RData")

# This function merges the data frames in each list element
# The result is a massive data frame in each of the 10 list elements
# The data frames are 255 columns wide
# Columns are 'Date' and the 254 'SeriesID' corresponding to each of the 254 counties
    fred.tables <- lapply(seq_along(fred.cat.list), function(x){
        cat.tabler(fred.cat.list[[x]])
    })
names(fred.tables) <- fred.master$category
# fred.tables   <- fred.tables %>% select(-1) %>% as.character() %>% as.numeric()
save(fred.tables, file = "Data/fred.tables.RData")

# write all 10 data frames to a file
# extension = ".txt"
# separator = "," separator
    lapply(seq_along(fred.tables), function(x){
        write.table(fred.tables[[x]], file = paste("Data/TableFiles/", names(fred.tables[x]), ".txt", sep = ""), sep = ",", row.names = F)
    })