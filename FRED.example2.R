
# Load Dependencies -------------------------------------------------------


library(ggplot2, quietly = T)
library(dplyr, quietly = T)
library(plyr, quietly = T)
library(rvest, quietly = T)
library(choroplethr, quietly = T)
library(choroplethrMaps, quietly = T)
library(gridExtra, quietly = T)
library(lubridate, quietly = T)
source("cat.functions.R")
load("Data/fred.master")
load("Data/fred.series")
load("Data/fred.obs")
data("county.regions")


# Load Geo Data -----------------------------------------------------------

tx.regions <- filter(county.regions, state.name == "texas") %>%
              select(region, "CountyName" = county.name)


# Estimated Household Income ----------------------------------------------


# emhi.obs <- obs.catcher(fred.series2, fred.obs2, "Estimate of Median Household Income")
# 
# # Find 2014 values (last row)
# loop.list <- list()
# emhi.obs  <- lapply(seq_along(emhi.obs), function(x){
# 
#                  to.get    <- dim(emhi.obs[[x]])[1]
#                  loop.df   <- data.frame(SeriesID = names(emhi.obs[x]),
#                                          EMHI     = emhi.obs[[x]][to.get, 2])
#                  loop.list[[x]] <- loop.df
# }) %>% ldply
# 
# # now create the data frame with 'region' identifier
# emhi.obs$CountyName <- emhi.series[emhi.obs$SeriesID == emhi.obs$SeriesID, 'CountyName']
# emhi.obs            <- left_join(emhi.obs, tx.regions, "CountyName")
# emhi.obs$EMHI       <- as.numeric(as.character(emhi.obs$EMHI))
# 
# 
# emhi.obs <- within(emhi.obs, value <- as.factor(cut(EMHI, quantile(EMHI), include.lowest = T, labels = F)))
# 
# choro_emhi     <- county_choropleth(emhi.obs, state_zoom = "texas", legend = "Rank: Lowest to Highest", num_colors = 4) +
#     ggtitle("Estimated Median Household Income\n 2014") +
#     coord_map()
# choro_emhi


# Per Capita Personal Income ----------------------------------------------


# # Get observations for "Per Capita Personal Income" into a table with county names
# pcpi.obs          <- obs.catcher(fred.series1, fred.obs1, "Per Capita Personal Income")
# pcpi.table        <- cat.tabler(pcpi.obs)
# pcpi.table.subset <- pcpi.table[year(pcpi.table$Date) == "2013", -1]
# pcpi.search       <- names(pcpi.table.subset) %>%
#                      paste(collapse = "|")
# pcpi.counties     <- fred.series1[grep(pcpi.search, fred.series1$SeriesID), 'CountyName'] %>%
#                      tolower()
# pcpi.data         <- cbind(PCPI = as.character(unlist(pcpi.table.subset)), CountyName = pcpi.counties) %>%
#                      as.data.frame()
# #!!!!!!!Start here!!!!!!!!~
# # Insert 'region'
# pcpi.data      <- left_join(pcpi.data, tx.regions, "CountyName")
# pcpi.data$PCPI <- as.numeric(as.character(pcpi.data$PCPI))
# 
# # Rank the values
# pcpi.data <- within(pcpi.data, value <- as.factor(cut(PCPI, quantile(PCPI), include.lowest = T, labels = F)))
# 
# # Plot it out
# choro_pcpi     <- county_choropleth(pcpi.obs, state_zoom = "texas", legend = "Rank: Lowest to Highest", num_colors = 4) +
#                   ggtitle("Per Capita Personal Income\n 2013") +
#                   coord_map()
# choro_pcpi


# All in one --------------------------------------------------------------


# These functions will:
# 1. Group data frames by category
# 2. Create a single data frame from and for each group of data frames
# 3. Save each of the data frames to a text file

# This function groups data frames by 'category'
# It returns a list of 10 elements (corresponding to the 10 'category' values)
# Each element has 254 data frames (one for each county)
    big.obs <- lapply(seq_along(fred.master$category), function(x){
        obs.catcher(series.table = fred.series,
                    obs.list     = fred.obs,
                    cat          = fred.master$category[x])
    })

# This function merges the data frames in each list element
# The result is a massive data frame in each of the 10 list elements
# The data frames are 255 columns wide
# Columns are 'Date' and the 254 'SeriesID' corresponding to each of the 254 counties
    fred.tables <- lapply(seq_along(big.obs), function(x){
        cat.tabler(big.obs[[x]])
    })

# write all 10 data frames to a file
# extension = ".txt"
# separator = "," separator
    lapply(seq_along(fred.tables), function(x){
        write.table(fred.tables[[x]], file = paste("Data/TableFiles/", names(fred.tables[x]), ".txt", sep = ""), sep = ",", row.names = F)
    })