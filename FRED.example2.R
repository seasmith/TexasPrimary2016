# fred.data.example2
library(ggplot2, quietly = T)
library(dplyr, quietly = T)
library(plyr, quietly = T)
library(rvest, quietly = T)
library(choroplethr, quietly = T)
library(choroplethrMaps, quietly = T)
library(gridExtra, quietly = T)
library(lubridate, quietly = T)
source("cat.functions.R")
data("county.regions")


# Load Geo Data -----------------------------------------------------------

tx.regions                <- filter(county.regions, state.name == "texas") %>%
                             select(region, "CountyName" = county.name)


# Estimated Household Income ----------------------------------------------


# emhi.series               <- fred.series2[fred.series2$Category == "Estimate of Median Household Income", ]
# emhi.series$County        <- tolower(emhi.series$County)
# tx.regions                <- filter(county.regions, state.name == "texas") %>%
#                              select(region, "CountyName" = county.name)
# colnames(emhi.series)[11] <- "CountyName"
# 
# # Find all SeriesID matching the above filtered data's SeriesID
# emhi.obs  <- fred.obs2[names(fred.obs2) == emhi.series$SeriesID]

emhi.obs <- obs.catcher(fred.series2, fred.obs2, "Estimate of Median Household Income")

# Find 2014 values (last row)
loop.list <- list()
emhi.obs  <- lapply(seq_along(emhi.obs), function(x){

                 to.get    <- dim(emhi.obs[[x]])[1]
                 loop.df   <- data.frame(SeriesID = names(emhi.obs[x]),
                                         EMHI    = emhi.obs[[x]][to.get, 2])
                 loop.list[[x]] <- loop.df
}) %>% ldply

# now create the data frame with 'region' identifier
emhi.obs$CountyName <- emhi.series[emhi.obs$SeriesID == emhi.obs$SeriesID, 'CountyName']
emhi.obs            <- left_join(emhi.obs, tx.regions, "CountyName")
emhi.obs$EMHI       <- as.numeric(as.character(emhi.obs$EMHI))


emhi.obs <- within(emhi.obs, value <- as.factor(cut(EMHI, quantile(EMHI), include.lowest = T, labels = F)))

choro_emhi     <- county_choropleth(emhi.obs, state_zoom = "texas", legend = "Rank: Lowest to Highest", num_colors = 4) +
    ggtitle("Estimated Median Household Income\n 2014") +
    coord_map()
choro_emhi



# Per Capita Personal Income ----------------------------------------------


# Get observations for "Per Capita Personal Income" into a table with county names
pcpi.obs          <- obs.catcher(fred.series1, fred.obs1, "Per Capita Personal Income")
pcpi.table        <- cat.tabler(pcpi.obs)
pcpi.table.subset <- pcpi.table[year(pcpi.table$Date) == "2013", -1]
pcpi.search       <- names(pcpi.table.subset) %>%
                     paste(collapse = "|")
pcpi.counties     <- fred.series1[grep(pcpi.search, fred.series1$SeriesID), 'County'] %>%
                     tolower() ## Rename 'County' to 'CountyName' !!!!!!!!!!!!!
pcpi.data         <- cbind(PCPI = as.character(unlist(pcpi.table.subset)), CountyName = pcpi.counties) %>%
                     as.data.frame()
#!!!!!!!Start here!!!!!!!!~
# Insert 'region'
pcpi.data      <- left_join(pcpi.data, tx.regions, "CountyName")
pcpi.data$PCPI <- as.numeric(as.character(pcpi.data$PCPI))

# Rank the values
pcpi.data <- within(pcpi.data, value <- as.factor(cut(PCPI, quantile(PCPI), include.lowest = T, labels = F)))

# Plot it out
choro_pcpi     <- county_choropleth(pcpi.obs, state_zoom = "texas", legend = "Rank: Lowest to Highest", num_colors = 4) +
                  ggtitle("Per Capita Personal Income\n 2013") +
                  coord_map()
choro_pcpi



# Unemployment Rate -------------------------------------------------------


