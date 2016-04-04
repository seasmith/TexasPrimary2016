# fred.data.example2
library(ggplot2, quietly = T)
library(dplyr, quietly = T)
library(rvest, quietly = T)
library(choroplethr, quietly = T)
library(choroplethrMaps, quietly = T)
library(gridExtra, quietly = T)
data("county.regions")


# Estimated Household Income ----------------------------------------------


emhi.series               <- fred.series2[fred.series2$Category == "Estimate of Median Household Income", ]
emhi.series$County        <- tolower(emhi.series$County)
tx.regions                <- filter(county.regions, state.name == "texas") %>%
                             select(region, "CountyName" = county.name)
colnames(emhi.series)[11] <- "CountyName"

# Find all SeriesID matching the above filtered data's SeriesID
emhi.obs  <- fred.obs2[names(fred.obs2) == emhi.series$SeriesID]

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


pcpi.series               <- fred.series1[fred.series1$Category == "Per Capita Personal Income", ]
pcpi.series$County        <- tolower(pcpi.series$County)
tx.regions                <- filter(county.regions, state.name == "texas") %>%
    select(region, "CountyName" = county.name)
colnames(pcpi.series)[11] <- "CountyName"

# Find all SeriesID matching the above filtered data's SeriesID
pcpi.obs  <- fred.obs1[names(fred.obs1) == pcpi.series$SeriesID]

# Find 2014 values (last row)
loop.list <- list()
pcpi.obs  <- lapply(seq_along(pcpi.obs), function(x){
    
    to.get    <- dim(pcpi.obs[[x]])[1]
    loop.df   <- data.frame(SeriesID = names(pcpi.obs[x]),
                            PCPI     = pcpi.obs[[x]][to.get, 2])
    loop.list[[x]] <- loop.df
}) %>% ldply

# now create the data frame with 'region' identifier
pcpi.obs$CountyName <- pcpi.series[pcpi.obs$SeriesID == pcpi.obs$SeriesID, 'CountyName']
pcpi.obs            <- left_join(pcpi.obs, tx.regions, "CountyName")
pcpi.obs$PCPI       <- as.numeric(as.character(pcpi.obs$PCPI))


pcpi.obs <- within(pcpi.obs, value <- as.factor(cut(PCPI, quantile(PCPI), include.lowest = T, labels = F)))

choro_pcpi     <- county_choropleth(pcpi.obs, state_zoom = "texas", legend = "Rank: Lowest to Highest", num_colors = 4) +
    ggtitle("Per Capita Personal Income\n 2013") +
    coord_map()
choro_pcpi



# Unemployment Rate -------------------------------------------------------


