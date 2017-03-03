
# ---- Load Dependencies
# Load Dependencies -------------------------------------------------------

library(tidyverse)
library(choroplethr)
library(choroplethrMaps)
library(ggloop)
load("~/R/TexasPrimary2016/Data/FRED/FRED_2012.RData")
load("~/R/TexasPrimary2016/Data/FRED/FRED_2014.RData")



# ---- Filter Data Ex
# Filter Data Ex ----------------------------------------------------------

# Disregard
# # No need to plot data that are missing
# FRED_2014 <- FRED_2014[ , colSums(is.na(FRED_2014)) == 0]




# ---- Filter Data 1
# Filter Data -------------------------------------------------------------

.FRED <- FRED_2014 %>%
  select(CountyName,
         UnRate,
         ResPop,
         PCPI,
         BachDegree,
         HSDegree,
         EMHI)
.FRED$BachDegree <- FRED_2012$BachDegree  # replace 2014 education data w/ 2012
.FRED$HSDegree   <- FRED_2012$HSDegree    # replace 2014 education data w/ 2012




# ---- Filter Data 2
# Filter Out Education Data -----------------------------------------------

FRED <- .FRED %>% select(-BachDegree, -HSDegree)




# ---- Maps
# Maps --------------------------------------------------------------------

regions <- county.regions %>% 
  select(region, county.name, state.name) %>%
  filter(state.name == "texas") %>%
  select(-state.name)


FRED.map <- FRED %>%
  left_join(regions, by = c("CountyName" = "county.name"))


maps <- list()


map_it <- . %>%
  mutate_(.data = FRED.map, value = .) %>%
  select(value, region) %>% {
    county_choropleth(df = ., state_zoom = "texas") +
      coord_map()
  }


maps$UnRate <- "UnRate" %>% map_it
maps$ResPop <- "ResPop" %>% map_it
maps$PCPI   <- "PCPI" %>% map_it
maps$EMHI   <- "EMHI" %>% map_it


ResPop_Range <- cut(x      = FRED$ResPop,
                    breaks = quantile(FRED$ResPop),
                    labels = 1:4,
                    include.lowest = TRUE)