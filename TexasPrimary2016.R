library(ggplot2)
library(dplyr)
library(rvest)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(knitr)

#Texas election data: http://elections.sos.state.tx.us/index.htm



# Download Data - Republicans ---------------------------------------------


#download
tex.rep            <- "http://elections.sos.state.tx.us/elchist273_race62.htm" %>%
                      read_html() %>%
                      html_nodes("table") %>%
                      html_table()
tex.rep            <- tex.rep[[1]]

#set names
tr.first           <- names(tex.rep)
tr.last            <- tex.rep[1,]
names(tex.rep)     <- c("CountyName", tr.last[2:14], "Uncommitted", "TotalVotes", "TotalVoters", "TurnOut")

#tidy up
tex.rep            <- tex.rep[-(1:3),]
tex.rep$CountyName <- tolower(as.character(tex.rep$CountyName))
tex.rep[,2:17]     <- sapply(tex.rep[,2:17], function(x) as.numeric(gsub(",", "", x)))
tex.rep[,18]       <- sapply(tex.rep[,18], function(x) as.numeric(gsub("%", "", x)))
tex.rep$CountyName <- gsub("lasalle", "la salle", tex.rep$CountyName)

#add percent columns for top 4
tex.rep <- mutate(tex.rep,
                jk = (Kasich/TotalVotes)*100,
                mr = (Rubio/TotalVotes)*100,
                dt = (Trump/TotalVotes)*100,
                tc = (Cruz/TotalVotes)*100
                )
tex.rep[,19:22] <- round(tex.rep[,19:22], digits = 1)


# Download Data - Democrats -----------------------------------------------


#download
tex.dem            <- "http://elections.sos.state.tx.us/elchist233_race62.htm" %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
tex.dem            <- tex.dem[[1]]

#set names
td.first           <- names(tex.dem)
td.last            <- tex.dem[1,]
names(tex.dem)     <- c("CountyName", td.last[2:9], "TotalVotes", "TotalVoters", "TurnOut")
names(tex.dem)[7]  <- "OMalley"

#tidy up
tex.dem            <- tex.dem[-(1:3),]
tex.dem$CountyName <- tolower(as.character(tex.dem$CountyName))
tex.dem[,2:11]     <- sapply(tex.dem[,2:11], function(x) as.numeric(gsub(",", "", x)))
tex.dem[,12]       <- sapply(tex.dem[,12], function(x) as.numeric(gsub("%", "", x)))
tex.dem$CountyName <- gsub("lasalle", "la salle", tex.dem$CountyName)

#add percent columns for top 4
tex.dem <- mutate(tex.dem,
                  hc = (Clinton/TotalVotes)*100,
                  bs = (Sanders/TotalVotes)*100,
                  mo = (OMalley/TotalVotes)*100
)
tex.dem[,13:15] <- round(tex.dem[,13:15], digits = 1)
#####-----Democrats



# Download Data - Geo -----------------------------------------------------


#get state geo data
data("county.regions")
tx.regions <- filter(county.regions, state.name == "texas") %>%
              select(region, "CountyName" = county.name)
tx.r.results <- left_join(tex.rep, tx.regions)
tx.d.results <- left_join(tex.dem, tx.regions)
#####-----Geo Data



# Tabling Data - Republicans ----------------------------------------------


#John Kasich
jk.counties <- filter(tx.r.results, Kasich > Cruz & Kasich > Trump & Kasich > Rubio)
kable(jk.counties, caption = "Counties won by Kasich")

#Marco Rubio
mr.counties <- filter(tx.r.results, Rubio > Cruz & Rubio > Trump & Rubio > Kasich)
kable(mr.counties, caption = "Counties won by Rubio")

#Donald Trump
dt.counties <- filter(tx.r.results, Trump > Cruz & Trump > Rubio & Trump > Kasich) %>%
               select(1,19:22)
kable(dt.counties, caption = "Counties won by Trump")

#Ted Cruz
tc.counties <- filter(tx.r.results, Cruz > Trump & Cruz > Rubio & Cruz > Kasich)
kable(tc.counties, caption = "Counties won by Cruz")


# Tabling Data - Democrats ------------------------------------------------


#Hillary Clinton
hc.counties <- filter(tx.d.results, Clinton > Sanders & Clinton > OMalley)
kable(hc.counties, caption = "Counties won by Clinton")

#Bernie Sanders
bs.counties <- filter(tx.d.results, Sanders > Clinton & Sanders > OMalley)
kable(bs.counties, caption = "Counties won by Sanders")
#####-----Democrats



# Maps - Republican -------------------------------------------------------


#Ted Cruz
tx.r.tc       <- tx.r.results
tx.r.tc$value <- tx.r.results$tc
choro_tc      <- county_choropleth(tx.r.tc, state_zoom="texas", legend = "%", num_colors=1) + 
                 ggtitle("Ted Cruz") +
                 coord_map()  # Adds a Mercator projection
choro_tc

#Donald Trump
tx.r.dt       <- tx.r.results
tx.r.dt$value <- tx.r.results$dt
choro_dt = county_choropleth(tx.r.dt, state_zoom="texas", legend = "%", num_colors=1) + 
    ggtitle("Donald Trump") +
    coord_map()  # Adds a Mercator projection
choro_dt

#John Kasich
tx.r.jk       <- tx.r.results
tx.r.jk$value <- tx.r.results$jk
choro_jk = county_choropleth(tx.r.jk, state_zoom="texas", legend = "%", num_colors=1) + 
    ggtitle("John Kasich") +
    coord_map()  # Adds a Mercator projection
choro_jk

#Marco Rubio
tx.r.mr       <- tx.r.results
tx.r.mr$value <- tx.r.results$mr
choro_mr = county_choropleth(tx.r.mr, state_zoom="texas", legend = "%", num_colors=1) + 
    ggtitle("Marco Rubio") +
    coord_map()  # Adds a Mercator projection
choro_mr


# Maps - Democrats --------------------------------------------------------


#Hillary Clinton
tx.d.hc       <- tx.d.results
tx.d.hc$value <- tx.d.results$hc
choro_hc = county_choropleth(tx.d.hc, state_zoom="texas", legend = "%", num_colors=1) + 
    ggtitle("Hillary Clinton") +
    coord_map()  # Adds a Mercator projection
choro_hc

#Bernie Sanders
tx.d.bs       <- tx.d.results
tx.d.bs$value <- tx.d.results$bs
choro_bs = county_choropleth(tx.d.bs, state_zoom="texas", legend = "%", num_colors=1) + 
    ggtitle("Bernie Sanders") +
    coord_map()  # Adds a Mercator projection
choro_bs