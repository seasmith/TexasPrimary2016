# Load Dependencies -------------------------------------------------------


library(ggplot2)
library(dplyr)
library(rvest)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(knitr)
wd <- getwd() ## for saving
dir.create(file.path(wd, "Data"), showWarnings = FALSE)
dir.create(file.path(wd, "Data", "Election"), showWarnings = FALSE)

# Texas election data: http://elections.sos.state.tx.us/index.htm


# Republican Data Table ---------------------------------------------------


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


# Democratic Data Table ---------------------------------------------------


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


# Download Data - Geo -----------------------------------------------------


data("county.regions")
tx.regions   <- filter(county.regions, state.name == "texas") %>%
    select(region, "CountyName" = county.name)
tx.r.results <- left_join(tex.rep, tx.regions, "CountyName")
tx.d.results <- left_join(tex.dem, tx.regions, "CountyName")


# Individual Republican Data Table ----------------------------------------


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


# Individual Democrat Data Table ------------------------------------------


#Hillary Clinton
hc.counties <- filter(tx.d.results, Clinton > Sanders & Clinton > OMalley)
kable(hc.counties, caption = "Counties won by Clinton")

#Bernie Sanders
bs.counties <- filter(tx.d.results, Sanders > Clinton & Sanders > OMalley)
kable(bs.counties, caption = "Counties won by Sanders")


# Master Data Table -------------------------------------------------------


#tidy up
tex.results                 <- left_join(tex.rep, tex.dem, "CountyName")
tex.results                 <- mutate(tex.results, TotalVotes = (TotalVotes.x + TotalVotes.y),
                                      TotalTurnOut = (TotalVotes/TotalVoters.x)*100,
                                      RepShare = (TotalVotes.x/TotalVotes)*100,
                                      DemShare = (TotalVotes.y/TotalVotes)*100)
tex.results                 <- tex.results[, -c(2:15, 23:30)]
tex.results[,16:18]         <- round(tex.results[,16:18], digits = 1)
tex.results                 <- left_join(tex.results, tx.regions, "CountyName")
names(tex.results)[2:4]     <- c("RepVotes", "TotalVoters", "RepTurnOut")
names(tex.results)[c(9,11)] <- c("DemVotes", "DemTurnOut")
tex.results                 <- select(tex.results, c(CountyName, TotalVotes, TotalVoters,
                                                     TotalTurnOut, RepVotes, RepTurnOut, RepShare,
                                                     jk, mr, dt, tc, DemVotes, DemTurnOut,
                                                     DemShare, hc, bs, mo, region))
#turn out
tex.turnout       <- tex.results
tex.turnout$value <- tex.turnout$TotalTurnOut


# Save Results ------------------------------------------------------------

save(tex.rep, file = file.path(wd, "Data", "Election", "tex.rep.RData"))
save(tex.dem, file = file.path(wd, "Data", "Election", "tex.dem.RData"))
save(tex.turnout, file = file.path(wd, "Data", "Election", "tex.turnout.RData"))
save(tex.results, file = file.path(wd, "Data", "Election", "tex.results.RData"))