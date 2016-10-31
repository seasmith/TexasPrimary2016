# Load Dependencies -------------------------------------------------------


library(ggplot2)
library(rvest)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(knitr)


# Texas election data: http://elections.sos.state.tx.us/index.htm


# Republican Data Table ---------------------------------------------------


#download
tex.rep            <- "http://elections.sos.state.tx.us/elchist273_race62.htm" %>%
                      read_html() %>%
                      html_nodes("table") %>%
                      html_table() %>%
                     `[[`(1)

#set names
r.first           <- names(tex.rep)
r.last            <- tex.rep[1,]
names(tex.rep)     <- c("CountyName", r.last[2:14], "Uncommitted", "TotalVotes", "TotalVoters", "TurnOut")

#tidy up
tex.rep            <- tex.rep[-(1:3),]
tex.rep$CountyName <- tolower(as.character(tex.rep$CountyName))
tex.rep[,2:17]     <- sapply(tex.rep[,2:17], function(x) as.numeric(gsub(",", "", x)))
tex.rep[,18]       <- sapply(tex.rep[,18], function(x) as.numeric(gsub("%", "", x)))
tex.rep$CountyName <- gsub("lasalle", "la salle", tex.rep$CountyName)

#add percent columns for top 4
tex.rep <- mutate(tex.rep,
                jb = (Bush/TotalVotes)*100,
                bc = (Carson/TotalVotes)*100,
                tc = (Cruz/TotalVotes)*100,
                cf = (Fiorina/TotalVotes)*100,
                lg = (Graham/TotalVotes)*100,
                eg = (Gray/TotalVotes)*100,
                mh = (Huckabee/TotalVotes)*100,
                jk = (Kasich/TotalVotes)*100,
                rp = (Paul/TotalVotes)*100,
                mr = (Rubio/TotalVotes)*100,
                rs = (Santorum/TotalVotes)*100,
                dt = (Trump/TotalVotes)*100)
tex.rep[,19:30] <- round(tex.rep[,19:30], digits = 2)


# Democratic Data Table ---------------------------------------------------


#download
tex.dem <- "http://elections.sos.state.tx.us/elchist233_race62.htm" %>%
            read_html() %>%
            html_nodes("table") %>%
            html_table() %>%
            `[[`(1)

#set names
d.first           <- names(tex.dem)
d.last            <- tex.dem[1,]
names(tex.dem)    <- c("CountyName", d.last[2:9], "TotalVotes", "TotalVoters", "TurnOut")
names(tex.dem)[7] <- "OMalley"

#tidy up
tex.dem            <- tex.dem[-(1:3),]
tex.dem$CountyName <- tolower(as.character(tex.dem$CountyName))
tex.dem[,2:11]     <- sapply(tex.dem[,2:11], function(x) as.numeric(gsub(",", "", x)))
tex.dem[,12]       <- sapply(tex.dem[,12], function(x) as.numeric(gsub("%", "", x)))
tex.dem$CountyName <- gsub("lasalle", "la salle", tex.dem$CountyName)

#add percent columns for top 4
tex.dem <- mutate(tex.dem,
                  hc = (Clinton/TotalVotes)*100,
                  rd = (`De La Fuente`/TotalVotes)*100,
                  ch = (Hawes/TotalVotes)*100,
                  kj = (Judd/TotalVotes)*100,
                  sl = (Locke/TotalVotes)*100,
                  mo = (OMalley/TotalVotes)*100,
                  bs = (Sanders/TotalVotes)*100,
                  ww = (Wilson/TotalVotes)*100
)
tex.dem[,13:20] <- round(tex.dem[,13:20], digits = 2)


# Download Data - Geo -----------------------------------------------------


data("county.regions")
tx.regions   <- filter(county.regions, state.name == "texas") %>%
    select(region, "CountyName" = county.name)
tx.rep <- left_join(tex.rep, tx.regions, "CountyName")
tx.dem <- left_join(tex.dem, tx.regions, "CountyName")



# Agg functions -----------------------------------------------------------

# Rank the specified data frame or portion of the data frame.
rank.df <- function(df) {
    rs <- apply(df, 1, function(x) rank(-x))
    df <- as.data.frame(t(rs))
    return(df)
}

# Individual Republican Data Tables ---------------------------------------

rankR <- rank.df(tex.results[,2:14])
rankR <- cbind(CountyName = tex.results[,1], rankR)

# Data Table template
# counties.Trump <- rankR[rankR$Trump == 1, ]

#John Kasich
jk.counties <- filter(tex.results, Kasich > Cruz & Kasich > Trump & Kasich > Rubio)
kable(jk.counties, caption = "Counties won by Kasich")

#Marco Rubio
mr.counties <- filter(tex.results, Rubio > Cruz & Rubio > Trump & Rubio > Kasich)
kable(mr.counties, caption = "Counties won by Rubio")

#Donald Trump
dt.counties <- filter(tex.results, Trump > Cruz & Trump > Rubio & Trump > Kasich) %>%
    select(1,19:22)
kable(dt.counties, caption = "Counties won by Trump")

#Ted Cruz
tc.counties <- filter(tex.results, Cruz > Trump & Cruz > Rubio & Cruz > Kasich)
kable(tc.counties, caption = "Counties won by Cruz")


# Individual Democrat Data Table ------------------------------------------


#Hillary Clinton
hc.counties <- filter(tex.results, Clinton > Sanders & Clinton > OMalley)
kable(hc.counties, caption = "Counties won by Clinton")

#Bernie Sanders
bs.counties <- filter(tex.results, Sanders > Clinton & Sanders > OMalley)
kable(bs.counties, caption = "Counties won by Sanders")


# Master Data Table -------------------------------------------------------


#tidy up
tex.results                 <- left_join(tex.rep, tex.dem, "CountyName")
tex.results                 <- mutate(tex.results,
                                      TotalVoters  = TotalVoters.x,
                                      TotalVotes   = (TotalVotes.x + TotalVotes.y),
                                      TotalTurnOut = (TotalVotes/TotalVoters.x)*100)
tex.results                 <- dplyr::rename(tex.results,
                                             TotalVotesR = TotalVotes.x,
                                             TurnOutR    = TurnOut.x,
                                             TotalVotesD = TotalVotes.y,
                                             TurnOutD    = TurnOut.y)
tex.results                 <- tex.results[, -grep("TotalVoters.x|TotalVoters.y", names(tex.results))]
tex.results                 <- left_join(tex.results, tx.regions, "CountyName")

# These two have been abandoned in favor of TotalVotesR, TurnOutR, TotalVotesD,
# and TurnOutD.
# names(tex.results)[2:4]     <- c("RepVotes", "TotalVoters", "RepTurnOut")
# names(tex.results)[c(9,11)] <- c("DemVotes", "DemTurnOut")

# I don't know what this was.
# tex.results                 <- select(tex.results, c(CountyName, TotalVotes, TotalVoters,
#                                                      TotalTurnOut, RepVotes, RepTurnOut, RepShare,
#                                                      jk, mr, dt, tc, DemVotes, DemTurnOut,
#                                                      DemShare, hc, bs, mo, region))
#turn out
# tex.turnout       <- tex.results
# tex.turnout$value <- tex.turnout$TotalTurnOut


# Save Results ------------------------------------------------------------

dir.save <- "Data/Primary"
dir.create(dir.save, showWarnings = FALSE)
save(tex.rep, file = file.path(dir.save, "tex.rep.RData"))
save(tex.dem, file = file.path(dir.save, "tex.dem.RData"))
save(tex.turnout, file = file.path(dir.save, "tex.turnout.RData"))
save(tex.results, file = file.path(dir.save, "tex.results.RData"))