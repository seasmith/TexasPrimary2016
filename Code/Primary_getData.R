# Load Dependencies -------------------------------------------------------


library(ggplot2)
library(rvest)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
# library(gridExtra)
library(knitr)
library(seasmith)


# Texas election data: http://elections.sos.state.tx.us/index.htm


# Republican Data Table ---------------------------------------------------


# Download - download Republican dataset
tex.rep <- "http://elections.sos.state.tx.us/elchist273_race62.htm" %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table() %>%
  `[[`(1)

# Annotate - set column names to last name of candidates, Uncommitted, etc.
r.first        <- names(tex.rep)
r.last         <- tex.rep[1,]
names(tex.rep) <- c("CountyName", r.last[getColNums(r.last, Jeb:`Donald J.`)], "Uncommitted", "TotalVotes", "TotalVoters", "TurnOut")

# Tidy - remove first three rows (column names and aggregate row), use lower case,
#      - convert to numeric, remove CountyName spaces
tex.rep            <- tex.rep[-(1:3),]
tex.rep$CountyName <- tolower(as.character(tex.rep$CountyName))
tex.rep[, getColNums(tex.rep, Bush:TotalVoters)] <- sapply(select(tex.rep, Bush:TotalVoters),
                                                   function(x) as.numeric(gsub(",", "", x)))
tex.rep[, "TurnOut"] <- sapply(tex.rep[, "TurnOut"],
                               function(x) as.numeric(gsub("%", "", x)))
tex.rep$CountyName <- gsub("lasalle", "la salle", tex.rep$CountyName)
tex.rep$RNotTop2   <- tex.rep %>%
  select(Bush:Christie, Fiorina:Santorum, Uncommitted) %>%
  apply(1, sum)
tex.rep            <- tex.rep %>%
  select(CountyName:Uncommitted, RNotTop2, TotalVotes:TurnOut)

#add percent columns for top 4
tex.rep <- mutate(tex.rep,
                jb = (Bush/TotalVotes)*100,
                bc = (Carson/TotalVotes)*100,
                cc = (Christie/TotalVoters)*100,
                tc = (Cruz/TotalVotes)*100,
                cf = (Fiorina/TotalVotes)*100,
                lg = (Graham/TotalVotes)*100,
                eg = (Gray/TotalVotes)*100,
                mh = (Huckabee/TotalVotes)*100,
                jk = (Kasich/TotalVotes)*100,
                rp = (Paul/TotalVotes)*100,
                mr = (Rubio/TotalVotes)*100,
                rs = (Santorum/TotalVotes)*100,
                dt = (Trump/TotalVotes)*100,
                rnt2 = (RNotTop2/TotalVotes)*100
                )
tex.rep[, getColNums(tex.rep, jb:rnt2)] <- round(select(tex.rep, jb:rnt2), digits = 2)


# Democratic Data Table ---------------------------------------------------


# Download - download Democrat dataset
tex.dem <- "http://elections.sos.state.tx.us/elchist233_race62.htm" %>%
            read_html() %>%
            html_nodes("table") %>%
            html_table() %>%
            `[[`(1)

# Annotate - set column names to last name of candidates, TotalVotes, etc
d.first           <- names(tex.dem)
d.last            <- tex.dem[1,]
names(tex.dem)    <- c("CountyName", d.last[getColNums(d.last, Hillary:`Willie L.`)], "TotalVotes", "TotalVoters", "TurnOut")
names(tex.dem)[3] <- "De_La_Fuente"
names(tex.dem)[7] <- "OMalley"

# Tidy - remove first three rows (column names and aggregate row), use lower case,
#      - convert to numeric, remove CountyName spaces
tex.dem            <- tex.dem[-(1:3),]
tex.dem$CountyName <- tolower(as.character(tex.dem$CountyName))
tex.dem[, getColNums(tex.dem, Clinton:TotalVoters)] <- sapply(select(tex.dem, Clinton:TotalVoters),
                                                              function(x) as.numeric(gsub(",", "", x)))
tex.dem[, "TurnOut"] <- sapply(tex.dem[, "TurnOut"], function(x) as.numeric(gsub("%", "", x)))
tex.dem$CountyName <- gsub("lasalle", "la salle", tex.dem$CountyName)
tex.dem$DNotTop2   <- tex.dem %>%
  select(De_La_Fuente:OMalley, Wilson) %>%
  apply(1, sum)
tex.dem            <- tex.dem %>%
  select(CountyName:Wilson, DNotTop2, TotalVotes:TurnOut)

#add percent columns for top 4
tex.dem <- mutate(tex.dem,
                  hc = (Clinton/TotalVotes)*100,
                  rd = (De_La_Fuente/TotalVotes)*100,
                  ch = (Hawes/TotalVotes)*100,
                  kj = (Judd/TotalVotes)*100,
                  sl = (Locke/TotalVotes)*100,
                  mo = (OMalley/TotalVotes)*100,
                  bs = (Sanders/TotalVotes)*100,
                  ww = (Wilson/TotalVotes)*100,
                  dnt2 = (DNotTop2/TotalVotes)*100
                  )
tex.dem[, getColNums(tex.dem, hc:dnt2)] <- round(select(tex.dem, hc:dnt2),
                                                 digits = 2)



# Master Data Table -------------------------------------------------------


#tidy up
tex.results                 <- left_join(tex.rep, tex.dem, "CountyName")
tex.results                 <- mutate(tex.results,
                                      TotalVoters  = TotalVoters.x,
                                      TotalVotes   = (TotalVotes.x + TotalVotes.y),
                                      TotalTurnOut = (TotalVotes/TotalVoters.x)*100)
tex.results                 <- dplyr::rename(tex.results,
                                             TotalVotesR  = TotalVotes.x,
                                             TotalVotersR = TotalVoters.x,
                                             TurnOutR     = TurnOut.x,
                                             TotalVotesD  = TotalVotes.y,
                                             TotalVotersD = TotalVoters.y,
                                             TurnOutD     = TurnOut.y)
# tex.results                 <- tex.results[, -grep("TotalVoters.x|TotalVoters.y", names(tex.results))]
# tex.results                 <- left_join(tex.results, tx.regions, "CountyName")



# Agg functions -----------------------------------------------------------


# Rank the specified data frame or portion of the data frame.
rank.df <- function(df) {
    rs <- apply(df, 1, function(x) rank(-x))
    df <- as.data.frame(t(rs))
    return(df)
}



# Individual Republican Data Tables ---------------------------------------

# Rank, find winners, find number of counties won, and names of those who won.
rankR <- rank.df(select(tex.results, Bush:Uncommitted))
ndxR <- max.col(select(tex.results, Bush:Uncommitted), ties.method = "first")
wR <- names(select(tex.results, Bush:Uncommitted))[ndxR]

RunnerUp <- apply(select(rankR, Bush:Uncommitted), 1, function(x) x == 2) %>%
  t() %>%
  as.data.frame() %>%
  apply(1, which) %>%
  lapply(function(x) names(select(tex.results, Bush:Uncommitted))[x]) %>%
  unlist
names(RunnerUp) <- tex.results$CountyName

rankR <- cbind(CountyName = tex.results$CountyName,
               Winner     = wR,
               RunnerUp   = RunnerUp,
               rankR)
rownames(rankR) <- NULL

countiesR     <- lapply(rankR[, -(1:3)], function(x) rankR[x < 2, ])
countiesR.won <- sapply(countiesR, function(x) nrow(x))
countiesR.won.names <- names(countiesR.won[which(countiesR.won > 0)])

winnersR <- lapply(countiesR.won.names, function(x) kable(countiesR[[x]]))
names(winnersR) <- countiesR.won.names



# Individual Democrat Data Table ------------------------------------------


rankD <- rank.df(select(tex.results, Clinton:Wilson))
firsts <- apply(select(rankD, Clinton:Wilson), 1, function(x) x == 1) %>%
  t() %>%
  as.data.frame() %>%
  apply(1, which) %>%
  lapply(function(x) names(select(tex.results, Clinton:Wilson))[x])
names(firsts) <- tex.results$CountyName

ties <- apply(select(rankD, Clinton:Wilson), 1, function(x) x == 1.5) %>%
  t() %>%
  as.data.frame() %>%
  apply(1, which) %>%
  lapply(function(x) {
    nms <- names(select(tex.results, Clinton:Wilson))[x] %>%
      paste(collapse = "-")
    if (nzchar(nms)) nms else NULL
  })
names(ties) <- tex.results$CountyName

# ties.ndx <- ties %>% lengths() %>% `>`(0)
# ties <- ties[ties.ndx]

wD <- Map(c, firsts, ties)
rankD <- cbind(CountyName = tex.results[, 1],
               Winner     = unlist(wD),
               rankD)
rownames(rankD) <- NULL

countiesD     <- lapply(rankD[, -(1:2)], function(x) rankD[x < 2, ])
countiesD.won <- sapply(countiesD, function(x) nrow(x))
countiesD.won.names <- names(countiesD.won[which(countiesD.won > 0)])

winnersD <- lapply(countiesD.won.names, function(x) kable(countiesD[[x]]))
names(winnersD) <- countiesD.won.names



# Download Data - Geo -----------------------------------------------------


data("county.regions")
tx.regions   <- filter(county.regions, state.name == "texas") %>%
  select(region, "CountyName" = county.name)
tx.rep <- left_join(tex.rep, tx.regions, "CountyName")
tx.dem <- left_join(tex.dem, tx.regions, "CountyName")



# Save Results ------------------------------------------------------------

dir.save <- "Data/Primary"
dir.create(dir.save, showWarnings = FALSE)
save(tex.rep, file = file.path(dir.save, "tex.rep.RData"))
save(tex.dem, file = file.path(dir.save, "tex.dem.RData"))
save(tex.results, file = file.path(dir.save, "tex.results.RData"))
save(rankR, file = file.path(dir.save, "rankR.RData"))
save(rankD, file = file.path(dir.save, "rankD.RData"))
