# Load Dependencies -------------------------------------------------------


library(rvest)
library(dplyr)
library(seasmith)


# Texas election data: http://elections.sos.state.tx.us/index.htm


# Republican Data Table ---------------------------------------------------


# Download - download Republican dataset
RVotesCount <- "http://elections.sos.state.tx.us/elchist273_race62.htm" %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table() %>%
  `[[`(1)

# Annotate - set column names to last name of candidates, Uncommitted, etc.
r.first        <- names(RVotesCount)
r.last         <- RVotesCount[1,]
names(RVotesCount) <- c("CountyName", r.last[getColNums(r.last, Jeb:`Donald J.`)], "Uncommitted", "TotalVotes", "TotalVoters", "TurnOut")

# Tidy - remove first three rows (column names and aggregate row), use lower case,
#      - convert to numeric, remove CountyName spaces
RVotesCount            <- RVotesCount[-(1:3),]
RVotesCount$CountyName <- tolower(as.character(RVotesCount$CountyName))
RVotesCount[, getColNums(RVotesCount, Bush:TotalVoters)] <- sapply(select(RVotesCount, Bush:TotalVoters),
                                                                   function(x) as.numeric(gsub(",", "", x)))
RVotesCount[, "TurnOut"] <- sapply(RVotesCount[, "TurnOut"],
                                   function(x) as.numeric(gsub("%", "", x)))
RVotesCount$CountyName <- gsub("lasalle", "la salle", RVotesCount$CountyName)
RVotesCount$NotTop2R   <- RVotesCount %>%
  select(Bush:Christie, Fiorina:Santorum, Uncommitted) %>%
  apply(1, sum)
RVotesCount            <- RVotesCount %>%
  select(CountyName:Uncommitted, NotTop2R, TotalVotes:TurnOut)

#add percent columns for top 4
RVotesPercent <- transmute(RVotesCount,
                           CountyName = CountyName,
                           Bush        = round((Bush/TotalVotes)*100, digits = 2),
                           Carson      = round((Carson/TotalVotes)*100, digits = 2),
                           Christie    = round((Christie/TotalVoters)*100, digits = 2),
                           Cruz        = round((Cruz/TotalVotes)*100, digits = 2),
                           Fiorina     = round((Fiorina/TotalVotes)*100, digits = 2),
                           Graham      = round((Graham/TotalVotes)*100, digits = 2),
                           Gray        = round((Gray/TotalVotes)*100, digits = 2),
                           Huckabee    = round((Huckabee/TotalVotes)*100, digits = 2),
                           Kasich      = round((Kasich/TotalVotes)*100, digits = 2),
                           Paul        = round((Paul/TotalVotes)*100, digits = 2),
                           Rubio       = round((Rubio/TotalVotes)*100, digits = 2),
                           Santorum    = round((Santorum/TotalVotes)*100, digits = 2),
                           Trump       = round((Trump/TotalVotes)*100, digits = 2),
                           Uncommitted = round((Uncommitted/TotalVotes)*100, digits = 2),
                           NotTop2R    = round((NotTop2R/TotalVotes)*100, digits = 2)
)

# Tidy - Change certain column names.
nm.ndx <- getColNums(RVotesCount, TotalVotes, TotalVoters, TurnOut)
names(RVotesCount)[nm.ndx] <- c("TotalVotesR", "TotalVotersR", "TurnOutR")

# Democratic Data Table ---------------------------------------------------


# Download - download Democrat dataset
DVotesCount <- "http://elections.sos.state.tx.us/elchist233_race62.htm" %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table() %>%
  `[[`(1)

# Annotate - set column names to last name of candidates, TotalVotes, etc
d.first           <- names(DVotesCount)
d.last            <- DVotesCount[1,]
names(DVotesCount)    <- c("CountyName", d.last[getColNums(d.last, Hillary:`Willie L.`)], "TotalVotes", "TotalVoters", "TurnOut")
names(DVotesCount)[3] <- "De_La_Fuente"
names(DVotesCount)[7] <- "OMalley"

# Tidy - remove first three rows (column names and aggregate row), use lower case,
#      - convert to numeric, remove CountyName spaces
DVotesCount            <- DVotesCount[-(1:3),]
DVotesCount$CountyName <- tolower(as.character(DVotesCount$CountyName))
DVotesCount[, getColNums(DVotesCount, Clinton:TotalVoters)] <- sapply(select(DVotesCount, Clinton:TotalVoters),
                                                              function(x) as.numeric(gsub(",", "", x)))
DVotesCount[, "TurnOut"] <- sapply(DVotesCount[, "TurnOut"], function(x) as.numeric(gsub("%", "", x)))
DVotesCount$CountyName <- gsub("lasalle", "la salle", DVotesCount$CountyName)
DVotesCount$NotTop2D   <- DVotesCount %>%
  select(De_La_Fuente:OMalley, Wilson) %>%
  apply(1, sum)
DVotesCount            <- DVotesCount %>%
  select(CountyName:Wilson, NotTop2D, TotalVotes:TurnOut)

#add percent columns for top 4
DVotesPercent <- transmute(DVotesCount,
                           CountyName = CountyName,
                           Clinton      = round((Clinton/TotalVotes)*100, digits = 2),
                           De_La_Fuente = round((De_La_Fuente/TotalVotes)*100, digits = 2),
                           Hawes        = round((Hawes/TotalVotes)*100, digits = 2),
                           Judd         = round((Judd/TotalVotes)*100, digits = 2),
                           Locke        = round((Locke/TotalVotes)*100, digits = 2),
                           OMalley      = round((OMalley/TotalVotes)*100, digits = 2),
                           Sanders      = round((Sanders/TotalVotes)*100, digits = 2),
                           Wilson       = round((Wilson/TotalVotes)*100, digits = 2),
                           NotTop2D     = round((NotTop2D/TotalVotes)*100, digits = 2)
)

nm.ndx <- getColNums(DVotesCount, TotalVotes, TotalVoters, TurnOut)
names(DVotesCount)[nm.ndx] <- c("TotalVotesD", "TotalVotersD", "TurnOutD")

# Master Data Table -------------------------------------------------------


#tidy up
TotalVotesCount <- left_join(RVotesCount, DVotesCount, "CountyName")
TotalVotesCount <- TotalVotesCount %>%
  mutate(TotalVotesAll  = TotalVotesR + TotalVotesD,
         TotalVotersAll = TotalVotersD,
         TurnOutAll     = TurnOutR + TurnOutD) %>%
  select(-TotalVotersD, -TotalVotersR)

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
               WinnerR    = wR,
               RunnerUpR  = RunnerUp,
               rankR)
rownames(rankR) <- NULL



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
               WinnerD    = unlist(wD),
               rankD)
rownames(rankD) <- NULL



# All Rankings ------------------------------------------------------------

rankAll <- bind_cols(rankR, select(rankD, -CountyName))

rankAll$PartyWinner <- tex.results %>%
  select(TurnOutR, TurnOutD) %>%
  max.col() %>%
  c("Republican", "Democrat")[.]



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
save(rankAll, file = file.path(dir.save, "rankAll.RData"))
