
# Load Dependencies -------------------------------------------------------


library(ggplot2)
library(dplyr)
library(rvest)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(knitr)


# Individual Republican Maps ----------------------------------------------

plotsR.df <- lapply(select(tex.results, TurnOutR:dt), function(x) {
    data.frame(region = tex.results$region, value = x, stringsAsFactors = FALSE)
})

plotsR <- lapply(plotsR.df, function(x) {
    county_choropleth(df = x,
                      state_zoom = "texas",
                      legend = "%",
                      num_colors = 1) +
        coord_map()
})

plotsR <- Map(function(x, y) x + ggtitle(y),
              x = plotsR,
              y = c("Turn Out: Republican", names(select(tex.results, Bush:Trump))))


# Individual Democrat Maps ------------------------------------------------


plotsD.df <- lapply(select(tex.results, TurnOutD:ww), function(x) {
    data.frame(region = tex.results$region, value = x, stringsAsFactors = FALSE)
})

plotsD <- lapply(plotsD.df, function(x) {
    county_choropleth(x, state_zoom = "texas", legend = "%", num_colors = 1) + coord_map()
})

plotsD <- Map(function(x, y) x + ggtitle(y),
              x = plotsD,
              y = c("Turn Out Democrat", names(select(tex.results, Clinton:Wilson))))


# Party Comparison Maps ---------------------------------------------------


#Republican counties won
tex.counties.rep <- filter(tex.results, RepShare > DemShare)
kable(tex.counties.rep, caption = "Counties won by Republicans")

tex.rep       <- tex.results
tex.rep$value <- tex.rep$RepShare
choro_rep     <- county_choropleth(tex.rep, state_zoom = "texas", legend = "%", num_colors = 1) +
    ggtitle("Republican Counties") +
    coord_map()
choro_rep

#Democrat counites won
tex.counties.dem <- filter(tex.results, DemShare > RepShare)
kable(tex.counties.dem, caption = "Counties won by Democrats")

tex.dem       <- tex.results
tex.dem$value <- tex.results$DemShare
choro_dem     <- county_choropleth(tex.dem, state_zoom = "texas", legend = "%", num_colors = 1) +
    ggtitle("Democratic Turnout") +
    coord_map()
choro_dem

#Party turnout winner
t.tx        <- tex.results
t.tx$winner <- as.factor(ifelse(t.tx$RepShare > t.tx$DemShare, "Republicans", "Democrats"))
t.tx$value  <- t.tx$winner
choro_party <- county_choropleth(t.tx, state_zoom = "texas", legend = "Winner", num_colors = 2) +
    ggtitle("Texas Presidential Primary\n Republicans vs Democrats\n March 15, 2016") +
    coord_map()

#County turnout (both parties combined)
choro_turnout     <- county_choropleth(tex.turnout, state_zoom = "texas", legend = "%", num_colors = 5) +
    ggtitle("County Turnout") +
    coord_map()
choro_turnout


# Misc --------------------------------------------------------------------


#Donald Trump vs Hillary Clinton
grid.arrange(choro_dt, choro_hc, ncol = 2, top = "The Delegate Leaders")

#Candidate with most votes (out of both parties)
votes     <- tex.results[,c(1:14,23:29)]
purevotes <- votes[,-1]
winner    <- sapply(seq_along(purevotes$Bush), function(x){
    
    most <- max(purevotes[x,])
    cand <- grep(most, purevotes[x,])
    names(purevotes)[cand]
})
tex.results$winner <- winner
tex.results$value  <- tex.results$winner
choro_winner <- county_choropleth(tex.results, state_zoom = "texas", legend = "Winner", num_colors = 4) +
    ggtitle("Leading Vote Getter: Republican or Democrat") +
    coord_map()
choro_winner