##!!You will need to use data from TexasPrimary2016.R



# Master Table - Prepping -------------------------------------------------


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


# Example Maps ------------------------------------------------------------


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

#County turnout (both parties combined)
choro_turnout     <- county_choropleth(tex.turnout, state_zoom = "texas", legend = "%", num_colors = 5) +
    ggtitle("County Turnout") +
    coord_map()
choro_turnout

#Donald Trump vs Hillary Clinton
grid.arrange(choro_dt, choro_hc, ncol = 2, top = "The Delegate Leaders")

#Party turnout comparison
t.tx        <- tex.results
t.tx$winner <- as.factor(ifelse(t.tx$RepShare > t.tx$DemShare, "Republicans", "Democrats"))
t.tx$value  <- t.tx$winner
choro_party <- county_choropleth(t.tx, state_zoom = "texas", legend = "Winner", num_colors = 2) +
    ggtitle("Texas Presidential Primary\n Republicans vs Democrats\n March 15, 2016") +
    coord_map()

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