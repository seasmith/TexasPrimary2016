##!!You will need to use the data.frames from TexasPrimary2016.R


# Individual Republican Maps ----------------------------------------------


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


# Individual Democrat Maps ------------------------------------------------


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