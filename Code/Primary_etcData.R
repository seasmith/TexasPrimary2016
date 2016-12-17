
# Load Dependencies -------------------------------------------------------

library(dplyr)
load("~/R/TexasPrimary2016/Data/Primary/rankR.RData")
load("~/R/TexasPrimary2016/Data/Primary/rankD.RData")
load("~/R/TexasPrimary2016/Data/Primary/tex.results.RData")



# Republican Rankings -----------------------------------------------------


countiesR     <- lapply(rankR[, -(1:3)], function(x) rankR[x < 2, ])
countiesR.won <- sapply(countiesR, function(x) nrow(x))
countiesR.won.names <- names(countiesR.won[which(countiesR.won > 0)])

winnersR <- lapply(countiesR.won.names, function(x) kable(countiesR[[x]]))
names(winnersR) <- countiesR.won.names



# Democrat Rankings -------------------------------------------------------


countiesD     <- lapply(rankD[, -(1:2)], function(x) rankD[x < 2, ])
countiesD.won <- sapply(countiesD, function(x) nrow(x))
countiesD.won.names <- names(countiesD.won[which(countiesD.won > 0)])

winnersD <- lapply(countiesD.won.names, function(x) kable(countiesD[[x]]))
names(winnersD) <- countiesD.won.names

