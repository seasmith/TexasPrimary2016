# Load Dependencies -------------------------------------------------------

library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales) ## for the object percent in scales_x_continuous()
source("~/R/TexasPrimary2016/Functions/agg.functions.R")
load("~/R/TexasPrimary2016/Data/FRED/fred.cat.list.RData")
load("~/R/TexasPrimary2016/Data/Election/tex.results.RData")


# Create 2013 tables ------------------------------------------------------


### Unemployment rate
    ur.2013 <- annual.average(fred.cat.list$`Unemployment Rate`, date = 2013)

### Per Capita Personal Income (PCPI)
    pcpi.2013 <- annual.value(fred.cat.list$`Per Capita Personal Income`, date = 2013)

### Civilian Labor Force (CLF)
    clf.2013 <- annual.average(fred.cat.list$`Civilian Labor Force`, date = 2013)

### Residential Population (RP)
    rp.2013 <- annual.average(fred.cat.list$`Resident Population`, date = 2013)


# Merge Data --------------------------------------------------------------

    
agg.2013 <- merge(ur.2013, pcpi.2013, by = "CountyName") %>%
            rename(UnRate = Value.x, PCPI = Value.y) %>%
            merge(clf.2013, by = "CountyName") %>%
            merge(rp.2013, by = "CountyName") %>%
            rename(CLF = Value.x, RP = Value.y)


# Add Election Turnout Winner (Party) -------------------------------------

    
### Party winner (democrat or republican)
    PartyWinner <- sapply(seq_along(tex.results$RepShare), function(x){
        if(tex.results$RepShare[x] > tex.results$DemShare[x]){
            "Republican"
        } else{
            "Democrat"
        }
    })

### Democrat winner of each county
    Dems <- tex.results[15:17]
    DWinner <- max.col(Dems)
    DemWinner <- names(Dems)[DWinner]
### Republican winner of each county
    Reps <- tex.results[8:11]
    RWinner <- max.col(Reps)
    RepWinner <- names(Reps)[RWinner]

### Create new columns in agg.2013
    agg.2013$PartyWinner <- PartyWinner
    agg.2013$DemWinner <- DemWinner
    agg.2013$RepWinner <- RepWinner


# Looking at the data itself ----------------------------------------------

### UnRate
    UnRate.dist <- ggplot(data = agg.2013, mapping = aes(UnRate, fill = PartyWinner)) +
                   geom_histogram(aes(y = ..density..), bins = 50, alpha = .5) +
                   geom_density(mapping = aes(color = PartyWinner), alpha = .1, size = 1)
    UnRate.dist + facet_grid(. ~ PartyWinner)

### PCPI
    PCPI.dist <- ggplot(data = agg.2013, mapping = aes(PCPI)) +
                 geom_histogram(aes(y = ..density.., fill = ..count..), bins = 50) +
                 geom_density(alpha = .1, fill = "#7FDBFF") +
                 scale_fill_gradient("Count", low = "#001F3F", high = "#FF4136")
    PCPI.dist2 <- ggplot(data = agg.2013, mapping = aes(PCPI, fill = PartyWinner)) +
                  geom_histogram(aes(y = ..density..), bins = 50, alpha = .5) +
                  geom_density(mapping = aes(color = PartyWinner), alpha = .1, size = 1)
    PCPI.dist2
    PCPI.dist + facet_grid(. ~ PartyWinner)
    
### CLF
    CLF.dist <- ggplot(data = agg.2013, mapping = aes(CLF)) +
                geom_histogram(aes(y = ..density..), bins = 25) +
                geom_density(alpha = .1, fill = "#7FDBFF")
    CLF.dist + facet_grid(. ~ PartyWinner)
    
### RP
    RP.dist <- ggplot(data = agg.2013, mapping = aes(RP)) +
               geom_histogram(aes(y = ..density..), bins =  25) +
               geom_density(alpha = .1, fill = "#7FDBFF")
    RP.dist + facet_grid(. ~ PartyWinner)
    
### CLF_RP (CLF divided by RP)
    CLF_RP.dist <- ggplot(data = agg.2013, mapping = aes(((CLF/1000)/RP), fill = PartyWinner)) +
                   geom_histogram(aes(y = ..density..), bins = 50, alpha = .5) +
                   geom_density(mapping = aes(color = PartyWinner), alpha = .1, size = 1) +
                   scale_x_continuous(labels = percent)
    CLF_RP.dist + facet_grid(. ~ PartyWinner)

# Unemployment Rate vs Per Capita Personal Income -------------------------

### . ~ PartyWinner
plot.1.1 <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI)) +
            geom_point(alpha = 1/4) + geom_smooth() +
            ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
            labs(x = "Unemployment Rate", y = "Per Capita Personal Income") +
            facet_grid(. ~ PartyWinner)

plot.1.2 <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI, color = factor(PartyWinner))) +
            geom_point(alpha = 1/2) + geom_smooth() +
            ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
            labs(x = "Unemployment Rate", y = "Per Capita Personal Income")

### . ~ DemWinner
plot.2.1 <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI)) +
          geom_point(alpha = 1/4) + geom_smooth() +
          ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
          labs(x = "Unemployment Rate", y = "Per Capita Personal Income") +
          facet_grid(. ~ DemWinner)

plot.2.2 <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI, color = factor(DemWinner))) +
            geom_point(alpha = 1/3) + geom_smooth() +
            ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income\n Bernie Sanders vs Hillary Clinton") +
            labs(x = "Unemployment Rate", y = "Per Capita Personal Income") + 
            scale_color_manual("Candidates", labels = c("Bernie Sanders", "Hillary Clinton"), values = c("blue", "red"))

### . ~ RepWinner
plot.3.1 <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI)) +
          geom_point(alpha = 1/4) + geom_smooth() +
          ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
          labs(x = "Unemployment Rate", y = "Per Capita Personal Income") +
          facet_grid(. ~ RepWinner)

plot.3.2 <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI, color = factor(RepWinner))) +
            geom_point(alpha = 1/2) + geom_smooth() +
            ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
            labs(x = "Unemployment Rate", y = "Per Capita Personal Income")


# CLF, RP, UnRate, PCPI ---------------------------------------------------

ggplot(data = agg.2013, mapping = aes(UnRate, CLF_RP)) + geom_point()
ggplot(data = agg.2013, mapping = aes(PCPI, CLF_RP)) + geom_point()



# # Example -----------------------------------------------------------------
# 
# 
# # subset harris county
# harris.clfn <- fred.tables$`Civilian Labor Force` %>% select(Date, TXHARR1LFN)
# 
# # subset by a year
# this.year    <- ""
# year.summary <- harris.clfn[year(harris.clfn$Date) == this.year, ]
# 
# # summarize yearly data
# year.mean <- colMeans(year.summary[,2])
# 
# # loop every year
# years.to.loop <- year(harris.clfn$Date) %>% unique()
# lapply(seq_along(years.to.loop), function(x){
#     year.summary <- harris.clfn[year(harris.clfn$Date) == years.to.loop[x], ]
#     monthly.clfn <- as.numeric(as.character(year.summary[, 2]))
#     year.mean    <- mean(monthly.clfn)
#     data.frame(Year = years.to.loop[x], Harris = year.mean)
# }) %>% ldply()
# 
# # Actual FUN --------------------------------------------------------------
# 
# 
# # subset a table by a county
# county.subset <- function(table, county){
#     subseted <- table %>% select(Date, county)
#     return(subsetted)
# }
# 
# # subset a table by year
# year.subset <- function(table, year){
#     subsetted <- table[year(table$Date) == year, ]
#     return(subsetted)
# }
# 
#         # subset a table by every year
#                 # returns a list object
#                 # each list element corresponds to a year
#                 # each list element contains a data frame of yearly values
#         all.years.subset <- function(table){
#             table.subset  <- table %>% select(1)
#             years.to.loop <- table.subset[[1]] %>% year() %>% unique()
#             env.table     <- table
#             lapply(seq_along(years.to.loop), function(X, table = env.table){
#                 year.subset(table = table, year = years.to.loop[X])
#             })
#         }
# 
# # summarize yearly data
# mean.subset <- function(table){
#     val <- table[,-1] %>% as.character() %>% as.numeric()
#     subsetted <- mean(val)
#     return(subsetted)
# }
# 
#         # summarize a list of yearly data
#         all.years.annualized <- function(list){
#             lapply(seq_along(list), function(x){
#                 
#             })
#         }
# 
# # loop everything
# 
# annual.table.loop <- function(table){
# ## set pre-loop variables
#     columns.to.loop <- dim(table)[2] - 1
#     counties        <- table %>% select(-1) %>% names
#     years.to.subset <- table %>% select(1) %>% year() %>% unique()
#     
#     lapply(seq_along(columns.to.loop), function(x){
#         county.table <- county.subset(table = table, county = counties[x])
#         year.table   <- year.subset(county.table)
#     })
# }