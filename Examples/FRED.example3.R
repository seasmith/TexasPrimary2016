# Load Dependencies -------------------------------------------------------

library(plyr)
library(dplyr)
#library(lubridate) ## added to agg.function.R
library(ggplot2)
library(scales) ## for the percent object in scales_x_continuous()
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

    
agg.2013 <- left_join(ur.2013, pcpi.2013, by = "CountyName") %>%
            rename(UnRate = Value.x, PCPI = Value.y) %>%
            left_join(clf.2013, by = "CountyName") %>%
            left_join(rp.2013, by = "CountyName") %>%
            rename(CLF = Value.x, RP = Value.y) %>%
            mutate(CLF_RP = (CLF/RP)/1000)


# Add Election Turnout Winner ---------------------------------------------

    
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
    
### Join agg.2013 with tex.results
    agg.2013 <- left_join(agg.2013, tex.results, by = "CountyName")
    ### Create new directory and save aggregated data
        dir.create(file.path("Data", "Aggregated"), showWarnings = FALSE)
        wd <- getwd()
        save(agg.2013, file = file.path(wd, "Data", "Aggregated", "agg.2013.RData"))


# Plot formatting ---------------------------------------------------------

### Party colors (BLUE = Democratic; RED = Republican)
    party.colors.1 <- scale_color_manual(values = c("#0000FF", "#FF0000"))
    party.colors.2 <- scale_fill_manual(values = c("#0000FF", "#FF0000"))
    

# Distribution of Data ----------------------------------------------------

    # variables <- c("UnRate", "PCPI", "CLF", "RP", "CLF_RP")
    # 
    # distribution.plots <-  variables %>%
    #     lapply(., function(x){
    #         ggplot(data = agg.2013, mapping = aes(x, fill = PartyWinner))
    #     })
    # 
    # names(distribution.plots) <- variables
    # 
    # histogram.bins.50 <- geom_histogram(aes(y = ..density..), bins =  50, alpha = .5)
    # histogram.bins.25 <- geom_histogram(aes(y = ..density..), bins =  25, alpha = .5)
    # density           <- geom_density(aes(color = PartyWinner), alpha = .1)
    # facet.PartyWinner <- facet_grid(. ~ PartyWinner)    


### UnRate
    UnRate.dist <- ggplot(data = agg.2013, mapping = aes(UnRate, fill = PartyWinner)) +
                   geom_histogram(aes(y = ..density..), bins = 50, alpha = .5) +
                   geom_density(mapping = aes(color = PartyWinner), alpha = .1, size = 1) + 
                   party.colors.1 + party.colors.2
    UnRate.dist
    UnRate.dist + facet_grid(. ~ PartyWinner) +
                  geom_vline(data = agg.2013,
                             aes(xintercept=mean(UnRate),linetype="dashed")) +
                  geom_vline(data = agg.2013,
                             aes(xintercept=median(UnRate), linetype="solid")) +
                  scale_linetype_identity(guide="legend", label = c("Mean", "Median"))
    #+
                  # geom_text(aes(0,median(UnRate),label = paste("Median =",round(median(UnRate), 2))),
                  #           vjust = 0,
                  #           hjust = 0) +
                  # geom_text(aes(0,mean(UnRate),label = paste("Mean =",round(mean(UnRate), 2))),
                  #           vjust = 3,
                  #           hjust = 0)
### PCPI
    PCPI.dist <- ggplot(data = agg.2013, mapping = aes(PCPI, fill = PartyWinner)) +
                 geom_histogram(aes(y = ..density..), bins = 50, alpha = .5) +
                 geom_density(mapping = aes(color = PartyWinner), alpha = .1, size = 1) + 
                 party.colors.1 + party.colors.2
    PCPI.dist
    PCPI.dist + facet_grid(. ~ PartyWinner) +
                geom_vline(data = agg.2013,
                           aes(xintercept=mean(PCPI)),
                           linetype="dashed") +
                geom_vline(data = agg.2013,
                           aes(xintercept=median(PCPI)))
### CLF
    CLF.dist <- ggplot(data = agg.2013, mapping = aes(CLF, fill = PartyWinner)) +
                geom_histogram(aes(y = ..density..), bins = 25, alpha = .5) +
                geom_density(aes(color = PartyWinner), alpha = .1) +
                party.colors.1 + party.colors.2
    CLF.dist
    CLF.dist + facet_grid(. ~ PartyWinner) +
               geom_vline(data = agg.2013,
                          aes(xintercept=mean(CLF)),
                          linetype="dashed") +
               geom_vline(data = agg.2013,
                          aes(xintercept=median(CLF)))
    
### RP
    RP.dist <- ggplot(data = agg.2013, mapping = aes(RP, fill = PartyWinner)) +
               geom_histogram(aes(y = ..density..), bins =  25, alpha = .5) +
               geom_density(aes(color = PartyWinner), alpha = .1) +
               party.colors.1 + party.colors.2
    RP.dist
    RP.dist + facet_grid(. ~ PartyWinner) +
              geom_vline(data = agg.2013,
                         aes(xintercept=mean(RP)),
                         linetype="dashed") +
              geom_vline(data = agg.2013,
                         aes(xintercept=median(RP)))
    
### CLF_RP (CLF divided by RP)
    CLF_RP.dist <- ggplot(data = agg.2013, mapping = aes(((CLF/1000)/RP), fill = PartyWinner)) +
                   geom_histogram(aes(y = ..density..), bins = 50, alpha = .5) +
                   geom_density(mapping = aes(color = PartyWinner), alpha = .1, size = 1) +
                   scale_x_continuous(labels = percent) +
                   party.colors.1 + party.colors.2
    CLF_RP.dist
    CLF_RP.dist + facet_grid(. ~ PartyWinner) +
                  geom_vline(data = agg.2013,
                             aes(xintercept=mean(CLF_RP)),
                             linetype="dashed") +
                  geom_vline(data = agg.2013,
                             aes(xintercept=median(CLF_RP)))


# Scatterplot - UnRate ~ PCPI ---------------------------------------------


### Unfaceted
    UnRate.PCPI <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI)) +
                   geom_point(alpha = 1/4) + geom_smooth()
    UnRate.PCPI
    
### . ~ PartyWinner
    UnRate.PCPI.PartyWinner <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI, color = PartyWinner)) +
                               geom_point(alpha = 1/2) + geom_smooth() +
                               ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
                               labs(x = "Unemployment Rate", y = "Per Capita Personal Income") + 
                               party.colors.1 + party.colors.2
    UnRate.PCPI.PartyWinner
    UnRate.PCPI.PartyWinner + facet_grid(. ~ PartyWinner)
    
    
### . ~ DemWinner
    UnRate.PCPI.DemWinner <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI, color = factor(DemWinner))) +
                             geom_point(alpha = 1/3) + geom_smooth() +
                             ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income\n Bernie Sanders vs Hillary Clinton") +
                             labs(x = "Unemployment Rate", y = "Per Capita Personal Income") + 
                             scale_color_manual("Candidates", labels = c("Bernie Sanders", "Hillary Clinton"), values = c("blue", "red"))
    UnRate.PCPI.DemWinner
    UnRate.PCPI.DemWinner + facet_grid(. ~ DemWinner)
    
### . ~ RepWinner
    UnRate.PCPI.RepWinner <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = PCPI, color = factor(RepWinner))) +
                             geom_point(alpha = 1/2) + geom_smooth() +
                             ggtitle("Texas Counties 2013:\nUnemployment Rate vs Per Capita Personal Income") +
                             labs(x = "Unemployment Rate", y = "Per Capita Personal Income") +
                             scale_color_manual("Candidates", labels = c("Donald Trump", "Ted Cruz"), values = c("blue", "red"))
    UnRate.PCPI.RepWinner
    UnRate.PCPI.RepWinner + facet_grid(. ~ RepWinner)
    


# Scatterplot - UnRate ~ CLF_RP -------------------------------------------


### Unfaceted
    CLF_RP.UnRate <- ggplot(data = agg.2013, mapping = aes(UnRate, CLF_RP)) + geom_point(alpha = 1/4) + geom_smooth()
    CLF_RP.UnRate
    
### . ~ PartyWinner
    CLF_RP.UnRate.PartyWinner <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = CLF_RP, color = PartyWinner)) +
                                 geom_point(alpha = 1/2) + geom_smooth() +
                                 ggtitle("Texas Counties 2013:\nUnemployment Rate vs (Civilian Labor Force/Residential Population)") +
                                 labs(x = "Unemployment Rate", y = "(Civilian Labor Force/Residential Population)") + 
                                 party.colors.1 + party.colors.2
    CLF_RP.UnRate.PartyWinner
    CLF_RP.UnRate.PartyWinner + facet_grid(. ~ PartyWinner)

### . ~ DemWinner
    CLF_RP.UnRate.DemWinner <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = CLF_RP, color = DemWinner)) +
                               geom_point(alpha = 1/2) + geom_smooth() +
                               ggtitle("Texas Counties 2013:\nUnemployment Rate vs (Civilian Labor Force/Residential Population)") +
                               labs(x = "Unemployment Rate", y = "(Civilian Labor Force/Residential Population)") + 
                               scale_color_manual("Candidates", labels = c("Bernie Sanders", "Hillary Clinton"), values = c("blue", "red"))
    CLF_RP.UnRate.DemWinner
    CLF_RP.UnRate.DemWinner + facet_grid(. ~ DemWinner)

### . ~ RepWinner
    CLF_RP.UnRate.RepWinner <- ggplot(data = agg.2013, mapping = aes(x = UnRate, y = CLF_RP, color = RepWinner)) +
                               geom_point(alpha = 1/2) + geom_smooth() +
                               ggtitle("Texas Counties 2013:\nUnemployment Rate vs (Civilian Labor Force/Residential Population)") +
                               labs(x = "Unemployment Rate", y = "(Civilian Labor Force/Residential Population)") + 
                               scale_color_manual("Candidates", labels = c("Donald Trump", "Ted Cruz"), values = c("blue", "red"))
    CLF_RP.UnRate.RepWinner
    CLF_RP.UnRate.RepWinner + facet_grid(. ~ RepWinner)


# Scatterplot - PCPI ~ CLF_RP ---------------------------------------------


### Unfaceted
    CLF_RP.PCPI <- ggplot(data = agg.2013, mapping = aes(PCPI, CLF_RP)) + geom_point(alpha = 1/4) + geom_smooth()
    CLF_RP.PCPI

### . ~ PartyWinner
    CLF_RP.PCPI.PartyWinner <- ggplot(data = agg.2013, mapping = aes(x = PCPI, y = CLF_RP, color = PartyWinner)) +
                               geom_point(alpha = 1/2) + geom_smooth() +
                               ggtitle("Texas Counties 2013:\nPer Capita Personal Income vs (Civilian Labor Force/Residential Population)") +
                               labs(x = "Per Capita Personal Income", y = "(Civilian Labor Force/Residential Population)") + 
                               party.colors.1 + party.colors.2
    CLF_RP.PCPI.PartyWinner
    CLF_RP.PCPI.PartyWinner + facet_grid(. ~ PartyWinner)
    
### . ~ DemWinner
    CLF_RP.PCPI.DemWinner <- ggplot(data = agg.2013, mapping = aes(x = PCPI, y = CLF_RP, color = DemWinner)) +
                             geom_point(alpha = 1/2) + geom_smooth() +
                             ggtitle("Texas Counties 2013:\nPer Capita Personal Income vs (Civilian Labor Force/Residential Population)") +
                             labs(x = "Per Capita Personal Income", y = "(Civilian Labor Force/Residential Population)") + 
                             scale_color_manual("Candidates", labels = c("Bernie Sanders", "Hillary Clinton"), values = c("blue", "red"))
    CLF_RP.PCPI.DemWinner
    CLF_RP.PCPI.DemWinner + facet_grid(. ~ DemWinner)
    
### . ~ RepWinner
    CLF_RP.PCPI.RepWinner <- ggplot(data = agg.2013, mapping = aes(x = PCPI, y = CLF_RP, color = RepWinner)) +
                             geom_point(alpha = 1/2) + geom_smooth() +
                             ggtitle("Texas Counties 2013:\nPer Capita Personal Income vs (Civilian Labor Force/Residential Population)") +
                             labs(x = "Per Capita Personal Income", y = "(Civilian Labor Force/Residential Population)") + 
                             scale_color_manual("Candidates", labels = c("Donald Trump", "Ted Cruz"), values = c("blue", "red"))
    CLF_RP.PCPI.RepWinner
    CLF_RP.PCPI.RepWinner + facet_grid(. ~ RepWinner)