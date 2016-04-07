
# Load Dependencies -------------------------------------------------------


library(rvest)
library(plyr)
library(dplyr)
source("scraper.functions.R")
myapi <- "" # set your API here


# Release Examples Info ---------------------------------------------------


# 116 = Unemployment in States and Local Areas (all other areas) 
# 346 = Small Area Income and Poverty Estimates 
# 119 = Annual Estimates of the Population for Counties 
# 330 = American Community Survey 
# 175 = Local Area Personal Income 


# Run the Meta Data Function-----------------------------------------------
 
    ## set 1st set of variables
        
        fred.input1 <- c("116", "119", "330", "175")
        filter1     <- c("tx", "county")
        f.data1     <- list()
        
## run the 1st set
    fred.series1 <- lapply(seq_along(fred.input1), function(x){
        
        tryCatch({
            f.data1[[x]] <- series.scraper(key    = myapi,
                                           id     = fred.input1[x],
                                           filter = filter1)
        }, error = function(e){
            Sys.sleep(3.5)
            f.data1[[x]] <- series.scraper(key    = myapi,
                                           id     = fred.input1[x],
                                           filter = filter1)
        })
    }) %>% ldply()

    ## set 2nd set of variables
        fred.input2 <- c("346")
        filter2     <- c("tx", "county", "income")
        f.data2     <- list()

## run the 2nd set
    fred.series2 <- lapply(seq_along(fred.input2), function(x){
        
        tryCatch({
            f.data2[[x]] <- series.scraper(key = myapi,
                                        id     = fred.input2[x],
                                        filter = filter2)
        }, error = function(e){
            Sys.sleep(3.5)
            f.data2[[x]] <- series.scraper(key = myapi,
                                        id     = fred.input2[x],
                                        filter = filter2)
        })
    }) %>% ldply()

## combine the two data frames and save
    fred.series <- rbind(fred.series1, fred.series2) %>%
                   arrange(Release, Category, CountyName)
    save(fred.series, file = "Data/fred.series.RData")


# Run the Data Function ---------------------------------------------------

## 1st series
    fred.obs1 <- lapply(seq_along(fred.series1$SeriesID), function(x){
        
        tryCatch({
            
           obs.scraper(myapi,fred.series1$SeriesID[x])
        }, error = function(e) {
            
            tryCatch({
                Sys.sleep(3.5)
                obs.scraper(myapi,fred.series1$SeriesID[x])
            }, error = function(e) {
                Sys.sleep(3.5)
                obs.scraper(myapi,fred.series1$SeriesID[x])
            })
        })
    })
# give names
    names(fred.obs1) <- fred.series1$SeriesID

## 2nd series
    fred.obs2 <- lapply(seq_along(fred.series2$SeriesID), function(x){
        
        tryCatch({
            
           obs.scraper(myapi,fred.series2$SeriesID[x])
        }, error = function(e) {
            
            tryCatch({
                Sys.sleep(3.5)
                obs.scraper(myapi,fred.series2$SeriesID[x])
            }, error = function(e) {
                Sys.sleep(3.5)
                obs.scraper(myapi,fred.series2$SeriesID[x])
            })
        })
    })
# give names
    names(fred.obs2) <- fred.series2$SeriesID

## concatenate fred.obs[x] and save as one R object
    fred.obs <- c(fred.obs1, fred.obs2)
    save(fred.obs, file = "Data/fred.obs.RData")
