library(rvest)
library(plyr)
source("scraper.functions.R")

# Release Examples Info ---------------------------------------------------


# 116 = Unemployment in States and Local Areas (all other areas) 
# 346 = Small Area Income and Poverty Estimates 
# 119 = Annual Estimates of the Population for Counties 
# 330 = American Community Survey 
# 175 = Local Area Personal Income 


# Run the Meta Data Function-----------------------------------------------
 
    ## set 1st set of variables
        myapi <- "" # set your API here
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

# save it
    save(fred.series1, file = "fred.series1.RData")

    ## set 2nd set of variables
        fred.input2 <- c("346")
        filter2     <- c("tx", "county", "income")
        f.data2     <- list()

## run the 2nd set
    fred.series2 <- lapply(seq_along(fred.input2), function(x){
        
        tryCatch({
            f.data2[[x]] <- series.scraper(key    = myapi,
                                        id     = fred.input2[x],
                                        filter = filter2
                                        )
        }, error = function(e){
            Sys.sleep(3.5)
            f.data2[[x]] <- series.scraper(key    = myapi,
                                        id     = fred.input2[x],
                                        filter = filter2
            )
        })
    }) %>% ldply()

# save it
    save(fred.series2, file = "fred.series2.RData")

# Clean up the Meta Data --------------------------------------------------

#     ## get the right county names
#     right.counties <- c("Deaf Smith", "El Paso", "Fort Bend",
#                         "Jeff Davis", "Jim Hogg", "Jim Wells",
#                         "La Salle","Live Oak", "Palo Pinto",
#                         "Red River", "San Augustine", "San Jacinto",
#                         "San Patricio", "San Saba", "Tom Green",
#                         "Val Verde", "Van Zandt")
# 
# ## 1st series unique 'Category' values
# uis1           <- unique(fred.series1$Category)
#     
# # clean up 'CountyName' and 'Category'
# fred.series1[fred.series1$Category == uis1[[3]], 'CountyName'] <- right.counties
# fred.series1[fred.series1$Category == uis1[[4]], 'CountyName'] <- right.counties
# fred.series1[fred.series1$Category == uis1[[6]], 'CountyName'] <- right.counties
# fred.series1[fred.series1$Category == uis1[[8]], 'CountyName'] <- right.counties
# fred.series1[fred.series1$Category == uis1[[10]], 'CountyName'] <- right.counties
# fred.series1[fred.series1$Category == uis1[[12]], 'CountyName'] <- right.counties
# fred.series1[fred.series1$Category == uis1[[14]], 'CountyName'] <- right.counties
# 
# fred.series1[fred.series1$Category == uis1[[3]], 'Category'] <- uis1[[1]]
# fred.series1[fred.series1$Category == uis1[[4]], 'Category'] <- uis1[[2]]
# fred.series1[fred.series1$Category == uis1[[6]], 'Category'] <- uis1[[5]]
# fred.series1[fred.series1$Category == uis1[[8]], 'Category'] <- uis1[[7]]
# fred.series1[fred.series1$Category == uis1[[10]], 'Category'] <- uis1[[9]]
# fred.series1[fred.series1$Category == uis1[[12]], 'Category'] <- uis1[[11]]
# 
#     # save a R object
#     save(fred.series1, file = "fred.series1.RData")
# 
# 
# ## 2nd series unique 'Category' names
# uis2           <- unique(fred.series2$Category)
# 
# # clean up 'CountyName' and 'Category'
# fred.series2[fred.series2$Category == uis2[[2]], 'CountyName'] <- right.counties
# fred.series2[fred.series2$Category == uis2[[4]], 'CountyName'] <- right.counties
# fred.series2[fred.series2$Category == uis2[[6]], 'CountyName'] <- right.counties    
# 
# fred.series2[fred.series2$Category == uis2[[2]], 'Category'] <- uis2[[1]]
# fred.series2[fred.series2$Category == uis2[[4]], 'Category'] <- uis2[[3]]
# fred.series2[fred.series2$Category == uis2[[6]], 'Category'] <- uis2[[5]]
# 
#     # save a R object
#     save(fred.series1, file = "fred.series1.RData")


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
# give it names and save as R object
    names(fred.obs1) <- fred.series1$SeriesID
    save(fred.obs1, file = "fred.obs1.RData")

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
# git is names and save as R object
    names(fred.obs2) <- fred.series2$SeriesID
    save(fred.obs2, file = "fred.obs2.RData")