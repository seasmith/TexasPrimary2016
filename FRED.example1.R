library(rvest)
library(plyr)
source("fred.series.R")
source("fred.data.R")
# Release Examples Info ---------------------------------------------------


# 116 = Unemployment in States and Local Areas (all other areas) 
# 346 = Small Area Income and Poverty Estimates 
# 119 = Annual Estimates of the Population for Counties 
# 330 = American Community Survey 
# 175 = Local Area Personal Income 


# Run the Meta Data Function-----------------------------------------------
 
        # set first set of variables
        myapi <- "" # set your API here
        fred.input1 <- c("116", "119", "330", "175")
        filter1     <- c("tx", "county")
        f.data1     <- list()
        # set second set of variables
        fred.input2 <- c("346")
        filter2     <- c("tx", "county", "income")
        f.data2     <- list()

# run both sets of variables
fred.series1 <- lapply(seq_along(fred.input1), function(x){
    
    tryCatch({
        f.data1[[x]] <- fred.series(key    = myapi,
                                    id     = fred.input1[x],
                                    filter = filter1
                                    )
    }, error = function(e){
        Sys.sleep(3.5)
        f.data1[[x]] <- fred.series(key    = myapi,
                                    id     = fred.input1[x],
                                    filter = filter1
        )
    })
}) %>% ldply()

fred.series2 <- lapply(seq_along(fred.input2), function(x){
    
    tryCatch({
        f.data2[[x]] <- fred.series(key    = myapi,
                                    id     = fred.input2[x],
                                    filter = filter2
                                    )
    }, error = function(e){
        Sys.sleep(3.5)
        f.data2[[x]] <- fred.series(key    = myapi,
                                    id     = fred.input2[x],
                                    filter = filter2
        )
    })
}) %>% ldply()


# Clean up the Meta Data --------------------------------------------------


    right.counties <- c("Deaf Smith", "El Paso", "Fort Bend",
                        "Jeff Davis", "Jim Hogg", "Jim Wells",
                        "La Salle","Live Oak", "Palo Pinto",
                        "Red River", "San Augustine", "San Jacinto",
                        "San Patricio", "San Saba", "Tom Green",
                        "Val Verde", "Van Zandt")
    
    uis1           <- unique(fred.series1$Category)
    
fred.series1[fred.series1$Category == uis1[[3]], 'County'] <- right.counties
fred.series1[fred.series1$Category == uis1[[4]], 'County'] <- right.counties
fred.series1[fred.series1$Category == uis1[[6]], 'County'] <- right.counties
fred.series1[fred.series1$Category == uis1[[8]], 'County'] <- right.counties
fred.series1[fred.series1$Category == uis1[[10]], 'County'] <- right.counties
fred.series1[fred.series1$Category == uis1[[12]], 'County'] <- right.counties
fred.series1[fred.series1$Category == uis1[[14]], 'County'] <- right.counties

fred.series1[fred.series1$Category == uis1[[3]], 'Category'] <- uis1[[1]]
fred.series1[fred.series1$Category == uis1[[4]], 'Category'] <- uis1[[2]]
fred.series1[fred.series1$Category == uis1[[6]], 'Category'] <- uis1[[5]]
fred.series1[fred.series1$Category == uis1[[8]], 'Category'] <- uis1[[7]]
fred.series1[fred.series1$Category == uis1[[10]], 'Category'] <- uis1[[9]]
fred.series1[fred.series1$Category == uis1[[12]], 'Category'] <- uis1[[11]]
# fred.series1[fred.series1$Category == uis1[[14]], 'Category'] <- uis1[[13]]

#     uis2           <- unique(fred.series2$Category)
#     
# incorrects     <- grep("in$|for$", uis2)
# sq             <- seq_along(uis2)
# corrects       <- sq[!(sq %in% incorrects)]
# new.fred.series2 <- lapply(incorrects, function(x){
#     fred.series2[fred.series2$Category == uis2[[x]], 'County'] <- right.counties
#     fred.series2[fred.series2$Category == uis2[[x]], 'Category'] <- uis2[[1]]
#     fred.series2
# })
fred.series2[fred.series2$Category == uis2[[2]], 'County'] <- right.counties
fred.series2[fred.series2$Category == uis2[[4]], 'County'] <- right.counties
fred.series2[fred.series2$Category == uis2[[6]], 'County'] <- right.counties    

fred.series2[fred.series2$Category == uis2[[2]], 'Category'] <- uis2[[1]]
fred.series2[fred.series2$Category == uis2[[4]], 'Category'] <- uis2[[3]]
fred.series2[fred.series2$Category == uis2[[6]], 'Category'] <- uis2[[5]]


# Run the Data Function ---------------------------------------------------

#run 1
fred.obs1 <- lapply(seq_along(fred.series1$SeriesID), function(x){
    
    tryCatch({
        
        fred.data(myapi,fred.series1$SeriesID[x])
    }, error = function(e) {
        
        tryCatch({
            Sys.sleep(3.5)
            fred.data(myapi,fred.series1$SeriesID[x])
        }, error = function(e) {
            Sys.sleep(3.5)
            fred.data(myapi,fred.series1$SeriesID[x])
        })
    })
})

names(fred.obs1) <- fred.series1$SeriesID
save(fred.obs1, file = "fred.obs1.RData")

# run 2
fred.obs2 <- lapply(seq_along(fred.series2$SeriesID), function(x){
    
    tryCatch({
        
        fred.data(myapi,fred.series2$SeriesID[x])
    }, error = function(e) {
        
        tryCatch({
            Sys.sleep(3.5)
            fred.data(myapi,fred.series2$SeriesID[x])
        }, error = function(e) {
            Sys.sleep(3.5)
            fred.data(myapi,fred.series2$SeriesID[x])
        })
    })
})

names(fred.obs2) <- fred.series2$SeriesID
save(fred.obs2, file = "fred.obs2.RData")