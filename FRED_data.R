library(rvest)
library(plyr)


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
fred.data1 <- lapply(seq_along(fred.input1), function(x){
    
    f.data1[[x]] <- fred.series(key    = myapi,
                                id     = fred.input1[x],
                                filter = filter1
                                )
}) %>% ldply()
fred.data2 <- lapply(seq_along(fred.input2), function(x){
    
    f.data2[[x]] <- fred.series(key    = myapi,
                                id     = fred.input2[x],
                                filter = filter2
                                )
}) %>% ldply()


# Run the Data Function ---------------------------------------------------


fred.test <- lapply(seq_along(fred.data2$SeriesID), function(x){

    tryCatch({
        print(x)
        fred.data(myapi,fred.data2$SeriesID[x])
    }, error = function(e) {
        
        tryCatch({
            Sys.sleep(3.5)
            fred.data(myapi,fred.data2$SeriesID[x])
        }, error = function(e) {
            Sys.sleep(3.5)
            fred.data(myapi,fred.data2$SeriesID[x])
        })
    })
})

names(fred.test) <- fred.data2$SeriesID
