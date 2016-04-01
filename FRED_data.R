library(rvest)

# set your API here 
myapi <- ""

# Meta Data Function ------------------------------------------------------

fred.series <- function(key = NULL, id = NULL, filters = NULL){
        
        if(is.null(key)){
            stop("An API 'key' is required!")
        }
        if(is.null(id)){
            stop("You must input a character vector corresponding to a release's 'id'")
        }
        root    <- "https://api.stlouisfed.org/fred/release/series?release_id="
        cred    <- "&api_key="
        if(!is.null(filters)){
            filter  <- paste("&tag_names=",
                             paste(filters, collapse = ";"),
                             sep = ""
                             )
             url    <- paste(root, id, cred, key, filter, sep = "")
        
        }   else {
                url <- paste(root, id, cred, key, sep = "")
        }
    
    html           <- url %>% read_html()
    series         <- html %>% html_nodes("series")
    series_id      <- series %>% html_attr("id")
    title          <- series %>% html_attr("title")
    frequency      <- series %>% html_attr("frequency")
    start          <- series %>% html_attr("observation_start")
    end            <- series %>% html_attr("observation_end")
    units          <- series %>% html_attr("units")
    adjustment     <- series %>% html_attr("seasonal_adjustment_short")
    last_updated   <- series %>% html_attr("last_updated")
    blessed_start  <- series %>% html_attr("realtime_start")
    blessed_end    <- series %>% html_attr("realtime_end")
    data.info      <- data.frame("Release" = rep(id, length(series)),
                                 "SeriesID" = series_id,
                                 "Title" = title,
                                 "Frequency" = frequency,
                                 "Units" = units,
                                 "Start" = start,
                                 "End" = end,
                                 "Adjustment" = adjustment,
                                 "LastUpdated" = last_updated,
                                 "Blessed" = blessed_start
                                 )
}

# Meta Data Inputs --------------------------------------------------------

# 116 = Unemployment in States and Local Areas (all other areas) ----------
# 
# 346 = Small Area Income and Poverty Estimates ---------------------------
# 
# 119 = Annual Estimates of the Population for Counties -------------------
# 
# 330 = American Community Survey -----------------------------------------
# 
# 175 = Local Area Personal Income ----------------------------------------

fred.input1 <- c("116", "119", "330", "175")
filter1     <- c("tx", "county")
fred.input2 <- c("346")
filter2     <- c("tx", "county", "income")

# Run the Meta Data Function-----------------------------------------------

f.data1    <- list()
fred.data1 <- lapply(seq_along(fred.input1), function(x){
    
    f.data1[[x]] <- fred.series(key    = myapi,
                                id     = fred.input1[x],
                                filter = filter1
                                )
}) %>% ldply()

f.data2    <- list()
fred.data2 <- lapply(seq_along(fred.input2), function(x){
    
    f.data2[[x]] <- fred.series(key    = myapi,
                                id     = fred.input2[x],
                                filter = filter2
                                )
}) %>% ldply()