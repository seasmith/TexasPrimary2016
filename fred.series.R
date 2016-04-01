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
    
    html               <- url %>% read_html()
    series             <- html %>% html_nodes("series")
    series_id          <- series %>% html_attr("id")                        %>% as.character()
    title              <- series %>% html_attr("title")                     %>% as.character()
    frequency          <- series %>% html_attr("frequency")                 %>% as.character()
    start              <- series %>% html_attr("observation_start")         %>% as.Date()
    end                <- series %>% html_attr("observation_end")           %>% as.Date()
    units              <- series %>% html_attr("units")                     %>% as.character()
    adjustment         <- series %>% html_attr("seasonal_adjustment_short") %>% as.character()
    last_updated       <- series %>% html_attr("last_updated")              %>% as.Date()
    blessed_start      <- series %>% html_attr("realtime_start")            %>% as.Date()
    blessed_end        <- series %>% html_attr("realtime_end")              %>% as.Date()
    release            <- rep(id, length(series))                           %>% as.numeric()
    data.info          <- data.frame("Release" = release,
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
    data.info$County   <- county.scraper(title)
    data.info$Category <- category.scraper(title)
}