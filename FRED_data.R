library(rvest)
library(plyr)

# Meta Data Function ------------------------------------------------------

# fred.series <- function(key = NULL, id = NULL, filters = NULL){
#         
#         if(is.null(key)){
#             stop("An API 'key' is required!")
#         }
#         if(is.null(id)){
#             stop("You must input a character vector corresponding to a release's 'id'")
#         }
#         root    <- "https://api.stlouisfed.org/fred/release/series?release_id="
#         cred    <- "&api_key="
#         if(!is.null(filters)){
#             filter  <- paste("&tag_names=",
#                              paste(filters, collapse = ";"),
#                              sep = ""
#                              )
#              url    <- paste(root, id, cred, key, filter, sep = "")
#         
#         }   else {
#                 url <- paste(root, id, cred, key, sep = "")
#         }
#     
#     html               <- url %>% read_html()
#     series             <- html %>% html_nodes("series")
#     series_id          <- series %>% html_attr("id")                        %>% as.character()
#     title              <- series %>% html_attr("title")                     %>% as.character()
#     frequency          <- series %>% html_attr("frequency")                 %>% as.character()
#     start              <- series %>% html_attr("observation_start")         %>% as.Date()
#     end                <- series %>% html_attr("observation_end")           %>% as.Date()
#     units              <- series %>% html_attr("units")                     %>% as.character()
#     adjustment         <- series %>% html_attr("seasonal_adjustment_short") %>% as.character()
#     last_updated       <- series %>% html_attr("last_updated")              %>% as.Date()
#     blessed_start      <- series %>% html_attr("realtime_start")            %>% as.Date()
#     blessed_end        <- series %>% html_attr("realtime_end")              %>% as.Date()
#     release            <- rep(id, length(series))                           %>% as.numeric()
#     data.info          <- data.frame("Release" = release,
#                                      "SeriesID" = series_id,
#                                      "Title" = title,
#                                      "Frequency" = frequency,
#                                      "Units" = units,
#                                      "Start" = start,
#                                      "End" = end,
#                                      "Adjustment" = adjustment,
#                                      "LastUpdated" = last_updated,
#                                      "Blessed" = blessed_start
#                                      )
#     data.info$County   <- county.scraper(title)
#     data.info$Category <- category.scraper(title)
# }

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



# Run the Meta Data Function-----------------------------------------------
 
myapi <- "" # set your API here
fred.input1 <- c("116", "119", "330", "175")
filter1     <- c("tx", "county")
fred.input2 <- c("346")
filter2     <- c("tx", "county", "income")

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


# Data Function -----------------------------------------------------------

# fred.data <- function(key, series){
#     
#     root        <- "https://api.stlouisfed.org/fred/series/observations?series_id="
#     cred        <- "&api_key="
#     url         <- paste(root, series, cred, key, sep = "")
#     html        <- url %>%
#                    read_html() %>%
#                    html_nodes("observation")
#     date        <- html %>% html_attr("date")  %>% as.Date()
#     value       <- html %>% html_attr("value") #%>% as.numeric()
#     data.values <- data.frame("Date" = date,
#                               "Value" = value
#                               )
# }


# Test Run Data Function --------------------------------------------------

fred.test <- lapply(seq_along(fred.data2$SeriesID), function(x){

    tryCatch({
        print(x)
        fred.data(myapi,fred.data2$SeriesID[x])
    }, error = function(e) {
        
        tryCatch({
            Sys.sleep(3.5)
            # print(paste("Second try", x))
            fred.data(myapi,fred.data2$SeriesID[x])
        }, error = function(e) {
            Sys.sleep(3.5)
            # print(paste("Third try", x))
            fred.data(myapi,fred.data2$SeriesID[x])
        })
    })
})

names(fred.test) <- fred.data2$SeriesID
