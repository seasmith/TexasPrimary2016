# series.scraper(key, id, filters) ----------------------------------------


# This function will retrieve the meta data for each series within the specified release.
# You MUST enter a 'key' (your FRED API)
# You MUST enter an 'id' (a number corresponding to a release)
# You MAY choose to enter filters (character vector of 'tags' which will restrict the results)
# A data frame will be returned which can act as a master/top-level table

series.scraper <- function(key = NULL, id = NULL, filters = NULL){
    
    if(is.null(key)){
        stop("An API 'key' is required!")
    }
    if(is.null(id)){
        stop("You must input a character vector corresponding to a release's 'id'")
    }
    
    source("county.scraper.R")
    source("category.scraper.R")
    
    root    <- "https://api.stlouisfed.org/fred/release/series?release_id="
    cred    <- "&api_key="
    
    if(!is.character(id)) {
        id <- as.character(id)
    }
    if(!is.null(filters)){
        filter <- paste("&tag_names=",
                        paste(filters, collapse = ";"),
                        sep = ""
        )
        url    <- paste(root, id, cred, key, filter, sep = "")
    }   else {
        url    <- paste(root, id, cred, key, sep = "")
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
    data.info          <- data.frame("Release"     = release,
                                     "SeriesID"    = series_id,
                                     "Title"       = title,
                                     "Frequency"   = frequency,
                                     "Units"       = units,
                                     "Start"       = start,
                                     "End"         = end,
                                     "Adjustment"  = adjustment,
                                     "LastUpdated" = last_updated,
                                     "Blessed"     = blessed_start
    )
    data.info$CountyName <- county.scraper(title)
    data.info$Category   <- category.scraper(title)
    data.info
}


# obs.scraper(key, series) ------------------------------------------------


# This function will retrieve the 'observations' (the actual data that is inside a 'series')
# You MUST specify a 'key' (your API)
# You MUST specify a 'series' (alphanumeric sequence corresponding to a data table within a release)
# A data frame will be returned with the actual data (a column of dates and a column of 'series' values)

obs.scraper <- function(key = NULL, series = NULL){
    
    if(is.null(key)){
        stop("An API 'key' is required!")
    }
    if(is.null(series)){
        stop("You must input a character vector corresponding to a series' 'id'")
    }
    
    root        <- "https://api.stlouisfed.org/fred/series/observations?series_id="
    cred        <- "&api_key="
    url         <- paste(root, series, cred, key, sep = "")
    
    html        <- url %>% read_html() %>% html_nodes("observation")
    date        <- html %>% html_attr("date")  %>% as.Date()
    value       <- html %>% html_attr("value")
    value       <- ifelse(value == ".", NA, value) %>% as.numeric()
    data.values <- data.frame("Date" = date,
                              "Value" = value
    )
}


# county.scraper(string) --------------------------------------------------


county.scraper <- function(string){
    
    spaces.vector    <- gregexpr("\\s", string)
    spaceless.vector <- sapply(seq_along(spaces.vector), function(x){
        
        target.space1 <- length(spaces.vector[[x]]) - 2
        target.space2 <- length(spaces.vector[[x]]) - 1
        target.pos1   <- spaces.vector[[x]][target.space1] + 1
        target.pos2   <- spaces.vector[[x]][target.space2] - 1
        substr(string[[x]], target.pos1, target.pos2)
    })
    spaceless.vector
}


# category.scraper(string) ------------------------------------------------


category.scraper <- function(string){
    
    spaces.vector    <- gregexpr("\\s", string)
    spaceless.vector <- sapply(seq_along(spaces.vector), function(x){
        
        target.space1 <- length(spaces.vector[[x]]) - 3
        target.pos1   <- spaces.vector[[x]][target.space1] - 1
        substr(string[[x]], 1, target.pos1)
    })
    spaceless.vector
}


# cat.county.scraper(title.vector) ----------------------------------------


cat.county.scraper <- function(title.vector){
    
# load dependencies
require(dplyr, quietly = T)
require(choroplethrMaps, quietly = T)
data("county.regions")

# create vector of texas counties
tx.counties  <- filter(county.regions, state.name == "texas") %>%
                select("CountyName" = county.name)
# initialize the list to hold the lapply() results 
extracted    <- list()

# begin function
        cat.county <-lapply(seq_along(title.vector), function(x){
            
                # find county position
                county.found <- paste(tx.counties[[1]], collapse = "|") %>%
                    gregexpr(title.vector[x], ignore.case = TRUE)
                # extract from start to 2 spaces before the start of the county
                first.extract <- substr(x     = title.vector[x],
                                        start = 1,
                                        stop  = county.found[[1]][1] - 2)
                
                # extract the 'Category'
                        # find position of either 'in' or 'for' at the end of the string
                        end.found     <- gregexpr("in$|for$", first.extract)
                        # extract from start to 2 spaces before either 'in' or 'for'
                        Category      <- substr(x     = first.extract,
                                                start = 1,
                                                stop  = end.found[[1]][1] - 2)
                
                # extract the 'County'
                        # find the space before 'County'
                        county.found2 <- gregexpr(" County,", title.vector[x])
                        # extract from start of county name (county.found) to one space before the comma
                        CountyName    <- substr(x     = title.vector[x],
                                                start = county.found[[1]][1],
                                                stop  = county.found2[[1]][1] - 1)
                
                extracted[[x]] <- c(Category, CountyName)

            
            })
cat.county
}


# Unrealized --------------------------------------------------------------


#     
# incorrects     <- grep("in$|for$", uis2)
# sq             <- seq_along(uis2)
# corrects       <- sq[!(sq %in% incorrects)]
# new.fred.series2 <- lapply(incorrects, function(x){
#     fred.series2[fred.series2$Category == uis2[[x]], 'CountyName'] <- right.counties
#     fred.series2[fred.series2$Category == uis2[[x]], 'Category'] <- uis2[[1]]
#     fred.series2
# })
