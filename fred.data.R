# This function will retrieve the 'observations' (the actual data that is inside a 'series')
# You MUST specify a 'key' (your API)
# You MUST specify a 'series' (alphanumeric sequence corresponding to a data table within a release)
# A data frame will be returned with the actual data (a column of dates and a column of 'series' values)

fred.data <- function(key, series){
    
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