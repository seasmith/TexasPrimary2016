fred.data <- function(key, series){
    
    root        <- "https://api.stlouisfed.org/fred/series/observations?series_id="
    cred        <- "&api_key="
    url         <- paste(root, series, cred, key, sep = "")
    
    html        <- url %>% read_html() %>% html_nodes("observation")
    date        <- html %>% html_attr("date")  %>% as.Date()
    value       <- html %>% html_attr("value") #%>% as.numeric()
    
    data.values <- data.frame("Date" = date,
                              "Value" = value
    )
}