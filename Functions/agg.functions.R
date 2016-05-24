library(dplyr)
library(lubridate)

# annual.average() --------------------------------------------------------


# df example: df = fred.cat.list$`Unemployment Rate`
annual.average <- function(df, date = 2013){
    aa.vector <- sapply(seq_along(df), function(x, year = date){
            df.year <- df[[x]][year(df[[x]]$Date) == year, ]
            df.year.average <- df.year$Value %>%
            as.character.factor() %>%
            as.numeric() %>%
            mean()
    })
    load("~/R/TexasPrimary2016/Data/tx.county.Rdata")
    aa.df <- data.frame(CountyName = tx.county, Value = aa.vector)
    return(aa.df)
}


# annual.value() ----------------------------------------------------------


annual.value <- function(df, date = 2013){
    av.vector <- sapply(seq_along(df), function(x, year = date){
            df.year <- df[[x]][year(df[[x]]$Date) == year, ]
            df.year.value <- df.year$Value %>% 
            as.character.factor() %>%
            as.numeric()
    })
    load("~/R/TexasPrimary2016/Data/tx.county.Rdata")
    av.df <- data.frame(CountyName = tx.county, Value = av.vector)
    return(av.df)
}