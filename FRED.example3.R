
# Example -----------------------------------------------------------------


# subset harris county
harris.clfn <- fred.tables$`Civilian Labor Force` %>% select(Date, TXHARR1LFN)

# subset by a year
this.year    <- ""
year.summary <- harris.clfn[year(harris.clfn$Date) == this.year, ]

# summarize yearly data
year.mean <- colMeans(year.summary[,2])

# loop every year
years.to.loop <- year(harris.clfn$Date) %>% unique()
lapply(seq_along(years.to.loop), function(x){
    year.summary <- harris.clfn[year(harris.clfn$Date) == years.to.loop[x], ]
    monthly.clfn <- as.numeric(as.character(year.summary[, 2]))
    year.mean    <- mean(monthly.clfn)
    data.frame(Year = years.to.loop[x], Harris = year.mean)
}) %>% ldply()



# Actual FUN --------------------------------------------------------------


# subset a table by a county
county.subset <- function(table, county){
    subseted <- table %>% select(Date, county)
    return(subsetted)
}

# subset a table by year
year.subset <- function(table, year){
    subsetted <- table[year(table$Date) == year, ]
    return(subsetted)
}

        # subset a table by every year
                # returns a list object
                # each list element corresponds to a year
                # each list element contains a data frame of yearly values
        all.years.subset <- function(table){
            table.subset  <- table %>% select(1)
            years.to.loop <- table.subset[[1]] %>% year() %>% unique()
            env.table     <- table
            lapply(seq_along(years.to.loop), function(X, table = env.table){
                year.subset(table = table, year = years.to.loop[X])
            })
        }

# summarize yearly data
mean.subset <- function(table){
    val <- table[,-1] %>% as.character() %>% as.numeric()
    subsetted <- mean(val)
    return(subsetted)
}

        # summarize a list of yearly data
        all.years.annualized <- function(list){
            lapply(seq_along(list), function(x){
                
            })
        }

# loop everything

annual.table.loop <- function(table){
## set pre-loop variables
    columns.to.loop <- dim(table)[2] - 1
    counties        <- table %>% select(-1) %>% names
    years.to.subset <- table %>% select(1) %>% year() %>% unique()
    
    lapply(seq_along(columns.to.loop), function(x){
        county.table <- county.subset(table = table, county = counties[x])
        year.table   <- year.subset(county.table)
    })
}