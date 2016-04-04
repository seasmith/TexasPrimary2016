cat.catcher <- function(series.table, cat){
    cat.series               <- series.table[series.table$Category == cat, ]
    cat.series$County        <- tolower(cat.series$County)
    tx.regions                <- filter(county.regions, state.name == "texas") %>%
        select(region, "CountyName" = county.name)
    colnames(cat.series)[11] <- "CountyName"
    
    # Find all SeriesID matching the above filtered data's SeriesID
    cat.obs  <- fred.obs1[names(fred.obs1) == cat.series$SeriesID]
}

cat.info <- function(series.table){

    cat      <- unique(series.table$Category)
    infos    <- list()
    big.list <- lapply(seq_along(cat), function(x){
            freq  <- unique(series.table[series.table$Category == cat[[x]], 'Frequency'])
            start <- unique(series.table[series.table$Category == cat[[x]], 'Start'])
            end   <- unique(series.table[series.table$Category == cat[[x]], 'End'])
            units <- unique(series.table[series.table$Category == cat[[x]], 'Units'])
            infos[[x]] <- data.frame(freq, start, end, units)
    })
    names(big.list) <- cat
    big.list
}