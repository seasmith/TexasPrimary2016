# obs.catcher(series.table, obs.list, cat) --------------------------------


# This function is required to achieve the results in 'FRED.example2.R'

obs.catcher <- function(series.table, obs.list, cat){
    cat.series               <- series.table[series.table[, 'Category'] == cat, ]
    cat.series[, 'County']   <- tolower(cat.series[, 'County'])
    colnames(cat.series)[11] <- "CountyName"
    
    # Find all SeriesID matching the above filtered data's SeriesID
    cat.obs <- list()
    cat.obs <- obs.list[cat.series[ , 'SeriesID']]
}


# cat.info(series.table) --------------------------------------------------


# This is an optional function which allows you to explore the constraints on a 'Series's' data.
# Specifically it examines the 'Frequency', 'Start', 'End', and 'Units' of each 'Category'.
cat.info <- function(series.table){

    cat      <- unique(series.table$Category)
    infos    <- list()
    big.list <- lapply(seq_along(cat), function(x){
            release <- unique(series.table[series.table$Category == cat[[x]], 'Release'])
            freq    <- unique(series.table[series.table$Category == cat[[x]], 'Frequency'])
            start   <- unique(series.table[series.table$Category == cat[[x]], 'Start'])
            end     <- unique(series.table[series.table$Category == cat[[x]], 'End'])
            units   <- unique(series.table[series.table$Category == cat[[x]], 'Units'])
            infos[[x]] <- data.frame(release, freq, start, end, units)
    })
    names(big.list) <- cat
    df              <- big.list %>% ldply
    names(df)[1]    <- "category"
    df
}


# cat.tabler(obs.list) ----------------------------------------------------


cat.tabler <- function(obs.list){
    x <- 1
    while(x <= length(obs.list)){
        if(x == 1){
            main.frame           <- data.frame()
            main.frame           <- obs.list[[x]]
            names(main.frame)[2] <- names(obs.list)[1]
            main.frame
        }   else {
            main.frame               <- merge(main.frame, obs.list[[x]], by = "Date")
            names(main.frame)[x + 1] <- names(obs.list)[x]
            main.frame
        }
        x = x + 1
        main.frame
    }
    main.frame
}

# # tranpose, create row names column, change column headers, and assign data types 
# main.frame2           <- t(main.frame)
# colnames(main.frame2) <- main.frame2[1, ]
# main.frame2           <- main.frame2[-1, ]


# Other -------------------------------------------------------------------


# # Find 2014 values (last row)
# loop.list <- list()
# pcpi.obs  <- lapply(seq_along(pcpi.obs), function(x){
#     
#     to.get    <- dim(pcpi.obs[[x]])[1]
#     loop.df   <- data.frame(SeriesID = names(pcpi.obs[x]),
#                             PCPI     = pcpi.obs[[x]][to.get, 2])
#     loop.list[[x]] <- loop.df
# }) %>% ldply

# # good effort here
# # it does work
# merged <- Reduce(function(...) merge(..., all = T, by = "Date"), pcpi.obs)