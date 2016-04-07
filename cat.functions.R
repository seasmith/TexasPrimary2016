# obs.catcher(series.table, obs.list, cat) --------------------------------


# This function will retrieve the observations for the specified category
# It returns a list of data frames
# Arguments:
    # series.table = use fred.series
    # obs.list     = use fred.obs
    # cat          = character string corresponding to a category
# Use the result of this function as the argument for cat.tabler()
# cat.tabler() will create a table from all the data frames

obs.catcher <- function(series.table, obs.list, cat){
    cat.series               <- series.table[series.table$Category == cat, ]
    cat.series$CountyName   <- tolower(cat.series$CountyName)
    
    # Find all SeriesID matching the above filtered data's SeriesID
    cat.obs <- list()
    cat.obs <- obs.list[cat.series$SeriesID]
}


# cat.info(series.table) --------------------------------------------------


# This function will create a 'master/top-level' table
# Specifically it examines the 'Frequency', 'Start', 'End', and 'Units' of each 'Category'.
cat.info <- function(series.table){
    require(plyr, quietly = T)
    require(dplyr, quietly = T)
    cat      <- unique(series.table$Category)
    infos    <- list()
    big.list <- lapply(seq_along(cat), function(x){
            release <- unique( series.table$Release[   series.table$Category == cat[[x]]])
            freq    <- unique( series.table$Frequency[ series.table$Category == cat[[x]]])
            start   <- unique( series.table$Start[     series.table$Category == cat[[x]]])
            end     <- unique( series.table$End[       series.table$Category == cat[[x]]])
            units   <- unique( series.table$Units[     series.table$Category == cat[[x]]])
            infos[[x]] <- data.frame(release, freq, start, end, units)
    })
    names(big.list) <- cat
    df              <- big.list %>% ldply()
    names(df)[1]    <- "category"
    df
}


# cat.tabler(obs.list) ----------------------------------------------------


# This function will place the 254 observations into a single table

cat.tabler <- function(obs.list){
    x <- 1
    while(x <= length(obs.list)){
        if(x == 1){
            main.frame           <- data.frame() # initialize the data frame
            main.frame           <- obs.list[[x]] # add the first list object
            names(main.frame)[2] <- names(obs.list)[1] # give that object a name
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