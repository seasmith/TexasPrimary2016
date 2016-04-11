
# cat.info() --------------------------------------------------------------

cat.info <- function(series.table){

### one swift succession of code
    summarized.series <- fred.series %>% select(2,3,6,7,8,9) %>%
                         aggregate(list(series.table$Category), unique) %>% select(3,2,4:7) %>%
                         arrange(Release, Category)
}


# obs.catcher(series.table, obs.list, cat) --------------------------------


# This function will retrieve the observations for the specified category
# It returns a list of data frames
# Arguments:
    # series.table = use fred.series
    # obs.list     = use fred.obs
    # cat          = character string corresponding to a category
# Use the result of this function as the argument for cat.tabler()

obs.catcher <- function(series.table, obs.list, cat){
    
### subset by category
    cat.series               <- series.table[series.table$Category == cat, ]
    
### Find all SeriesID matching the subset's SeriesID
    cat.obs <- list()
    cat.obs <- obs.list[cat.series$SeriesID]
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