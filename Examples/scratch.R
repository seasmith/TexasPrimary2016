### create variables for splitting the data frame into a list
    x.vars <- c("UnRate", "PCPI", "CLF", "RP", "CLF_RP")
    facet.var <- c("PartyWinner")
    x.vars.search <- paste("^", x.vars, "$", sep = "", collapse = "|")
    dist.plot.x.vars <- grep(x.vars.search, colnames(agg.2013))
    dist.plot.facet.var <- grep(facet.var, colnames(agg.2013))

### split the data frame
    .df <- lapply(dist.plot.x.vars, function(x){
        agg.2013[,c(1, x, 7)]
    })
    ### name the columns of the data frames
        .new.df <- lapply(seq_along(.df), function(x){
            colnames(.df[[x]]) <- c("CountyName", "Variable", "PartyWinner")
            return(.df[[x]])
        })
    ### name the data frames in the list
        names(.new.df) <- x.vars
    
### create layers and facets
    histogram.bins.50 <- geom_histogram(aes(y = ..density..), bins =  50, alpha = .5)
    histogram.bins.25 <- geom_histogram(aes(y = ..density..), bins =  25, alpha = .5)
    density           <- geom_density(aes(color = PartyWinner), alpha = .1)
    facet.PartyWinner <- facet_grid(. ~ PartyWinner)

### create the function to loop through in mapply()
    make.dist.plot <- function(.data, x.name){
        ggplot(data = .data,mapping = aes(x = Variable, fill = PartyWinner)) +
        xlab(x.name) +
        histogram.bins.50 +
        density +
        party.colors.1 +
        party.colors.2 +
        facet.PartyWinner
    }

### run mapply() on the list of data frames
    distribution.plots <- mapply(FUN = make.dist.plot,
                             .data = .new.df,
                             x.name = names(.new.df),
                             SIMPLIFY = FALSE)
    ### name the plots
        names(distribution.plots) <- x.vars
    
