# get.agg.names(cols, facet) ----------------------------------------------


    # get.agg.names <- function(cols, facet){
    #         plot.index <- list()
    #         dist.plot.cols <- which(colnames())
    #         dist.plot.facet <- grep(facet, colnames(agg.2013))
    #         plot.index <- list(dist.plot.cols, dist.plot.facet)
    #         names(plot.index) <- c("cols", "facet")
    #         return(plot.index)
    # }


# split.agg(cols, facets) -------------------------------------------------


split.agg <- function(data, facet = NULL){
        
        if(!is.null(facet) & (facet %in% names(data))){
            data2 <- data[, !names(data) %in% facet]
        } else{
            data2 <- data
        }
    
                data2.names <- names(data2)
    
        split.df <- function(x, facet){
            
                    # x.name <- substitute(x)
                    # facet.names <- substitute(facet)  ## if !is.null()
            
            shdw.list <- data.frame()        ## to hold data in loop
            shdw.list <- data[,c(x, facet)]
            colnames(shdw.list) <- c("Variable", facet)
            return(shdw.list)
        }
        
        agg.list <- lapply(data2.names, split.df, facet = facet)
        
        names(agg.list) <- data2.names              ## name it
        return(agg.list)
}

    
### create the function to loop through in mapply()
    dist.plot <- function(.data, x.name){
            ggplot(data = .data, mapping = aes(x = Variable, fill = Facet)) +
            
                xlab(x.name) +
            
            geom_histogram(aes(y = ..density..), bins =  50, alpha = .5) +
            geom_density(aes(color = Facet), alpha = .1) +
            
                party.colors.1 +
                party.colors.2 +
            
            facet_grid(. ~ Facet) +
            geom_vline(data = .data,
                       aes(xintercept = mean(Variable),linetype = "dashed"),
                       size = 1) +
            geom_vline(data = .data,
                       aes(xintercept = median(Variable), linetype="solid"),
                       size = 1) +
            scale_linetype_identity(guide="legend", label = c("Mean", "Median"))
    }

### run mapply() on the list of data frames
    distribution.plots <- mapply(FUN = dist.plot,
                             .data = splits,
                             x.name = names(splits),
                             SIMPLIFY = FALSE)
    # ### name the plots
    #     names(distribution.plots) <- cols
