x.vars <- c("UnRate", "PCPI", "CLF", "RP", "CLF_RP")
facet.var <- c("PartyWinner")
x.vars.search <- paste("^", x.vars, "$", sep = "", collapse = "|")
dist.plot.x.vars <- grep(x.vars.search, colnames(agg.2013))
dist.plot.facet.var <- grep(facet.var, colnames(agg.2013))

        ### .data = data frame
        ### .x = column/variable
        ### .facet = column/variable to facet by (i.e. PartyWinner)
        make.dist.plot <- function(.data, .x){
            expression({}
            party.colors.1 <- scale_color_manual(values = c("#0000FF", "#FF0000"))
            party.colors.2 <- scale_fill_manual(values = c("#0000FF", "#FF0000"))
            
            ggplot(data = .data,
                   mapping = aes(x = .data[.x],
                                 fill = PartyWinner)) +
            geom_histogram(aes(y = ..density..), bins =  50, alpha = .5) +
            geom_density(aes(color = PartyWinner), alpha = .1) +
                party.colors.1 +
                party.colors.2 +
                facet_grid(. ~ PartyWinner)
            })
            # scale_color_manual(values = c("#0000FF", "#FF0000")) +
            # scale_fill_manual(values = c("#0000FF", "#FF0000")) +
            # facet_grid(. ~ .data[.facet])
        }
distribution.plots <- lapply(X = dist.plot.x.vars,
                             FUN = function(x = X, y = y, z = z){
                                   make.dist.plot(.data = y,
                                                  .x = x,
                                                  .facet = z)},
                             y = agg.2013,
                             z = dist.plot.facet.var)
# distribution.plots <- mapply(make.dist.plot,
#                              .data = agg.2013,
#                              .x = c(UnRate, PCPI, CLF, RP, CLF_RP),
#                              .facet = .facet.variable)

    names(distribution.plots) <- x.vars
    
    histogram.bins.50 <- geom_histogram(aes(y = ..density..), bins =  50, alpha = .5)
    histogram.bins.25 <- geom_histogram(aes(y = ..density..), bins =  25, alpha = .5)
    density           <- geom_density(aes(color = PartyWinner), alpha = .1)
    facet.PartyWinner <- facet_grid(. ~ PartyWinner)

histogram.density <- expression(geom_histogram(aes(y = ..density..),
                                    bins = 50,
                                    alpha = .5) +
                     geom_density(mapping = aes(color = PartyWinner),
                                  alpha = .1,
                                  size = 1) + 
                     party.colors.1 +
                     party.colors.2)