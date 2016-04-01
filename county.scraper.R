county.scraper <- function(string){
    
    spaces.vector    <- gregexpr("\\s", string)
    spaceless.vector <- sapply(seq_along(spaces.vector), function(x){
        
        target.space1 <- length(spaces.vector[[x]]) - 2
        target.space2 <- length(spaces.vector[[x]]) - 1
        target.pos1   <- spaces.vector[[x]][target.space1] + 1 %>% as.integer()
        target.pos2   <- spaces.vector[[x]][target.space2] - 1 %>% as.integer()
        pie <- substr(string[[x]], target.pos1, target.pos2)
        print(pie)
    })
}
