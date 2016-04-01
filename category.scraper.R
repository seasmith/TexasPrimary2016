category.scraper <- function(string){

        spaces.vector    <- gregexpr("\\s", string)
        spaceless.vector <- sapply(seq_along(spaces.vector), function(x){
            
            target.space1 <- length(spaces.vector[[x]]) - 3
            target.pos1   <- spaces.vector[[x]][target.space1] - 1
            substr(string[[x]], 1, target.pos1)
        })
        spaceless.vector
}