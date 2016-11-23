
# Load Dependencies -------------------------------------------------------


library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(knitr)

df <- list()
plots <- list()

# Individual Republican Maps ----------------------------------------------

df$R <- lapply(select(tex.results, TurnOutR:dt), function(x) {
    data.frame(region = tex.results$region, value = x, stringsAsFactors = FALSE)
})

plots$R <- lapply(df$R, function(x) {
    county_choropleth(df = x,
                      state_zoom = "texas",
                      legend = "%",
                      num_colors = 1) +
        coord_map()
})

plots$R <- Map(function(x, y) x + ggtitle(y),
              x = plots$R,
              y = c("Turn Out: Republican", names(select(tex.results, Bush:Trump))))


# Individual Democrat Maps ------------------------------------------------


df$D <- lapply(select(tex.results, TurnOutD:ww), function(x) {
    data.frame(region = tex.results$region, value = x, stringsAsFactors = FALSE)
})

plots$D <- lapply(df$D, function(x) {
    county_choropleth(x, state_zoom = "texas", legend = "%", num_colors = 1) + coord_map()
})

plots$D <- Map(function(x, y) x + ggtitle(y),
              x = plots$D,
              y = c("Turn Out Democrat", names(select(tex.results, Clinton:Wilson))))


# Party Comparison Maps ---------------------------------------------------

# Who won?
df$party.turnout <- tex.results[, c(1, grep("TurnOutR|TurnOutD", names(tex.results)))]
df$party.winner <- apply(
                         df$party.turnout[2:3],
                         1,
                         function(x) rank(-x)
                         ) %>%
                   t()
df$party.winner[df$party.winner[, 1] == 1, 1] <- "Republican"
df$party.winner[df$party.winner[, 1] == 2, 1] <- "Democrat"
df$party.winner[, 2] <- tex.results$region
df$party.winner      <- data.frame(value = df$party.winner[, 1],
                                      region = as.numeric(df$party.winner[, 2]))
names(df$party.winner) <- c("value", "region")

# Plot data frame
plots$party.winner <- county_choropleth(df$party.winner,
                                        state_zoom = "texas",
                                        legend = "Winning Party",
                                        num_colors = 2) +
    coord_map() +
    scale_fill_manual(values = c("#0000FF", "#FF0000")) +
    labs(title = "Turn Out Winner by Party",
         fill = "Party")

# Margin of victory
df$party.margin <- (df$party.turnout$TurnOutR - df$party.turnout$TurnOutD) %>%
    data.frame(value = .)
df$party.margin$region <- tex.results$region
plots$party.margin  <- county_choropleth(df$party.margin, state_zoom = "texas",
                                            legend = "Margin of Victory",
                                            num_colors = 9) +
     coord_map() +
     scale_fill_manual(values = c("#1E59D9", "#D91E1E"))

# Margin of victory with range
df$party.range <- data.frame(
                             value = cut(
                                 df$party.margin$value,
                                 breaks = c(75, 50, 25, 5, 0, -5, -25, -50, -75),
                                 include.lowest = TRUE),
                             region = tex.results$region)

plots$party.range <- county_choropleth(df$party.range,
                                       state_zoom = "texas",
                                       legend = "Margin of Victory",
                                       num_colors = 9) +
  coord_map() +
  scale_fill_manual(values = c("#1E59D9", "#D91E1E"))


# Candidate Comparison ----------------------------------------------------

# Construst data frame
candidates <- c("Bush", "Carson", "Christie", "Cruz", "Fiorina", "Graham", 
                "Gray", "Huckabee", "Kasich", "Paul", "Rubio", "Santorum", "Trump", 
                "Clinton", "De La Fuente", "Hawes", "Judd", "Locke", "OMalley", 
                "Sanders", "Wilson")

df$candidate.votes <- tex.results[, c("CountyName", candidates)]

df$candidate.winner <- apply(df$candidate.votes[2:length(df$candidate.votes)],
                             1,
                             function(x) rank(-x)) %>%
                       t() %>%
                       data.frame()
df$candidate.winner[, ncol(df$candidate.winner) + 1] <- tex.results$region
df$candidate.winner <- df$candidate.winner %>% 
                       gather(key = value, value = place, Bush:Wilson)
df$candidate.winner <- df$candidate.winner[df$candidate.winner$place == 1, ]
names(df$candidate.winner)[1] <- "region"

# Plot
plots$candidate.winner <- county_choropleth(df$candidate.winner, state_zoom = "texas",
                                            legend = "Winning Candidate") +
  coord_map() +
  scale_fill_manual(values = c("#1D2951", "#8B0000", "#3FFF00", "#FF9F00")) +
  labs(title = "Vote Winner by Candidate - Either Party",
       fill = "Candidate")

# Misc --------------------------------------------------------------------


#Donald Trump vs Hillary Clinton

#Candidate with most votes (out of both parties)
