
# Load Dependencies -------------------------------------------------------


library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggloop)
load("~/R/TexasPrimary2016/Data/Primary/tex.results.RData")
load("~/R/TexasPrimary2016/Data/Primary/rankAll.RData")
load("~/R/TexasPrimary2016/Data/FRED/FRED.RData")



# Z Scores ----------------------------------------------------------------

zscores <- list()

# Data frame - Republican per county z-scores
zscores$R <- tex.results %>%
  select(Bush:Uncommitted) %>%
  apply(1, sd) %>%
  data.frame(sd = .)

zscores$R$mean <- tex.results %>%
  select(Bush:Uncommitted) %>%
  apply(1, mean)

zscores$R <- tex.results %>%
  select(Bush:Uncommitted) %>%
  lapply(function(x) (x - zscores$R$mean) / zscores$R$sd) %>%
  dplyr::bind_cols(zscores$R,
                   data.frame(TotalVotesR = tex.results$TotalVotesR),
                   data.frame(WinnerR     = rankAll$WinnerR,
                              RunnerUpR   = rankAll$RunnerUpR))

# Plot
zscores$Rplot <- ggplot(zscores$R,
                        aes(x     = Cruz,
                            y     = Trump,
                            size  = TotalVotesR,
                            color = WinnerR)) +
  ggtitle("Z-Score: Cruz vs Trump") +
  geom_point(alpha = 3/5) +
  scale_color_manual(values = c("blue", "red"))

# Plot
# zscores$Rubio.v.Cruz_winner <- ggplot(zscores$R,
#                                    aes(x = Rubio,
#                                        y = Cruz,
#                                        size = TotalVotesR)) +
#   geom_point() +
#   facet_wrap(~ Winner, nrow = 2)

zscores$Rubio.v.Cruz_winner <- ggplot(zscores$R,
                                      aes(x = Rubio,
                                          y = Cruz,
                                          size = TotalVotesR,
                                          color = WinnerR)) +
  geom_point()

zscores$Rubio.v.Cruz_runnerup <- ggplot(zscores$R,
                                        aes(x = Rubio,
                                            y = Cruz,
                                            size = TotalVotesR,
                                            color = RunnerUpR)) +
  geom_point()

# Data frame - Democrat per county z-scores
zscores$D <- tex.results %>%
  select(Clinton:Wilson) %>%
  apply(1, sd) %>%
  data.frame(sd = .)

zscores$D$mean <- tex.results %>%
  select(Clinton:Wilson) %>%
  apply(1, mean)

zscores$D <- tex.results %>%
  select(Clinton:Wilson) %>%
  lapply(function(x) (x - zscores$D$mean) / zscores$D$sd) %>%
  dplyr::bind_cols(zscores$D,
                   data.frame(TotalVotesD = tex.results$TotalVotesD),
                   data.frame(WinnerD     = rankAll$WinnerD)
  )

zscores$Dplot <- ggplot(zscores$D,
                        aes(x     = Clinton,
                            y     = Sanders,
                            size  = TotalVotesD,
                            color = WinnerD)) +
  ggtitle("Z-Score: Clinton vs Sanders") +
geom_point(alpha = 3/5) +
  scale_color_manual(values = c("blue", "red", "purple"))




# More Plots --------------------------------------------------------------

counts <- FRED %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_density()

df <- rankAll %>% left_join(FRED, "CountyName")

counts.PartyWinner <- df %>%
  ggloop(aes_loop(PCPI_2013:RP_2013, color = PartyWinner)) %L+%
  geom_density() %L+%
  scale_color_manual(values = c("blue", "red"))

multiplot(plotlist = counts.PartyWinner[[1]], cols = 2)
