
# Load Dependencies -------------------------------------------------------


library(tibble)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggloop)
load("~/R/TexasPrimary2016/Data/Primary/rankAll.RData")
load("~/R/TexasPrimary2016/Data/FRED/FRED.RData")


# Create Plots ------------------------------------------------------------


df <- rankAll %>% left_join(FRED, "CountyName")

counts.PartyWinner <- df %>%
  ggloop(aes_loop(PCPI_2013:RP_2013, color = PartyWinner)) %L+%
  geom_density() %L+%
  scale_color_manual(values = c("blue", "red"))

multiplot(plotlist = counts.PartyWinner[[1]], cols = 2)

# ---- Rearrange data
# Rearrange data ----------------------------------------------------------


rankAll_FRED <- rankAll %>%
  select(CountyName, PartyWinner, WinnerR, WinnerD) %>%
  left_join(FRED) %>% as_tibble()

rankAll_FRED.long <- rankAll_FRED %>%
  select(-CountyName) %>%
  gather(-PartyWinner, -WinnerR, -WinnerD, key = "key", value = "value")


# ---- Density Plots
# Density Plots -----------------------------------------------------------


density <- list()

density$PartyWinner <- rankAll_FRED.long %>%
  ggplot(aes(value, fill = PartyWinner)) +
  facet_wrap(~key, scales = "free", ncol = 2) +
  geom_density(alpha = 1/4) +
  scale_fill_manual(values = c("blue", "red"))

density$WinnerR <- rankAll_FRED.long %>%
  ggplot(aes(value, fill = WinnerR)) +
  facet_wrap(~key, scales = "free", ncol = 2) +
  geom_density(alpha = 1/4) +
  scale_fill_manual(values = c("blue", "red"))

density$WinnerR.WinnerD <- rankAll_FRED.long %>%
  filter(WinnerD != "Clinton-Sanders") %>%
  ggplot(aes(value, fill = WinnerR)) +
  facet_wrap(~key + WinnerD, scales = "free", ncol = 2) +
  geom_density(alpha = 1/4) +
  scale_fill_manual(values = c("blue", "red"))

density$WinnerD <- rankAll_FRED.long %>%
  ggplot(aes(value, fill = WinnerD)) +
  facet_wrap(~key, scales = "free", ncol = 2) +
  geom_histogram(alpha = 1/4) +
  scale_fill_manual(values = c("blue", "purple", "red"))

density$WinnerD.notie <- rankAll_FRED.long %>%
  filter(WinnerD != "Clinton-Sanders") %>%
  ggplot(aes(value, fill = WinnerD)) +
  facet_wrap(~key, scales = "free", ncol = 2) +
  geom_density(alpha = 1/4) +
  scale_fill_manual(values = c("blue", "red"))

density$WinnerD.notie.WinnerR <- d_winner.notie +
  facet_wrap(~key + WinnerR, scales = "free", ncol = 2) +
  geom_density(alpha = 1/4) +
  scale_fill_manual(values = c("blue", "red"))



# ---- Boxplots
# Boxplots ----------------------------------------------------------------


boxplot <- list()

boxplot$WinnerR <- rankAll_FRED.long %>%
  filter(key != "RP_2013") %>%
  ggplot(aes(key, value, fill = WinnerR)) + 
  geom_boxplot(alpha = 1/2) +
  scale_fill_manual(values = c("blue", "red"))

boxplot$WinnerD <- rankAll_FRED.long %>%
  filter(key != "RP_2013") %>%
  ggplot(aes(key, value, fill = WinnerD)) + 
  geom_boxplot(alpha = 1/2) +
  scale_fill_manual(values = c("blue", "purple", "red"))
