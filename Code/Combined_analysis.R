
# Load_Dependencies -------------------------------------------------------

library(tidyverse)
load("~/R/TexasPrimary2016/Data/FRED/fred.tables.RData")
load("~/R/TexasPrimary2016/Data/FRED/FRED.RData")
load("~/R/TexasPrimary2016/Data/Primary/tex.results.RData")
load("~/R/TexasPrimary2016/Data/Primary/rankAll.RData")

# Data_Prep ---------------------------------------------------------------

percents <- tex.results %>%
  select(CountyName, TotalVotesR, TotalVotesD) %>%
  mutate(Percent_R = TotalVotesR/(TotalVotesR + TotalVotesD),
         Percent_D = TotalVotesD/(TotalVotesD + TotalVotesR)) %>%
  select(-(TotalVotesR:TotalVotesD))

# Add party winner
primary_data <- percents %>%
  inner_join(select(rankAll, CountyName, PartyWinner), by = "CountyName")

# Add county area
county.map_texas <- county.map %>%
  select(STATE, NAME, CENSUSAREA) %>%
  filter(STATE == 48) %>%
  select(-STATE) %>%
  distinct() %>%
  mutate(NAME = stringi::stri_trans_tolower(NAME))

primary_data <- primary_data %>%
  inner_join(county.map_texas, by = c("CountyName" = "NAME"))

# Add population data
combined <- primary_data %>%
  inner_join(FRED, by = "CountyName") %>%
  mutate(Pop_Density = ResPop*1000/CENSUSAREA)




# Plots -------------------------------------------------------------------


combined_long %>%
  ggplot(aes(Pop_Density, value)) +
    geom_point(data = select(combined_long, -PartyWinner), color = "gray70") +
    geom_point(aes(color = PartyWinner), alpha = .25) +
    facet_grid(key ~ PartyWinner, scales = "free_y") +
    scale_color_manual(values = c("blue", "red")) +
    scale_x_log10()


# Long_Format -------------------------------------------------------------


PCPI_long <- fred.tables$`Per Capita Personal Income` %>%
  gather(... = -Date, PCPI_key, PCPI_value) %>%
  inner_join(select(fred.series, SeriesID, CountyName), by = c("PCPI_key" = "SeriesID")) %>%
  inner_join(primary_data, by = "CountyName")

# PCPI normalized from 0 (lowest) to 1 (highest)
PCPI_norm_long <- fred.tables$`Per Capita Personal Income` %>%
  mutate_at(vars(starts_with("PCPI")), seasmith::normalize) %>%
  gather(... = -Date, PCPI_key, PCPI_value) %>%
  inner_join(select(fred.series, SeriesID, CountyName), by = c("PCPI_key" = "SeriesID")) %>%
  inner_join(primary_data, by = "CountyName")

RP_long <- fred.tables$`Resident Population` %>%
  gather(... = -Date, RP_key, RP_value) %>%
  inner_join(select(fred.series, SeriesID, CountyName), by = c("RP_key" = "SeriesID")) %>%
  inner_join(primary_data, by = "CountyName")

UR_long <- fred.tables$`Unemployment Rate` %>%
  gather(... = -Date, UR_key, UR_value) %>%
  inner_join(select(fred.series, SeriesID, CountyName), by = c("UR_key" = "SeriesID")) %>%
  inner_join(primary_data, by = "CountyName")




# Plots -------------------------------------------------------------------

plots <- list()

# ---- PCPI
# County PCPI over time; faceted and colored
PCPI_norm_long %>%
  ggplot(aes(Date, PCPI_value)) +
  geom_line(data = select(PCPI_norm_long, -PartyWinner), aes(group = PCPI_key), color = "gray70") +
  geom_line(aes(group = PCPI_key, color = PartyWinner)) +
  facet_wrap(~PartyWinner) +
  scale_color_manual(values = c("blue", "red"))

# Counties whose PCPI peaked before 2014
PCPI_norm_long %>%
  filter(PCPI_value == 1, Date < "2014-01-01") %>%
  ggplot(aes(Date, Percent_R)) +
  geom_point(aes(color = PartyWinner))

PCPI_long %>%
  filter(Date == "2014-01-01") %>%
  gather(... = c(Percent_D, Percent_R)) %>%
  ggplot(aes(PCPI_value, value)) +
    geom_point(aes(color = key))



# ---- PCPI_and_RP
plots$p1 <- PCPI_long %>%
  inner_join(RP_long, by = c("Date", "CountyName")) %>%
  ggplot(aes(Date, RP_value)) +
    geom_line(aes(group = CountyName, color = PartyWinner.x), alpha = 1/4)

plots$p1 +
    facet_grid(. ~ PartyWinner.x)

plots$p2 <- PCPI_long %>%
  inner_join(select(RP_long, -PartyWinner), by = c("Date", "CountyName")) %>%
  ggplot(aes(RP_value, PCPI_value)) +
    geom_point(aes(color = PartyWinner), alpha = 1/4)


# ---- UR_and_RP
plots$p3 <- UR_long %>%
  inner_join(select(RP_long, -PartyWinner), by = c("Date", "CountyName")) %>%
  ggplot(aes(RP_value, UR_value)) +
    geom_point(aes(color = PartyWinner), alpha = 1/2, size = 1)


# ---- UR_and_PCPI
UR_PCPI_long <- UR_long %>%
  inner_join(select(PCPI_long, -PartyWinner), by = c("Date", "CountyName"))

UR_PCPI_long %>%
  ggplot(aes(PCPI_value, UR_value)) +
    geom_point(data = select(UR_PCPI_long, -PartyWinner), color = "gray70") +
    geom_point(aes(color = PartyWinner), alpha = 1/4) +
    scale_color_manual(values = c("blue", "red")) +
    facet_wrap(~PartyWinner)

