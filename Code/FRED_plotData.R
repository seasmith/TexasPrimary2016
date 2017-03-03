
# ---- Load Dependencies
# Load Dependencies -------------------------------------------------------

library(tidyverse)
library(choroplethr)
library(choroplethrMaps)
library(ggloop)
load("~/R/TexasPrimary2016/Data/FRED/FRED_2012.RData")
load("~/R/TexasPrimary2016/Data/FRED/FRED_2014.RData")



# ---- Filter Data Ex
# Filter Data Ex ----------------------------------------------------------

# Disregard
# # No need to plot data that are missing
# FRED_2014 <- FRED_2014[ , colSums(is.na(FRED_2014)) == 0]


# ---- Filter Data 1
# Filter Data -------------------------------------------------------------

.FRED <- FRED_2014 %>%
  select(CountyName,
         UnRate,
         ResPop,
         PCPI,
         BachDegree,
         HSDegree,
         EMHI)
.FRED$BachDegree <- FRED_2012$BachDegree  # replace 2014 education data w/ 2012
.FRED$HSDegree   <- FRED_2012$HSDegree    # replace 2014 education data w/ 2012


# ---- Filter Data 2
# Filter Out Education Data -----------------------------------------------

FRED <- .FRED %>% select(-BachDegree, -HSDegree)


# ---- Summary Plots
# Summary Plots --------------------------------------------------------------



# Boxplots ----------------------------------------------------------------
# ---- Boxplots Dressing
# Functional sequence to create labels.
make_labels <- . %>%
  plyr::count() %>%
  mutate(freq = freq / n) %>%
  unite(labels, x:freq, sep = " = ") %>%
  sapply(function(x) paste(x, "Counties")) %>%
  unlist() %>%
  `names<-`(NULL)

# Functional sequence to create legend.
make_legend <- . %>%
  factor(labels = make_labels(.))

# Need to divide counts by number sent to rep().
# Initialize n <- 2; change if needed.
n <- 2
  


# ---- Boxplots Basic

# Need list to hold all plots.
boxplot <- list()

# All variables
boxplot$All <- FRED %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot() +
  facet_wrap(~key, scales = "free")

# Income
boxplot$PCPI.EMHI <- FRED %>%
  select(PCPI, EMHI) %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot()

# Unemployment Rate
boxplot$UnRate <- FRED %>%
  select(UnRate) %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot()

# Population
boxplot$ResPop <- FRED %>%
  select(ResPop) %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot() +
  scale_y_log10()



# ---- Boxplots Economic

## Boxplotting data frame
FRED.bp <- FRED %>%
  keep(is.numeric) %>%
  gather(... = c(-ResPop, -UnRate))


ResidentialPopulation_Range <- FRED$ResPop %>%
  cut(breaks = c(0, median(FRED$ResPop), max(FRED$ResPop))) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.ResPop_PCPI.ResPop <- FRED.bp %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResidentialPopulation_Range)) +
  facet_wrap(~key) +
  scale_x_log10()


UnemploymentRate_Range <- FRED$UnRate %>%
  cut(breaks = c(0, median(FRED$UnRate), max(FRED$UnRate))) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.UnRate_PCPI.UnRate <- FRED.bp %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnemploymentRate_Range)) +
  facet_wrap(~key)



# ---- Boxplots Cutting 1

## Income vs ResPop
q <- FRED$ResPop %>%
  quantile()


ResidentialPopulation_Range <- FRED$ResPop %>%
  cut(breaks = q, include.lowest = TRUE) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.ResPop_PCPI.ResPop_cut1 <- FRED.bp %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResidentialPopulation_Range, fill  = ResidentialPopulation_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResidentialPopulation_Range), alpha = 1/2) +
  facet_wrap(~key) +
  scale_x_log10()


## Income vs UnRate
q <- FRED$UnRate %>%
  quantile()


UnemploymentRate_Range <- FRED$UnRate %>%
  cut(breaks = q, include.lowest = TRUE) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.UnRate_PCPI.UnRate_cut1 <- FRED.bp %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnemploymentRate_Range, fill = UnemploymentRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnemploymentRate_Range), alpha = 1/2) +
  facet_wrap(~key)

  

# ---- Boxplots Cutting 2

## Income vs ResPop
ResidentialPopulation_Range <- FRED$ResPop %>%
  cut(breaks = c(0, 3, 6, 15, 30, 50, 100, max(FRED$ResPop))) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.ResPop_PCPI.ResPop_cut2 <- FRED.bp %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResidentialPopulation_Range, fill  = ResidentialPopulation_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResidentialPopulation_Range), alpha = 1/2) +
  facet_wrap(~key) +
  scale_x_log10()


## Income vs UnRate
UnemploymentRate_Range <- FRED$UnRate %>%
  cut(breaks = c(0, 3.5, 4, 4.5, 5, 5.25, 5.5, max(FRED$UnRate))) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.UnRate_PCPI.UnRate_cut2 <- FRED.bp %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnemploymentRate_Range, fill = UnemploymentRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnemploymentRate_Range), alpha = 1/2) +
  facet_wrap(~key)



# ---- Boxplots Cutting 3

## Income vs ResPop
ResidentialPopulation_Range <- FRED$ResPop %>%
  cut_number(8) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.ResPop_PCPI.ResPop_cut3 <- FRED.bp %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResidentialPopulation_Range, fill  = ResidentialPopulation_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResidentialPopulation_Range), alpha = 1/2) +
  facet_wrap(~key) +
  scale_x_log10()


## Income vs UnRate
UnemploymentRate_Range <- FRED$UnRate %>%
  cut_number(8) %>%
  rep(n) %>%
  make_legend()


boxplot$EMHI.UnRate_PCPI.UnRate_cut3 <- FRED.bp %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnemploymentRate_Range, fill = UnemploymentRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnemploymentRate_Range), alpha = 1/2) +
  facet_wrap(~key)


# ---- Boxplots Cutting 4

make_labels2 <- . %>%
  plyr::count() %>%
  select(x) %>%
  unlist() %>%
  `names<-`(NULL)

# Functional sequence to create legend.
make_legend2 <- . %>%
  factor(labels = make_labels2(.))

# ResidentialPopulation_Range <- FRED$ResPop %>%
#   cut_number(8) %>%
#   rep(n) %>%
#   make_legend2()


ResidentialPopulation_Range <- ResidentialPopulation_Range[1:254]
UnemploymentRate_Range      <- UnemploymentRate_Range[1:254]


FRED.bp4 <- FRED %>% add_column(ResidentialPopulation_Range,
                                UnemploymentRate_Range) %>%
  discard(is.character) %>%
  gather(... = c(-UnRate,
                 -ResPop,
                 -ResidentialPopulation_Range,
                 -UnemploymentRate_Range))


boxplot$grid1 <- FRED.bp4 %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnemploymentRate_Range, fill = UnemploymentRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnemploymentRate_Range), alpha = 1/2) +
  facet_grid(ResidentialPopulation_Range ~ key)


boxplot$grid2 <- FRED.bp4 %>%
  ggplot(aes(UnRate, value)) +
  # geom_boxplot(aes(group = UnemploymentRate_Range, fill = UnemploymentRate_Range),
  #              alpha = 1/4) +
  geom_point(aes(color = UnemploymentRate_Range), alpha = 1/2) +
  facet_grid(ResidentialPopulation_Range ~ key)


boxplot$grid3 <- FRED.bp4 %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = UnemploymentRate_Range,
                   fill = UnemploymentRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnemploymentRate_Range), alpha = 1/2)


boxplot$grid3.1 <- boxplot$grid3 +
  facet_grid(UnemploymentRate_Range ~ key)


boxplot$grid3.2 <- boxplot$grid3 +
  facet_grid(key ~ UnemploymentRate_Range)



boxplot$grid4 <- FRED.bp4 %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = ResidentialPopulation_Range,
                   fill = ResidentialPopulation_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResidentialPopulation_Range), alpha = 1/2)


boxplot$grid4.1 <- boxplot$grid4 +
  facet_grid(ResidentialPopulation_Range ~ key)


boxplot$grid4.2 <- boxplot$grid4 +
  facet_grid(key ~ ResidentialPopulation_Range)


# ---- Points
# Point Plots -------------------------------------------------------------

point <- list()


# Population
ResidentialPopulation_Range <- FRED$ResPop %>%
  cut_number(8) %>%
  rep(1) %>%
  make_legend()


FRED.p1 <- FRED %>%
  mutate(ResidentialPopulation_Range = ResidentialPopulation_Range)


point$PCPI.EMHI_cut1 <- FRED.p1 %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point(data = transform(FRED.p1, ResidentialPopulation_Range = NULL), color = "gray80") +
  geom_point(aes(color = ResidentialPopulation_Range)) +
  facet_wrap(~ResidentialPopulation_Range)


point$PCPI.EMHI_cut1.boxplot <- FRED.p1 %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point(data = transform(FRED.p1, ResidentialPopulation_Range = NULL), color = "gray80") +
  geom_boxplot(aes(group = ResidentialPopulation_Range, fill = ResidentialPopulation_Range), alpha = 1/3) +
  geom_point(aes(color = ResidentialPopulation_Range)) +
  facet_wrap(~ResidentialPopulation_Range)


point$PCPI.EMHI_cut1.smooth <- FRED.p1 %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point(data = transform(FRED.p1, ResidentialPopulation_Range = NULL), color = "gray80") +
  geom_smooth(aes(group = ResidentialPopulation_Range)) +
  geom_point(aes(color = ResidentialPopulation_Range)) +
  facet_wrap(~ResidentialPopulation_Range)


# Unemployment
UnemploymentRate_Range <- FRED$UnRate %>%
  cut_number(8) %>%
  rep(1) %>%
  make_legend()


FRED.p2 <- FRED %>%
  mutate(UnemploymentRate_Range = UnemploymentRate_Range,
         ResidentialPopulation_Range = ResidentialPopulation_Range)


point$PCPI.EMHI_cut2 <- FRED.p2 %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point(data = transform(FRED.p2, UnemploymentRate_Range = NULL), color = "gray80") +
  geom_point(aes(color = UnemploymentRate_Range)) +
  facet_wrap(~UnemploymentRate_Range)


point$PCPI.EMHI_cut2.boxplot <- FRED.p2 %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point(data = transform(FRED.p2, UnemploymentRate_Range = NULL), color = "gray80") +
  geom_boxplot(aes(group = UnemploymentRate_Range, fill = UnemploymentRate_Range), alpha = 1/3) +
  geom_point(aes(color = UnemploymentRate_Range)) +
  facet_wrap(~UnemploymentRate_Range)


point$PCPI.EMHI_cut2.smooth <- FRED.p2 %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point(data = transform(FRED.p2, UnemploymentRate_Range = NULL), color = "gray80") +
  geom_smooth(aes(group = UnemploymentRate_Range)) +
  geom_point(aes(color = UnemploymentRate_Range)) +
  facet_wrap(~UnemploymentRate_Range)



# ---- Points Exp
point$exp <- FRED %>%
  ggloop(mappings = aes_loop(PCPI:EMHI, EMHI:PCPI),
         remap_xy = FALSE) +
  facet_wrap(~cut(x      = FRED$ResPop,
                  breaks = c(0,10,50,250,1000, max(FRED$ResPop))), ncol = 1)


point$exp[1] <- point$exp[1] +
  geom_boxplot(outlier.alpha = 1/3, outlier.color = "blue",
               fill = "blue", alpha = 1/3) +
  geom_point(alpha = 1/3, color = "red")


point$exp[2] <- point$exp[2] +
  geom_boxplot(outlier.alpha = 1/3, outlier.color = "red",
               fill = "red", alpha = 1/3) +
  geom_point(alpha = 1/3, color = "blue")

# gridExtra::grid.arrange(point$exp[[1]], point$exp[[2]], ncol = 2)


# Find means
s <- split(x = FRED,
           f = cut(x = FRED$ResPop,
                   breaks = c(0, 10, 50, 250, 1000, max(FRED$ResPop))))


means <- lapply(s, function(x) {
  x %>%
    keep(is.numeric) %>%
    colMeans()
})



# ---- Density
# Density -----------------------------------------------------------------

density <- list()


density$All <- FRED %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_density()


# ---- Density_2
# Keep the same rep number.
n = 2


ResidentialPopulation_Range <- FRED$ResPop %>%
  cut_number(8) %>%
  rep(n) %>%

    make_legend()

# Create new data frame
FRED5 <- FRED.bp %>%
  mutate(ResidentialPopulation_Range = ResidentialPopulation_Range)


# Income vs Population
density$EMHI.PCPI_ResPop <- FRED5 %>%
  group_by(key, ResidentialPopulation_Range) %>%
  mutate(median = median(value),
         mean   = mean(value)) %>%
  ggplot(aes(value)) +
  geom_density(aes(fill  = ResidentialPopulation_Range,
                   color = ResidentialPopulation_Range),
               alpha = 1/4) +
  geom_vline(aes(xintercept = median)) +
  geom_vline(aes(xintercept = mean), lty = "dashed") +
  facet_grid(ResidentialPopulation_Range ~ key)


# Income vs Population
density$EMHI.PCPI_ResPop2 <- FRED5 %>%
  group_by(key, ResidentialPopulation_Range) %>%
  mutate(median = median(value),
         mean   = mean(value)) %>%
  ggplot(aes(value, ..count..)) +
  geom_density(aes(fill  = ResidentialPopulation_Range,
                   color = ResidentialPopulation_Range),
               alpha = 1/4,
               position = "stack") +
  geom_vline(aes(xintercept = median(value))) +
  geom_vline(aes(xintercept = mean(value)), lty = "dashed") +
  facet_grid(~key)


# Income vs Population
density$EMHI.PCPI_ResPop3 <- FRED5 %>%
  group_by(key, ResidentialPopulation_Range) %>%
  mutate(median = median(value),
         mean   = mean(value)) %>%
  ggplot(aes(value, ..count..)) +
  geom_density(aes(fill  = ResidentialPopulation_Range,
                   color = ResidentialPopulation_Range),
               alpha = 1/4,
               position = "fill") +
  facet_grid(~key)



# ---- Population_Density
# Population_Density ------------------------------------------------------

county.map_texas <- county.map %>%
  select(STATE, NAME, CENSUSAREA) %>%
  filter(STATE == 48) %>%
  select(-STATE) %>%
  distinct() %>%
  mutate(NAME = stringi::stri_trans_tolower(NAME))

