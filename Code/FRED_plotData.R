
# ---- Load Dependencies
# Load Dependencies -------------------------------------------------------

library(tibble)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
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
.FRED$BachDegree <- FRED_2012$BachDegree
.FRED$HSDegree   <- FRED_2012$HSDegree


# ---- Filter Data 2
# Filter Out Education Data -----------------------------------------------

FRED <- .FRED %>% select(CountyName,
                        UnRate,
                        ResPop,
                        PCPI,
                        EMHI)


# ---- Summary Plots
# Summary Plots --------------------------------------------------------------



# ---- Density
# Density -----------------------------------------------------------------

density <- list()

density$All <- FRED %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_density()


# Boxplots ----------------------------------------------------------------
# ---- Boxplots Dressing

make_labels <- . %>%
  plyr::count() %>%
  mutate(freq = freq / n) %>%
  unite(labels, x:freq, sep = " = ") %>%
  sapply(function(x) paste(x, "Counties")) %>%
  unlist() %>%
  `names<-`(NULL)

# Need to divide counts by number sent to rep().
# Initialize n <- 2; change if needed.
n <- 2
  
# Functional sequence to create legend
make_legend <- . %>%
  factor(labels = make_labels(.))


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

## Income vs ResPop
FRED2 <- FRED %>%
  keep(is.numeric) %>%
  gather(-ResPop, key = "key", value = "value")

ResPop_Range <- FRED$ResPop %>%
  cut(breaks = c(0, median(FRED$ResPop), max(FRED$ResPop))) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.ResPop_PCPI.ResPop <- FRED2 %>%
  filter(key != "UnRate") %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResPop_Range)) +
  facet_wrap(~key) +
  scale_x_log10()

## Income vs UnRate
FRED3 <- FRED %>%
  keep(is.numeric) %>%
  gather(-UnRate, key = "key", value = "value")

UnRate_Range <- FRED$UnRate %>%
  cut(breaks = c(0, median(FRED$UnRate), max(FRED$UnRate))) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.UnRate_PCPI.UnRate <- FRED3 %>%
  filter(key != "ResPop") %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnRate_Range)) +
  facet_wrap(~key)


# ---- Boxplots Cutting 1

## Income vs ResPop
q <- FRED$ResPop %>%
  quantile()

ResPop_Range <- FRED$ResPop %>%
  cut(breaks = q, include.lowest = TRUE) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.ResPop_PCPI.ResPop_cut1 <- FRED2 %>%
  filter(key != "UnRate") %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResPop_Range, fill  = ResPop_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResPop_Range), alpha = 1/2) +
  facet_wrap(~key) +
  scale_x_log10()

## Income vs UnRate
q <- FRED$UnRate %>%
  quantile()

UnRate_Range <- FRED$UnRate %>%
  cut(breaks = q, include.lowest = TRUE) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.UnRate_PCPI.UnRate_cut1 <- FRED3 %>%
  filter(key != "ResPop") %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnRate_Range, fill = UnRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnRate_Range), alpha = 1/2) +
  facet_wrap(~key)
  

# ---- Boxplots Cutting 2

## Income vs ResPop
ResPop_Range <- FRED$ResPop %>%
  cut(breaks = c(0, 3, 6, 15, 30, 50, 100, max(FRED$ResPop))) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.ResPop_PCPI.ResPop_cut2 <- FRED2 %>%
  filter(key != "UnRate") %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResPop_Range, fill  = ResPop_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResPop_Range), alpha = 1/2) +
  facet_wrap(~key) +
  scale_x_log10()

## Income vs UnRate
UnRate_Range <- FRED$UnRate %>%
  cut(breaks = c(0, 3.5, 4, 4.5, 5, 5.25, 5.5, max(FRED$UnRate))) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.UnRate_PCPI.UnRate_cut2 <- FRED3 %>%
  filter(key != "ResPop") %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnRate_Range, fill = UnRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnRate_Range), alpha = 1/2) +
  facet_wrap(~key)


# ---- Boxplots Cutting 3

## Income vs ResPop
ResPop_Range <- FRED$ResPop %>%
  cut_number(8) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.ResPop_PCPI.ResPop_cut3 <- FRED2 %>%
  filter(key != "UnRate") %>%
  ggplot(aes(ResPop, value)) +
  geom_boxplot(aes(group = ResPop_Range, fill  = ResPop_Range),
               alpha = 1/4) +
  geom_point(aes(color = ResPop_Range), alpha = 1/2) +
  facet_wrap(~key) +
  scale_x_log10()

## Income vs UnRate
UnRate_Range <- FRED$UnRate %>%
  cut_number(8) %>%
  rep(n) %>%
  make_legend()

boxplot$EMHI.UnRate_PCPI.UnRate_cut3 <- FRED3 %>%
  filter(key != "ResPop") %>%
  ggplot(aes(UnRate, value)) +
  geom_boxplot(aes(group = UnRate_Range, fill = UnRate_Range),
               alpha = 1/4) +
  geom_point(aes(color = UnRate_Range), alpha = 1/2) +
  facet_wrap(~key)



# ---- Points
# Point Plots -------------------------------------------------------------

point <- list()
point$PCPI.EMHI_cut1 <- FRED %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point() +
  facet_wrap(~cut(x = FRED$ResPop,
                  breaks = c(0, 10, 50, 250, 1000, max(FRED$ResPop))),
             ncol = 5)

point$PCPI.EMHI_cut1.boxplot <- FRED %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_boxplot() +
  geom_point(alpha = 1/3) +
  facet_wrap(~cut(x = FRED$ResPop,
                  breaks = c(0, 10, 50, 250, 1000, max(FRED$ResPop))),
             ncol = 5)

point$PCPI.EMHI_cut2 <- FRED %>%
  ggplot(aes(EMHI, PCPI)) +
  geom_point() +
  facet_wrap(~cut_number(ResPop, 5),
             ncol = 5)

# ---- Points Exp
point$exp <- FRED %>%
  ggloop(mappings = aes_loop(PCPI:EMHI, EMHI:PCPI),
         remap_xy = FALSE) %L+%
  facet_wrap(~cut(x      = FRED$ResPop,
                  breaks = c(0,10,50,250,1000, max(FRED$ResPop))), ncol = 1)

point$exp[1] <- point$exp[1] %L+%
  geom_boxplot(outlier.alpha = 1/3, outlier.color = "blue",
               fill = "blue", alpha = 1/3) %L+%
  geom_point(alpha = 1/3, color = "red")

point$exp[2] <- point$exp[2] %L+%
  geom_boxplot(outlier.alpha = 1/3, outlier.color = "red",
               fill = "red", alpha = 1/3) %L+%
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

