
# ---- Load Dependencies
# Load Dependencies -------------------------------------------------------


library(tibble)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
load("~/R/TexasPrimary2016/Data/FRED/FRED.RData")



# ---- Summary Plots
# Summary Plots --------------------------------------------------------------

# ---- Density

density <- list()

density$All <- FRED %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_density()

# ---- Boxplots Basic

boxplot <- list()

boxplot$PCPI.EMHI <- FRED %>%
  select(PCPI_2013, EMHI_2013) %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot()

boxplot$RP <- FRED %>%
  select(RP_2013) %>%
  gather() %>%
  ggplot(aes(key, value)) +
  geom_boxplot()

boxplot$RP.log <- boxplot$RP +
  scale_y_log10()

# ---- Boxplots Economic

FRED2 <- FRED %>%
  keep(is.numeric) %>%
  gather(-RP_2013, key = "key", value = "value")

boxplot$EMHI.RP_PCPI.RP <- FRED2 %>%
  ggplot(aes(RP_2013, value)) +
  geom_boxplot(aes(group = rep(cut(x      = FRED$RP_2013,
                                   breaks = c(0, 1000, max(FRED$RP_2013))), 2))) +
  facet_wrap(~key)

# ---- Boxplots Cutting1

boxplot$EMHI.RP_PCPI.RP_cut1 <- FRED2 %>%
  ggplot(aes(RP_2013, value)) +
  geom_boxplot(aes(group = rep(cut(x      = FRED$RP_2013,
                               breaks = c(0, 10, 25, 50, 100, 250, 500, 1000,
                                          max(FRED$RP_2013))), 2),
                   fill  = rep(cut(x      = FRED$RP_2013,
                               breaks = c(0, 10, 25, 50, 100, 250, 500, 1000,
                                          max(FRED$RP_2013))), 2))) +
  facet_wrap(~key) +
  scale_x_log10()

# ---- Boxplots Cutting2

boxplot$EMHI.RP_PCPI.RP_cut2 <- FRED2 %>%
  ggplot(aes(RP_2013, value)) +
  geom_boxplot(aes(group = rep(cut(x = FRED$RP_2013,
                                   breaks = c(0, 2, 6, 10, 25, 50, 100, max(FRED$RP_2013))), 2),
                   fill = rep(cut(x = FRED$RP_2013,
                                  breaks = c(0, 2, 6, 10, 25, 50, 100, max(FRED$RP_2013))), 2))) +
  facet_wrap(~key) +
  scale_x_log10()

# ---- Points

point <- list()
point$PCPI.EMHI_cut1 <- FRED %>%
  ggplot(aes(EMHI_2013, PCPI_2013)) +
  geom_point() +
  facet_wrap(~cut(x = FRED$RP_2013,
                  breaks = c(0, 10, 50, 250, 1000, max(FRED$RP_2013))),
             ncol = 5)

point$PCPI.EMHI_cut1.boxplot <- FRED %>%
  ggplot(aes(EMHI_2013, PCPI_2013)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~cut(x = FRED$RP_2013,
                  breaks = c(0, 10, 50, 250, 1000, max(FRED$RP_2013))),
             ncol = 5)

point$PCPI.EMHI_cut2 <- FRED %>%
  ggplot(aes(EMHI_2013, PCPI_2013)) +
  geom_point() +
  facet_wrap(~cut_number(RP_2013, 5),
             ncol = 5)

# Alt cut
ct <- cut_number(FRED$RP_2013, 6)

# Find means
s <- split(x = FRED,
           f = cut(x = FRED$RP_2013,
                   breaks = c(0, 10, 50, 250, 1000, max(FRED$RP_2013))))

means <- lapply(s, function(x) {
  x %>%
    keep(is.numeric) %>%
    colMeans()
})

# point$PCPI.EMHI_cut2 <- FRED %>%
#   ggplot(aes(EMHI_2013, PCPI_2013,
#              color = cut(x      = FRED$RP_2013,
#                          breaks = c(0, 10, 50, 250, 1000, max(FRED$RP_2013))))) +
#   geom_point() +
#   guides(color = guide_legend(title = "Point Plot")) +
#   scale_color_manual(values = c("red", "orange", "white", "green", "blue")) +
#   theme_dark()
