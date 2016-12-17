
# Load Dependencies -------------------------------------------------------


library(dplyr)
load("~/R/TexasPrimary2016/Data/FRED/fred.tables.RData")
load("~/R/TexasPrimary2016/Data/FRED/fred.series.RData")



# Estimate of Median Household Income for 2013 ----------------------------


EMHI.rndx <- fred.tables$`Estimate of Median Household Income` %>%
  nrow() %>%
  `-`(1)
EMHI_2013 <- fred.tables$`Estimate of Median Household Income`[ EMHI.rndx, -1] %>%
  t()
EMHI_2013 <- data.frame(EMHI_2013 = as.numeric(EMHI_2013),
                        SeriesID = as.character(rownames(EMHI_2013)),
                        stringsAsFactors = FALSE)

FRED <- fred.series %>%
  select(SeriesID, CountyName) %>%
  inner_join(EMHI_2013, "SeriesID")



# Per Capita Personal Income for 2013 -------------------------------------


PCPI.rndx <- fred.tables$`Per Capita Personal Income` %>%
  nrow() %>%
  `-`(1)
PCPI_2013 <- fred.tables$`Per Capita Personal Income`[PCPI.rndx, -1] %>%
  t()
PCPI_2013 <- data.frame(PCPI_2013 = as.numeric(PCPI_2013),
                        SeriesID = as.character(rownames(PCPI_2013)),
                        stringsAsFactors = FALSE)

FRED <- fred.series %>%
  select(SeriesID, CountyName) %>%
  inner_join(PCPI_2013, "SeriesID") %>%
  inner_join(FRED, "CountyName") %>%
  select(CountyName, PCPI_2013, EMHI_2013)



# Residential Population for 2013 -----------------------------------------


RP.ndx <- fred.tables$`Resident Population` %>%
  nrow() %>%
  `-`(1)
RP_2013 <- fred.tables$`Resident Population`[RP.ndx, -1] %>%
  t()
RP_2013 <- data.frame(RP_2013 = as.numeric(RP_2013),
                      SeriesID = as.character(rownames(RP_2013)),
                      stringsAsFactors = FALSE)

FRED <- fred.series %>%
  select(SeriesID, CountyName) %>%
  inner_join(RP_2013, "SeriesID") %>%
  inner_join(FRED, "CountyName") %>%
  select(CountyName, PCPI_2013, EMHI_2013, RP_2013)



# Save Results ------------------------------------------------------------


dir.save <- "Data/FRED"
save(FRED, file = file.path(dir.save, "FRED.RData"))
