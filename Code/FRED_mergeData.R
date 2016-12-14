# Merging FRED data into the tex.results and other data frames.

# Estimate of Median Household Income for 2013
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

# Merge EHMI_2013 with fred.series

# Per Capita Personal Income for 2013
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

tst <- tex.results %>% select(CountyName, TotalVotesR) %>% inner_join(FRED, "CountyName") %>% inner_join(rankR, "CountyName")
