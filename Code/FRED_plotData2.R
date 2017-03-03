# Load Dependencies -------------------------------------------------------



# Education Data ----------------------------------------------------------



FRED.ed1 <- .FRED %>%
  select(BachDegree, HSDegree) %>%
  gather() %>%
  mutate(ResPop_Range = rep(ResPop_Range, 2)) %>%
  group_by(key, ResPop_Range) %>%
  mutate(median = median(value),
         mean   = mean(value)) #%>%
  # group_by(key) %>%
  # mutate(median_all = median(value),
  #        mean_all = mean(value))

# Better?
.FRED %>%
  select(ResPop, BachDegree, HSDegree) %>%
  gather(-ResPop, key = "key", value = "value") %>%
  mutate(ResPop_Range = rep(ResPop_Range, 2)) %>%
  group_by(key, ResPop_Range) %>%
  mutate(range_mean = mean(value*ResPop)/mean(ResPop),
         range_median = median(value)) %>%
  group_by(key) %>%
  mutate(domain_mean = mean(value*ResPop)/mean(ResPop),
         domain_median = median(value))

# Mean education values.
means <- FRED.ed1 %>%
  group_by(key) %>%
  summarise(mean(value))

# Median education values.
medians <- FRED.ed1 %>%
  group_by(key) %>%
  summarise(median(value))

# These xintercept medians and means are WRONG.
# Need to group_by(key, ResPop_Range) %>% mutate(... = fun(value*ResPop))
FRED.ed1 %>%
  ggplot(aes(value)) +
  geom_density() +
  geom_vline(aes(xintercept = median), size = 1) +
  geom_vline(aes(xintercept = mean), size = 1, lty = "dashed") +
  # geom_vline(aes(xintercept = median_all), color = "red") +
  # geom_vline(aes(xintercept = mean_all), color = "red", lty = "dashed") +
  facet_grid(ResPop_Range ~ key)

