library(tidyverse)
setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/Final/Minnesota/FMR/")

lastTwoToYear = function(strings){
  prefix = ifelse(strings > 26, "19", "20")
  str_c(prefix, strings) %>% as.integer
}

# Free unused memory after running!
fmr = read_csv("FMR_All_1983_2025.csv") %>% 
  # FIPS codes: First 5 digits are counties
  select(c(starts_with("fmr"), starts_with("pop"), "areaname22", "state")) %>%
  mutate(fmr25 = 40, fmr24 = 40) %>% 
  # Each year/bedroom is a column: pivot so rows are county+year
  pivot_longer(starts_with("fmr") & contains("_"), 
               names_prefix = "fmr", names_to = "year", values_to = "fair_market_rent") %>% 
  mutate(bedrooms = as.integer(str_extract(year, "(?<=_)\\d")),
         year = str_extract(year, "\\d+(?=_)")) %>% 
  # Percentiles for each year also have their own columns
  pivot_longer(starts_with("fmr") & !contains("_"), 
               names_prefix = "fmr", names_to = "year2", values_to = "percentile") %>% 
  filter(year == year2) %>% 
  select(-year2) %>% 
  mutate(year = lastTwoToYear(year)) %>% 
  filter(percentile == 40) %>% 
  select(-percentile) %>% 
  group_by(areaname22, year, bedrooms) %>% 
  summarize(fair_market_rent = mean(fair_market_rent),
            across(starts_with("pop"), sum),
            state = median(state)) %>%
  pivot_wider(names_from = "bedrooms", values_from = "fair_market_rent", names_prefix = "fmr_") %>% 
  ungroup() %>% 
  rename(statecode = state) %>%
  mutate(statecode = as.character(statecode))

statecodes = read_csv("crosswalk.csv") %>% 
  select(fipsstatecode, statename) %>% 
  rename(statecode = fipsstatecode) %>% 
  distinct()

fmr = fmr %>% 
  inner_join(crosswalk)

write_csv(fmr, "FMR.csv")
