library(tidyverse)
setwd("~/Coursework/DatEcon/Project/Data/CountyData/FMR")

lastTwoToYear = function(strings){
  prefix = ifelse(strings > 26, "19", "20")
  str_c(prefix, strings) %>% as.integer
}

# Free unused memory after running!
fmr = read_csv("FMR_All_1983_2025.csv") %>% 
  # FIPS codes: First 5 digits are counties
  mutate(fips = str_extract(fips, "\\d{5}")) %>% 
  select(c(starts_with("fmr"), "fips", "state", starts_with("pop"))) %>%
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
  group_by(fips, year, bedrooms) %>% 
  summarize(fair_market_rent = mean(fair_market_rent),
            across(starts_with("pop"), mean)) %>% 
  pivot_wider(names_from = "bedrooms", values_from = "fair_market_rent", names_prefix = "fmr_")

write_csv(fmr, "FMR.csv")

  
