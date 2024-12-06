setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/DID")
library(tidyverse)

cross = read_csv("crosswalk.csv") %>% 
  rename(cbsa_code = cbsacode, state_fips = fipsstatecode) %>% 
  mutate(metro = str_starts(metropolitanmicropolitanstatis, "Metro")) %>% 
  select(cbsa_code, state_fips, metro)

rdc = read_csv("RDC_Inventory_Core_Metrics_Metro_History.csv") %>% 
  mutate(date = ym(month_date_yyyymm),
         year = year(date),
         month = month(date)) %>% 
  select(-c(month_date_yyyymm, month,
            ends_with("yy"), ends_with("mm"), quality_flag)) %>% 
  inner_join(cross) %>% 
  filter(metro) %>% 
  mutate(state = covidcast::fips_to_abbr(state_fips)) %>% 
  select(-metro, state_fips)

write_csv(rdc, "RDC.csv")