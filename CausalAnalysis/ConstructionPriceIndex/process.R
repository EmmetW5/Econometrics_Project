setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/ConstructionPriceIndex")
library(tidyverse)
library(readxl)

sold_ind = read_xlsx("price_sold_cust.xlsx") %>% 
  select(Year, starts_with("Q")) %>% 
  pivot_longer(starts_with("Q"), names_to = "Quarter", 
               values_to = "sold_price_ind") %>% 
  mutate(Quarter = str_extract(Quarter, "[0-9]$") %>% as.numeric(),
         date_string = str_c(Year, " ", (Quarter-1)*3 + 1),
         date = ym(date_string)) %>% 
  select(-date_string) %>% 
  rename_with(str_to_lower, everything()) %>% 
  na.omit()

constr_ind = read_xlsx("price_uc_cust.xlsx") %>% 
  mutate(date = date(Month)) %>% 
  rename(constr_price_ind = `Laspeyres\n (Fixed)`) %>% 
  select(date, constr_price_ind)

cpi = read_csv("CPIU.csv") %>% 
  rename(date = DATE, cpi = CPIAUCSL)

ffr = read_csv("FEDFUNDS.csv") %>% 
  rename(date = DATE, ffr = FEDFUNDS)

mspus = read_csv("MSPUS.csv") %>% 
  rename_with(str_to_lower, everything())

constr_data = sold_ind %>% 
  inner_join(mspus) %>%
  inner_join(constr_ind) %>% 
  inner_join(cpi) %>% 
  inner_join(ffr) %>% 
  mutate(sold_price_ind = sold_price_ind,
         constr_price_ind = constr_price_ind,
         sold_corrected = (mspus / sold_price_ind),
         constr_corrected = (mspus / constr_price_ind),
         real_ffr = ffr / cpi)

write_csv(constr_data, "CONSTR.csv")
