setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/Final/ConstructionPriceIndex")
library(tidyverse)
library(readxl)

rename_lower = function(x) rename_with(x, str_to_lower, everything())

sold_ind = read_xlsx("price_sold_cust.xlsx") %>% 
  select(Year, starts_with("Q")) %>% 
  pivot_longer(starts_with("Q"), names_to = "Quarter", 
               values_to = "sold_price_ind") %>% 
  mutate(Quarter = str_extract(Quarter, "[0-9]$") %>% as.numeric(),
         date_string = str_c(Year, " ", (Quarter-1)*3 + 1),
         date = ym(date_string)) %>% 
  select(-date_string) %>% 
  rename_lower() %>% 
  na.omit()

constr_ind = read_xlsx("price_uc_cust.xlsx") %>% 
  mutate(date = date(Month)) %>% 
  rename(constr_price_ind = `Laspeyres\n (Fixed)`) %>% 
  select(date, constr_price_ind)

mspus = read_csv("MSPUS.csv") %>% 
  rename_lower()

msptfc = read_csv("MSPUS_cash.csv") %>% 
  rename_lower()

fmr = read_csv("FMR/FMR.csv") %>% 
  group_by(year) %>% 
  summarize(fmr_1 = mean(fmr_1))

cpi = read_csv("CPIU.csv") %>% 
  rename_lower() %>% 
  rename(cpi = cpiaucsl) %>% 
  mutate(date = ymd(date))

constr_data = sold_ind %>% 
  inner_join(mspus) %>%
  inner_join(msptfc) %>%
  inner_join(constr_ind) %>% 
  inner_join(fmr) %>% 
  inner_join(cpi) %>% 
  mutate(cpi_mspus = (mspus / cpi),
         sold_corrected = (mspus / sold_price_ind),
         cpi_sold = (sold_price_ind / cpi),
         constr_corrected = (mspus / constr_price_ind),
         sold_corrected_cash = (msptfc / sold_price_ind),
         constr_corrected_cash = (msptfc / constr_price_ind),
         fmr_corrected = (fmr_1 / constr_price_ind))

write_csv(constr_data, "CONSTR.csv")
