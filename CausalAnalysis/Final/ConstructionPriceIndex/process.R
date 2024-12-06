setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/Final/ConstructionPriceIndex")
library(tidyverse)
library(readxl)

rename_lower = function(x) rename_with(x, str_to_lower, everything())
yq_to_dates = function(x) {
  x %>% 
    mutate(date_string = str_c(year, " ", quarter),
           date = yq(date_string)) %>% 
    select(-c(date_string, quarter))
}

sold_ind = read_xlsx("price_sold_cust.xlsx") %>% 
  select(Year, starts_with("Q")) %>% 
  pivot_longer(starts_with("Q"), names_to = "Quarter", 
               values_to = "sold_price_ind") %>% 
  rename_lower() %>% 
  yq_to_dates() %>% 
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

hpi = read_csv("hpi_at_state.csv") %>% 
  rename_lower() %>% 
  group_by(year, quarter) %>% 
  summarize(hpi = mean(hpi, na.rm = T)) %>%
  ungroup() %>% 
  yq_to_dates()
  
constr_data = sold_ind %>% 
  inner_join(mspus) %>%
  inner_join(msptfc) %>%
  inner_join(constr_ind) %>% 
  inner_join(fmr) %>% 
  inner_join(cpi) %>% 
  inner_join(hpi) %>% 
  mutate(cpi_mspus = (mspus / cpi),
         sold_corrected = (mspus / sold_price_ind),
         cpi_sold = (sold_price_ind / cpi),
         constr_corrected = (mspus / constr_price_ind),
         sold_corrected_cash = (msptfc / sold_price_ind),
         constr_corrected_cash = (msptfc / constr_price_ind),
         fmr_corrected = (fmr_1 / constr_price_ind))

write_csv(constr_data, "CONSTR.csv")
