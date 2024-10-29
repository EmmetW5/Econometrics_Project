setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/ConstructionPriceIndex")
library(tidyverse)
library(readxl)

constr_ind = read_xlsx("price_sold_cust.xlsx") %>% 
  select(Year, starts_with("Q")) %>% 
  pivot_longer(starts_with("Q"), names_to = "Quarter", 
               values_to = "constr_price_ind") %>% 
  mutate(Quarter = str_extract(Quarter, "[0-9]$") %>% as.numeric(),
         date_string = str_c(Year, " ", (Quarter-1)*3 + 1),
         date = ym(date_string)) %>% 
  select(-date_string) %>% 
  rename_with(str_to_lower, everything()) %>% 
  na.omit()

mspus = read_csv("MSPUS.csv") %>% 
  rename_with(str_to_lower, everything())

base_index = constr_ind %>% 
  filter(year == 2005, quarter == 1) %>% 
  pull(constr_price_ind)

permit_values = read_xlsx("building_permits.xlsx") %>% 
  rename_with(make.names, unique = TRUE) %>% 
  select(State.Name, Survey.Date, ends_with("rep")) %>% 
  rename_with(\(n) str_extract(n, ".+[^(\\.rep)]")) %>% 
  group_by(Survey.Date) %>% 
  summarize(units = sum(Single.family.units),
            value = sum(Single.family.value)) %>% 
  mutate(average_value = value / units,
         date_string = str_c("01/01/", Survey.Date),
         date = mdy(date_string))

constr_data = inner_join(constr_ind, mspus) %>% 
  mutate(corrected_price = (mspus / constr_price_ind) / base_index)

write_csv(constr_data, "CONSTR.csv")
