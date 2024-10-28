setwd("~/Coursework/DatEcon/Project/Data/CountyData/RealtorDotCom")
library(tidyverse)

rdc = read_csv("RDC_Inventory_Core_Metrics_County_History.csv") %>% 
  rename(fips = county_fips) %>% 
  mutate(date = ym(month_date_yyyymm),
         year = year(date),
         month = month(date)) %>% 
  select(-c(date, month_date_yyyymm, month, county_name,
            ends_with("yy"), ends_with("mm"), quality_flag)) %>% 
  group_by(fips, year) %>% 
  summarize(across(everything(), mean))

write_csv(rdc, "RDC.csv")