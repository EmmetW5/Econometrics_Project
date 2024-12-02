setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/DID")
library(tidyverse)

rdc = read_csv("RDC_Inventory_Core_Metrics_Metro_History.csv") %>% 
  mutate(date = ym(month_date_yyyymm),
         year = year(date),
         month = month(date)) %>% 
  select(-c(month_date_yyyymm, month,
            ends_with("yy"), ends_with("mm"), quality_flag))

write_csv(rdc, "RDC.csv")