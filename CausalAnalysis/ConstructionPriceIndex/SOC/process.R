setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/ConstructionPriceIndex/SOC")
library(tidyverse)
library(readxl)

setwd("Data")

soc = list.files() %>% 
  map(\(f) 
      if(str_ends(f, regex("xls", ignore_case = T))){
        read_xls(f)
      } else {
        read_xlsx(f)
      }
  ) %>% 
  data.table::rbindlist(fill = TRUE) %>% 
  as_tibble() %>% 
  rename_with(str_lower, everything())

soc %>% 
  filter(!is.na(lotv) & !is.na(pvalu) & !is.na(fslpr),
         fslpr != 0 & lotv != 0 & pvalu != 0,
         pvalu_f == 0 & lotv_f == 0 & fslpr_f == 0)