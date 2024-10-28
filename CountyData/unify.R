library(tidyverse)
setwd("~/Coursework/DatEcon/Project/Data/CountyData")

table_files = list(
  "fmr" = "FMR/FMR.csv",
  "rdc" = "RealtorDotCom/RDC.csv",
  "nrc" = "NewResidentialConstruction/NRC.csv",
  "acs" = "ACS/ACS.csv"
)

counties = table_files %>% 
  map(read_csv) %>% 
  map(\(x) mutate(x, fips = as.character(fips))) %>% 
  reduce(inner_join, c("fips", "year"))
