library(tidyverse)
setwd("~/Coursework/DatEcon/Project/Data/CountyData/NewResidentialConstruction")

files = str_c("Months/", list.files("Months"))
tbls = map(files, \(f) quietly(read_csv)(f, skip = 1)$result)

NRC = tbls %>% 
  reduce(bind_rows) %>% 
  mutate(year = str_sub(Date, 1, 4),
         month = str_sub(Date, 5, 6),
         fips = str_c(State, County),
         total_units = rowSums(select(., starts_with("Units")))) %>% 
  select(year, month, fips, total_units) %>% 
  group_by(fips, year) %>%
  summarize(total_units = sum(total_units))

write_csv(NRC, "NRC.csv")
