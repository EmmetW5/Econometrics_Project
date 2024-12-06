setwd("~/Coursework/DatEcon/Project/Econometrics_Project/CausalAnalysis/Final/Minnesota")
library(tidyverse)

fmr = read_csv("FMRDC.csv") %>% 
  filter(year >= 2016, year < 2024) %>% 
  distinct(year, areaname22, .keep_all = T) %>% 
  rename(price = fmr_1) %>% 
  filter(areaname22 != "Milwaukee-Waukesha-West Allis, WI MSA",
         str_ends(areaname22, "Area"))

full_obs = fmr %>% 
  count(areaname22) %>% 
  filter(n == max(n)) %>% 
  pull(areaname22)

fmr = filter(fmr, areaname22 %in% full_obs)

if(length(states) > 0) fmr = filter(fmr, statename %in% states)

write_csv(fmr, "FMR_synth.csv")
