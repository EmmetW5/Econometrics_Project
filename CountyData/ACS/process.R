# Run on RStudio cluster due to ACS data being incredibly large

setwd("~/ACS")
library(tidyverse)
library(data.table)

recodeNAs = function(dt, codes){
  iwalk(codes, \(m, n) dt <<- mutate(dt, !!n := na_if(.data[[n]], m)))
  dt
}

# Must use fread to not crash
acs = fread("usa_00001.csv") %>% 
  as_tibble() %>% 
  rename_with(str_to_lower, everything()) %>% 
  # ACS is missing county data for many counties (nearly half of observations)!
  filter(!is.na(countyfip) & countyfip != 0) %>% 
  mutate(fips = str_c(str_pad(statefip, 2, "left", 0), 
                      str_pad(countyfip, 3, "left", 0))) %>% 
  select(-c(sample, serial, cbserial, hhwt, cluster, strata, gq, perwt,
            pernum, statefip, countyfip)) %>% 
  recodeNAs(list(
    "mortgage" = 0,
    "marst" = 9,
    "sex" = 2,
    "metro" = 0,
    "kitchen" = 0,
    "plumbing" = 0,
    "rooms" = 0
  )) %>% 
  mutate(metro = (metro > 1),
         debt_incurred = mortgage > 1,
         married = (marst %in% c(1, 2)),
         male = (sex == 1),
         cpi_rent = (cpi99 * rent),
         complete_plumbing = (plumbing == 20),
         has_kitchen = (kitchen > 1)
  ) %>% 
  group_by(fips, year) %>% 
  summarize(across(c(has_kitchen, complete_plumbing, debt_incurred, metro), 
                   \(x) mean(x, na.rm = T)))

write_csv(acs, "ACS.csv")
