setwd("~/Housing/ACS")
library(tidyverse)
library(data.table)

recodeNAs = function(dt, codes){
  iwalk(codes, \(m, n) dt <<- mutate(dt, !!n := na_if(.data[[n]], m)))
  dt
}

acs = fread("usa_00002.csv") %>% 
  as_tibble() %>% 
  rename_with(str_to_lower, everything()) %>% 
  mutate(fips = str_c(str_pad(statefip, 2, "left", 0), 
                      str_pad(countyfip, 3, "left", 0))) %>% 
  select(-c(sample, serial, cbserial, hhwt, cluster, strata, gq, perwt,
            pernum, statefip, countyfip, rent, pctmetro)) %>% 
  recodeNAs(list(
    "rooms" = 0,
    "rentgrs" = 0,
    "unitsstr" = 0,
    "builtyr2" = 0,
    "age" = 999,
    "hhincome" = 9999999,
    "valueh" = 9999998,
    "bedrooms" = 0
  )) %>% 
  mutate(metro = (metro > 1),
         debt_incurred = mortgage > 1,
         married = (marst %in% c(1, 2)),
         male = (sex == 1),
         cpi_rent = (cpi99 * rentgrs),
         complete_plumbing = (plumbing == 20),
         has_kitchen = (kitchen > 1),
         bedrooms = bedrooms - 1,
         year_from_start = year - min(year)
  ) %>% 
  select(-c(kitchen, plumbing, marst, mortgage, sex))