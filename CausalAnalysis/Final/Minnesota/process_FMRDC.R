fmr = read_csv("FMR/FMR.csv")
rdc = read_csv("RDC/RDC.csv") %>% 
  distinct(cbsa_code, year, .keep_all = T)

name_to_fips = read_csv("FMR/FMR_All_1983_2025.csv") %>% 
  select(areaname22, fips) %>% 
  mutate(fips = str_sub(fips, 1, 5)) %>% 
  distinct(areaname22, .keep_all = T)

fips_to_cbsa = read_csv("FMR/crosswalk.csv") %>% 
  mutate(fips = str_c(fipsstatecode, fipscountycode)) %>% 
  select(cbsacode, fips) %>% 
  rename(cbsa_code = cbsacode)

fmr %>% 
  left_join(name_to_fips, "areaname22") %>% 
  left_join(fips_to_cbsa, "fips") %>% 
  inner_join(rdc, c("cbsa_code", "year")) %>% 
  write_csv("FMRDC.csv")
  