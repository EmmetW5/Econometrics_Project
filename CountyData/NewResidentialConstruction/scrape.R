library(tidyverse)
library(rvest)

setwd("~/Coursework/DatEcon/Project/Data/CountyData/NewResidentialConstruction/Months")

url = "https://www2.census.gov/econ/bps/County/"

download_fname = function(fname) download.file(str_c(url, fname), fname) 

files = read_html(url) %>% 
  html_elements("a") %>% 
  as.character() %>% 
  str_extract("co[0-9]+c.txt") %>% 
  discard(is.na) %>% 
  map(slowly(download_fname, rate_delay(pause = 2)))