library(rdhs)
library(tidyverse)
library(haven)


get_file_strings <- function(country, year) {
  #setwd(gsub(" ", "", paste("DHS_data/", country)))
  for (i in 1:length(list.files(year))){
    folder = list.files(year)[i]
    file = list.files(gsub(" ", "", paste(year, "/", folder)))[1]
    string = gsub(" ", "", paste(year, "/", folder, "/", file))
    print(string) 
  }
  
  
  
}

get_file_strings("Ghana", "2014")

GHBR72FL <- read_dta(file = "2014/GHBR72DT/GHBR72FL.DTA")
GHCR71FL <- read_dta(file = "2014/GHCR71DT/GHCR71FL.DTA")
GHHR72FL <- read_dta(file = "2014/GHHR72DT/GHHR72FL.DTA")
GHIR72FL <- read_dta(file = "2014/GHIR72DT/GHIR72FL.DTA")
GHKR72FL <- read_dta(file = "2014/GHKR72DT/GHKR72FL.DTA")
GHMR71FL <- read_dta(file = "2014/GHMR71DT/GHMR71FL.DTA")
GHPR72FL <- read_dta(file = "2014/GHPR72DT/GHPR72FL.DTA")
