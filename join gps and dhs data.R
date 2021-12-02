library(tidyverse)
library(sf)

country <- "ZMIR71"
gpscode <- "ZMGE71"
countryName <- "Zambia"

dhs <- readRDS(paste0("DHS_data/", countryName, "/", country, "FL.RDS"))
gps <- st_read(paste0("DHS_data/", countryName, "/", gpscode, "FL/", gpscode, "FL.shp"))