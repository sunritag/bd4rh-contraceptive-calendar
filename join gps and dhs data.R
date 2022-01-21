library(tidyverse)
library(sf)
library(sp)
library(lubridate)

#load conflict data
conflict <- read.csv(file = 'conflict_coordinates.csv')

#load dhs and gps data
country <- "nmir51"
gpscode <- "NMGE53"
countryName <- "Namibia"

dhs <- readRDS(paste0("DHS_data/", countryName, "/", country, "FL.RDS"))
gps <- st_read(paste0("DHS_data/", countryName, "/", gpscode, "FL/", gpscode, "FL.shp"))

dhs_join <- left_join(dhs, gps, by = c("v001" = "DHSCLUST"))

#load regions
##namibia_regions <- st_read("DHS_data/Namibia/region_shps/sdr_subnational_boundaries.shp")
##ghana_regions <- st_read("DHS_data/Ghana/region_shps/sdr_subnational_boundaries.shp")

#clean conflict data
conflict <- conflict %>%
  mutate(conflict_region = case_when(
    country == "Ghana" & latitude > 10.5 ~ "Upper East",
    country == "Ghana" & latitude < 10.5 ~ "Northern",
    country == "Namibia" & longitude < 15.7 ~ "Oshana",
    country == "Namibia" & latitude < -21 ~ "Otjozondjupa",
    country == "Namibia" & longitude < 17.5 && latitude > -20 ~ "Oshikoto",
    country == "Namibia" & longitude > 22.3 ~ "Caprivi",
    TRUE ~ "Kavango"
  )) %>%
  filter((country == "Namibia" & year %in% c(1999:2002)) | 
           (country == "Ghana" & year %in% c(1991:1995))) %>%
  mutate(month_start = month(date_start),
         month_end = month(date_end))
## Ghana violence started 4/1991 and ended 4/1995
## Namibia violence started 12/1999 and ended 2/2002

#combining dhs_join and conflict
dhs_join <- dhs_join %>%
  mutate(exposed = ifelse(ADM1FIPSNA %in% c("Caprivi", "Kavango", "Northern"),
                          1, 0),
         one_yr_before = ifelse(country == "Ghana" & ((year == 1990 & month %in% c(4:12)) | (year == 1991 & month %in% c(1:3))),
                                1, 
                                ifelse(country == "Namibia" & ((year == 1998 & month == 12) | (year == 1999 & month %in% c(1:11))), 
                                       1, 0)),
         two_yr_before = ifelse(country == "Ghana" & ((year == 1989 & month %in% c(4:12)) | (year == 1990 & month %in% c(1:3))),
                                1, 
                                ifelse(country == "Namibia" & ((year == 1997 & month == 12) | (year == 1998 & month %in% c(1:11))), 
                                       1, 0)),
         onset = ifelse(country == "Ghana" & year == 1991 & month == 4,
                        1, 
                        ifelse(country == "Namibia" & year == 1999 & month == 12, 
                               1, 0)),
         during = ifelse(country == "Ghana" & ((year == 1991 & month %in% c(5:12)) | (year == 1995 & month %in% c(1:3)) | (year %in% c(1992:1994))),
                         1,
                         ifelse(country == "Namibia" & ((year %in% c(2000:2001)) | (year == 2002 & month == 1)), 
                                1, 0)),
         end = ifelse(country == "Ghana" & year == 1995 & month == 4,
                       1, 
                       ifelse(country == "Namibia" & year == 2002 & month == 2, 
                              1, 0)),
         one_yr_after = ifelse(country == "Ghana" & ((year == 1995 & month %in% c(5:12)) | (year == 1996 & month %in% c(1:4))),
                               1, 
                               ifelse(country == "Namibia" & ((year == 2002 & month %in% c(3:12)) | (year == 2003 & month %in% c(1:2))), 
                                      1, 0)),
         four_yr_after = ifelse(country == "Ghana" & ((year == 1998 & month %in% c(5:12)) | (year == 1999 & month %in% c(1:4))),
                                1, 
                                ifelse(country == "Namibia" & ((year == 2005 & month %in% c(3:12)) | (year == 2006 & month %in% c(1:2))), 
                                       1, 0))
        )

#save final joined data
saveRDS(dhs_join, file=paste0("DHS_data/", countryName, "/", country, "FL_joined.RDS"))

dhs_join_filtered <- dhs_join %>%
  select(month, year, caseid, Country.Name, ADM1FIPS, LATNUM, LONGNUM)
write.csv(dhs_join_filtered, file=paste0("DHS_data/", countryName, "/", country, "FL_joined.csv"), row.names = FALSE)
