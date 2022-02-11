library(tidyverse)
library(sf)
library(sp)
library(lubridate)

#load conflict data
conflict <- read.csv(file = 'conflict_coordinates.csv')

#load dhs and gps data
country <- "NGIR53"
gpscode <- "NGGE52"
countryName <- "Nigeria"

dhs <- readRDS(paste0("DHS_data/", countryName, "/", country, "FL.RDS"))
gps <- st_read(paste0("DHS_data/", countryName, "/", gpscode, "FL/", gpscode, "FL.shp"))

dhs_join <- left_join(dhs, gps, by = c("v001" = "DHSCLUST"))

#load regions
nigeria_regions <- st_read("DHS_data/Nigeria/shps/sdr_subnational_boundaries.shp")

#clean conflict data
conflict <- conflict %>%
  filter(country == "Nigeria" & year %in% c(2003:2018))
conflict_coords <- conflict %>%
  mutate(month_start = month(date_start),
         month_end = month(date_end)) %>%
  select(longitude, latitude)

#add region variable to conflict data
## create a points collection
conflict_coords_sf <- do.call("st_sfc",c(lapply(1:nrow(conflict_coords), 
                                                function(i) {st_point(as.numeric(conflict_coords[i, ]))}), list("crs" = 4326))) 

conflict_trans <- st_transform(conflict_coords_sf, 2163) # apply transformation to pnts sf
nigeria_regions_trans <- st_transform(nigeria_regions, 2163)      # apply transformation to polygons sf

## intersect and extract state name
conflict_coords$region <- apply(st_intersects(nigeria_regions_trans, conflict_trans, sparse = FALSE), 2, 
                                function(col) { 
                                  nigeria_regions_trans[which(col), ]$DHSREGEN
                                })
conflict_coords$region <- toupper(conflict_coords$region)

#combining dhs_join and conflict
dhs_join <- dhs_join %>%
  mutate(exposed = ifelse(toupper(ADM1NAME) %in% unique(conflict_coords$region), 1, 0))

#save final joined data
saveRDS(dhs_join, file=paste0("DHS_data/", countryName, "/", country, "FL_joined.RDS"))


