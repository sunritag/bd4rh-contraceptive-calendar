library(tidyverse)
library(sf)
library(sp)
library(lubridate)

#load conflict data
conflict <- read.csv(file = 'conflict_coordinates.csv')

#load dhs and gps data
country <- "NGIR6A"
gpscode <- "NGGE6A"
countryName <- "Nigeria"

dhs <- readRDS(paste0("DHS_data/", countryName, "/", country, "FL.RDS"))
gps <- st_read(paste0("DHS_data/", countryName, "/", gpscode, "FL/", gpscode, "FL.shp"))

dhs_join <- left_join(dhs, gps, by = c("v001" = "DHSCLUST"))

#load regions
nigeria_regions <- st_read("DHS_data/Nigeria/shps/sdr_subnational_boundaries.shp")

#clean conflict data
conflict <- conflict %>%
  filter(country == "Nigeria" & year %in% c(2003:2018)) %>%
  mutate(month_start = month(date_start),
         month_end = month(date_end))
conflict_coords <- conflict %>%
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

#replicating for 10 km buffers
gps_10 <- st_transform(gps, 29902)
gps_10 <- st_buffer(gps_10, 10000)
gps_10_trans <- st_transform(gps_10, 2163)      # apply transformation to polygons sf
conflict_coords$cluster_10 <- apply(st_intersects(gps_10_trans, conflict_trans, sparse = FALSE), 2, 
                                    function(col) { 
                                      gps_10_trans[which(col), ]$DHSCLUST
                                    })

#replicating for 20 km buffers
gps_20 <- st_transform(gps, 29902)
gps_20 <- st_buffer(gps_20, 20000)
gps_20_trans <- st_transform(gps_20, 2163)      # apply transformation to polygons sf
conflict_coords$cluster_20 <- apply(st_intersects(gps_20_trans, conflict_trans, sparse = FALSE), 2, 
                                    function(col) { 
                                      gps_20_trans[which(col), ]$DHSCLUST
                                    })

#replicating for 15 km buffers
gps_15 <- st_transform(gps, 29902)
gps_15 <- st_buffer(gps_15, 15000)
gps_15_trans <- st_transform(gps_15, 2163)      # apply transformation to polygons sf
conflict_coords$cluster_15 <- apply(st_intersects(gps_15_trans, conflict_trans, sparse = FALSE), 2, 
                                    function(col) { 
                                      gps_15_trans[which(col), ]$DHSCLUST
                                    })

dhs_join <- dhs_join %>%
  mutate(exposed_10 = ifelse(v001 %in% unique(unlist(conflict_coords$cluster_10)), 1, 0),
         exposed_15 = ifelse(v001 %in% unique(unlist(conflict_coords$cluster_15)), 1, 0),
         exposed_20 = ifelse(v001 %in% unique(unlist(conflict_coords$cluster_20)), 1, 0)
  )

#create intensity variable
conflict_intensity <- conflict %>%
  group_by(year, month_start) %>%
  summarise(deaths = sum(best)) %>%
  rename(month = month_start)
dhs_join <- left_join(dhs_join, conflict_intensity, by = c("year", "month"))

#save final joined data
saveRDS(dhs_join, file=paste0("DHS_data/", countryName, "/", country, "FL_joined.RDS"))
