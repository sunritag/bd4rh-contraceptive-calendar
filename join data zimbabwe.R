library(tidyverse)
library(sf)
library(sp)
library(lubridate)

#load conflict data
conflict <- read.csv(file = 'conflict_coordinates.csv')

#load dhs and gps data
country <- "ZWIR62"
gpscode <- "ZWGE61"
countryName <- "Zimbabwe"

dhs <- readRDS(paste0("DHS_data/", countryName, "/", country, "FL.RDS"))
gps <- st_read(paste0("DHS_data/", countryName, "/", gpscode, "FL/", gpscode, "FL.shp"))

dhs_join <- left_join(dhs, gps, by = c("v001" = "DHSCLUST"))

#load regions
##mali_regions <- st_read("DHS_data/Ghana/shps/sdr_subnational_boundaries.shp")
zimbabwe_regions <- st_read("DHS_data/Zimbabwe/shps/sdr_subnational_boundaries.shp")

#clean conflict data
conflict <- conflict %>%
  filter(country == "Zimbabwe" & year %in% c(2007, 2008, 2009))
#conflict <- st_as_sf(conflict, coords = c('latitude', 'longitude'), crs = st_crs(gps))
conflict_coords <- conflict %>%
  mutate(month_start = month(date_start),
         month_end = month(date_end)) %>%
  select(longitude, latitude)
##Zimbabwe started 4/2007 and ended 8/2009

#add region variable to conflict data
## create a points collection
conflict_coords_sf <- do.call("st_sfc",c(lapply(1:nrow(conflict_coords), 
                                     function(i) {st_point(as.numeric(conflict_coords[i, ]))}), list("crs" = 4326))) 

conflict_trans <- st_transform(conflict_coords_sf, 2163) # apply transformation to pnts sf
zimbabwe_regions_trans <- st_transform(zimbabwe_regions, 2163)      # apply transformation to polygons sf

## intersect and extract state name
conflict_coords$region <- apply(st_intersects(zimbabwe_regions_trans, conflict_trans, sparse = FALSE), 2, 
                     function(col) { 
                       zimbabwe_regions_trans[which(col), ]$DHSREGEN
                     })
conflict_coords$region <- toupper(conflict_coords$region)


#combining dhs_join and conflict
dhs_join <- dhs_join %>%
  mutate(exposed = ifelse(toupper(ADM1NAME) %in% unique(conflict_coords$region), 1, 0),
         three_yr_before = ifelse((year %in% c(2004:2005) & month %in% c(4:12)) | (year %in% c(2005:2006) & month %in% c(1:3)),
                                  1, 0),
         one_yr_before = ifelse((year == 2006 & month %in% c(4:12)) | (year == 2007 & month %in% c(1:3)),
                                1, 0),
         onset = ifelse(year == 2007 & month == 4, 1, 0),
         during = ifelse((year == 2007 & month %in% c(5:12)) | year == 2008 | (year == 2009 & month %in% c(1:7)),
                         1, 0),
         end = ifelse(year == 2009 & month == 8, 1, 0),
         one_yr_after = ifelse((year == 2009 & month %in% c(9:12)) | (year == 2010 & month %in% c(1:8)),
                               1, 0),
         four_yr_after = ifelse((year %in% c(2010:2012) & month %in% c(9:12)) | (year %in% c(2011:2013) & month %in% c(1:8)),
                                1, 0))

#save final joined data
saveRDS(dhs_join, file=paste0("DHS_data/", countryName, "/", country, "FL_joined.RDS"))


