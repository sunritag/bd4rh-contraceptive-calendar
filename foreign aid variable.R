library(tidyverse)
library(sf)

foreign_aid <- read_csv("foreign_aid_data.csv") #copy

# Zimbabwe

zim <- readRDS("DHS_data/Zimbabwe/Zimbabwe_final.RDS")

zim_foreign <- foreign_aid %>% #copy, change zim_foreign to dhs and zim to dhs
  filter(Country == "Zimbabwe") %>% 
  select(-Country) %>%
  right_join(zim, by = c("Year" = "year")) %>%
  rename(foreign_aid = Value)

saveRDS(zim_foreign, file=paste0("DHS_data/Zimbabwe/Zimbabwe_final.RDS"))

# Mali

mal <- readRDS("DHS_data/Mali/Mali_final.RDS")

mal_foreign <- foreign_aid %>%
  filter(Country == "Mali") %>% 
  select(-Country) %>%
  right_join(mal, by = c("Year" = "year")) %>%
  rename(foreign_aid = Value)

saveRDS(mal_foreign, file=paste0("DHS_data/Mali/Mali_final.RDS"))

# Nigeria

