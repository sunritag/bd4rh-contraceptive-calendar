library(tidyverse)
library(sf)

# Creating final datasets

country1 <- "NGIR7BFL_joined"
country2 <- "NGIR6AFL_joined"
country3 <- "NGIR53FL_joined"
countryName <- "Nigeria"

dhs1 <- readRDS(paste0("DHS_data/", countryName, "/", country1, ".RDS"))
dhs2 <- readRDS(paste0("DHS_data/", countryName, "/", country2, ".RDS"))
dhs3 <- readRDS(paste0("DHS_data/", countryName, "/", country3, ".RDS"))

dhs <- rbind(dhs1, dhs2, dhs3)

# Creating outcome variables
'%!in%' <- function(x,y)!('%in%'(x,y))

dhs <- dhs %>%
  mutate(abortion = ifelse(c == "T", 1, 0),
         conception = ifelse(lag(caseid) == caseid & lag(c) != "P" & c == "P", 1, 0),
         start = ifelse(
           lag(caseid, n = 1L) == caseid &
           lag(caseid, n = 2L) == caseid &
           lag(caseid, n = 3L) == caseid &
           lag(c, n = 1L) == "0" &
           lag(c, n = 2L) == "0" &
           lag(c, n = 3L) == "0" &
           c %!in% c("0", "P", "T", "B"), 
           1, 0),
         start_modern = ifelse(
           lag(caseid, n = 1L) == caseid &
           lag(caseid, n = 2L) == caseid &
           lag(caseid, n = 3L) == caseid &
           lag(c, n = 1L) == "0" &
           lag(c, n = 2L) == "0" &
           lag(c, n = 3L) == "0" &
           c %!in% c("0", "P", "T", "B") & modTrad == "modern", 
           1, 0),
         start_trad = ifelse(
           lag(caseid, n = 1L) == caseid &
             lag(caseid, n = 2L) == caseid &
             lag(caseid, n = 3L) == caseid &
             lag(c, n = 1L) == "0" &
             lag(c, n = 2L) == "0" &
             lag(c, n = 3L) == "0" &
             c %!in% c("0", "P", "T", "B") & modTrad == "traditional", 
           1, 0),
         start_short = ifelse(
           lag(caseid, n = 1L) == caseid &
             lag(caseid, n = 2L) == caseid &
             lag(caseid, n = 3L) == caseid &
             lag(c, n = 1L) == "0" &
             lag(c, n = 2L) == "0" &
             lag(c, n = 3L) == "0" &
             c %!in% c("0", "P", "T", "B") & longShort == "short", 
           1, 0),
         start_long = ifelse(
           lag(caseid, n = 1L) == caseid &
             lag(caseid, n = 2L) == caseid &
             lag(caseid, n = 3L) == caseid &
             lag(c, n = 1L) == "0" &
             lag(c, n = 2L) == "0" &
             lag(c, n = 3L) == "0" &
             c %!in% c("0", "P", "T", "B") & longShort == "long", 
           1, 0),
         shortLong_switch = ifelse(
           longShort == "long" &
           (lag(longShort, n = 1L) == "short" | 
            (lag(longShort, n = 2L) == "short" & lag(longShort, n = 1L) != "short") | 
            (lag(longShort, n = 3L) == "short" & lag(longShort, n = 2L) != "short")),
           1, 0),
         longShort_switch = ifelse(
           longShort == "short" &
             (lag(longShort, n = 1L) == "long" | 
                (lag(longShort, n = 2L) == "long" & lag(longShort, n = 1L) != "long") | 
                (lag(longShort, n = 3L) == "long" & lag(longShort, n = 2L) != "long")),
           1, 0),
         modTrad_switch = ifelse(
           modTrad == "traditional" &
             (lag(modTrad, n = 1L) == "modern" | 
                (lag(modTrad, n = 2L) == "modern" & lag(modTrad, n = 1L) != "modern") | 
                (lag(modTrad, n = 3L) == "modern" & lag(modTrad, n = 2L) != "modern")),
           1, 0),
         tradMod_switch = ifelse(
           modTrad == "modern" &
             (lag(modTrad, n = 1L) == "traditional" | 
                (lag(modTrad, n = 2L) == "traditional" & lag(modTrad, n = 1L) != "traditional") | 
                (lag(modTrad, n = 3L) == "traditional" & lag(modTrad, n = 2L) != "traditional")),
           1, 0),
         discontinuation = ifelse(
           lead(c, n = 1L) == "0" &
           lead(c, n = 2L) == "0" &
           lead(c, n = 3L) == "0" &  
           c %!in% c("0", "P", "T", "B"),
           1, 0)
         )

dhs <- dhs %>%
  filter(is.na(time) == FALSE) %>%
  filter(c != " ")


#adding foreign aid variable

foreign_aid <- read_csv("foreign_aid_data.csv")

dhs <- foreign_aid %>% 
  filter(Country == country) %>% 
  select(-Country) %>%
  right_join(dhs, by = c("Year" = "year")) %>%
  rename(foreign_aid = Value)


#save final joined data
saveRDS(dhs, file=paste0("DHS_data/", countryName, "/", countryName, "_final.RDS"))