
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# This script reads in raw DHS data and cleans the calendar into an event file
# Code written by Amy Finnegan, amy.finnegan@gmail.com
# .............................................................................
#
# KEY:
# sections        ::::
# major headings  ====
# minor headings  ----
#
# MAJOR OUTPUT: 
# input/clean/datL.Rda - long event file
# .............................................................................
# @todo
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



#libraries ===================================================================
library(foreign)
library(tidyverse)
library(magrittr)
library(stringr)
library(reshape2)
library(haven)


# functions ===================================================================
# rounding functions to keep trailing zeros -----------------------------------
rd0 <- function(y) sprintf("%.0f", round(y, 0))
rd1 <- function(y) sprintf("%.1f", round(y, 1))
rd2 <- function(y) sprintf("%.2f", round(y, 2))
rd3 <- function(y) sprintf("%.3f", round(y, 3))



# read in data ===============================================================
#BOX_PATH <- "C:/Users/amyfi/Box/"

### CHANGE - file name (Country) and country name
country <- "NGIR7B"
countryName <- "Nigeria"

# read in the IR data file from your country of interest
# dhs <- read.dta(unzip(paste0(BOX_PATH, "/bd4rh-data/RawZIPFiles/", country, "DT.ZIP"),
#                         files = paste0(country, "FL.DTA")))
dhs <- read_dta(paste0("DHS_data/", countryName, "/", country, "FL.DTA"))

dhs$vcal_2=str_pad(dhs$vcal_2, width=80, side="right") # make reasons 80 char


# split calendar into sep variables
# label as months

dems <- 
  dhs %>%
  select(caseid, # id 
         v000, # country code
         v001, # cluster location
         v005, # weight
         v007, # year of interview
         v008, # cmc date of interview
         v011, # cmc date of birth of woman
         v012, # age of woman on survey date
         v013, # age brackets
         v017, # cmc cal start
         v018, # row of first calendar entry
         v019, # length of cal
         v024, # province
         v025, # urban/rural
         v149, # ed brackets
         v190, # wealth quintile
         v211, # cmc of first birth to woman
         v212, # age of resp at first birth
         v501, # current marital status
         v624, # unmet need variable (at time of survey)
         vcal_1, # cal string
         vcal_2) # reasons for discontinuation, truncated



# the calendar history --------------------------------------------------------
# gives a list of each ind split into months
# note: if not all respondents had a calendar, they are missing
ch <- strsplit(dhs$vcal_1, "")
names(ch) <- as.character(dems$caseid)
#ch <- as.data.frame(do.call(rbind, ch))
ch <- data.frame(do.call(rbind, ch))
names(ch) <- paste0("m", 1:length(ch))
ch$caseid <- rownames(ch)


# long data
cLong <-
  ch %>%
  select("caseid", starts_with("m")) %>%
  gather(m, c, -caseid)

# the discontinuation history ------------------------------------------------
# the dicontinuation vars

dh <- strsplit(dhs$vcal_2, "")
names(dh) <- dhs$caseid
#dh <- as.data.frame(do.call(rbind, dh))
dh <- data.frame(do.call(rbind, dh))
names(dh) <- paste0("m", 1:length(dh))
dh$caseid <- rownames(dh)

# long data (more obs b/c it keeps all blank strings)
dLong <-
  dh %>%
  select("caseid", starts_with("m")) %>%
  gather(m, d, -caseid)


# relabel month to be cmc (only in cLong b/c you're going to combine)

# add in cmc date by starting at last day in calendar and counting up
cLong$cmc <- NA # need NA column
cmcImpute <- unique(dhs$v017) # m80==cmcImpute

# m80 is first day of calendar, count backwards to present month-year
for (m in paste0("m", 80:1)) {
  cLong$cmc[cLong$m==m] <- cmcImpute # when month is mX, cmc = cmcImpute
  #print(cbind(m, cmcImpute)) # used for checking
  cmcImpute <- cmcImpute + 1 # counts up from last day by 1
}

# check that date of survey matches first date of calendar
table(dhs$v008, dhs$v018)  

# add month and year from cmc
# http://demographicestimation.iussp.org/content/dhs-century-month-codes
cLong$year <- 1900 + as.integer((cLong$cmc-1)/12)
cLong$month <- cLong$cmc - 12*(cLong$year-1900)

# combine c and d history
datL <-
  cLong %>%
  left_join(., dLong, by=c("caseid", "m")) %>%
  mutate(m=ifelse(nchar(m)==2, paste0("m0", substr(m, 2, 2)), m)) # add leading zeros

# labels ----------------------------------------------------------------------

# M does not appear in the key, I have labeled it a short, modern method

# change names so that they are long vs. short
long <- c(2, 6, 7)
short <- c(1, 3, 4, 5, 8, 9, "W",
           "N", "A", "L", "C", "F", "M", "S", "E")
none <- c(0)
pregBirth <- c("P", "T", "B")

# change names so that they are modern/traditional/birth/none
# modern vs. traditional
modern <- c(1, 2, 3, 4, 5, 6, 7, "N", "C", "F", "M", "S", "E")
traditional <- c(8, 9, "W", "A", "L")
none <- c(0)
pregBirth <- c("P", "T", "B")

datL <-
  datL %>%
  mutate(longShort=ifelse(c %in% long, "long",
                          ifelse(c %in% short, "short",
                                 ifelse(c %in% pregBirth, "Birth/Pregnacy", "none")))) %>%
  mutate(modTrad=ifelse(c %in% modern, "modern",
                        ifelse(c %in% traditional, "traditional",
                               ifelse(c %in% pregBirth, "Birth/Pregnancy", "none")))) %>%
  mutate(cText=factor(c,
                      levels <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                  "W", "N", "A", "L", "C", "F", 
                                  "P", "T", "B", "M", "S", "E"),
                      labels <- c("Non-use",
                                  "Pill",
                                  "IUD",
                                  "Injections",
                                  "Diaphragm",
                                  "Condom",
                                  "Female Sterilization",
                                  "Male Sterilization",
                                  "Periodic Abstinence/rhythm",
                                  "Withdrawal",
                                  "Other trad'l methods",
                                  "Norplant",
                                  "Abstinence",
                                  "LAM",
                                  "Female condom",
                                  "Foam & Jelly",
                                  "Pregnancy",
                                  "Terminated pregnancy/non-live birth",
                                  "Birth",
                                  "Country-specific short, modern",
                                  "Standard Days Method",
                                  "Emergency Contraception"))) %>%
  mutate(dText=factor(d,
                      levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                               "C", "F", "A", "D", "W", "K", "?"),
                      labels=c("Became pregnant while using",
                               "Wanted to become pregnant",
                               "Husband disapproved",
                               "Side effects",
                               "Health concerns",
                               "Access/availability",
                               "Wanted more effective method",
                               "Inconvenient to use",
                               "Infrequent sex/husband away",
                               "Cost",
                               "Fatalistic",
                               "Difficult to get pregnant/menopause",
                               "Marital dissolution",
                               "Other reasons",
                               "Don't know",
                               "Country-specific reason"))) %>%
  mutate(cTextReasonD=ifelse(!is.na(as.character(dText)), 
                             paste0("Quit ", as.character(cText), ": ", as.character(dText)),
                             as.character(cText)))

# merge on demographics
datL <-
  datL %>%
  left_join(., dems, by=c("caseid"))

# construct age at each month
datL <-
  datL %>%
  mutate(age=trunc((cmc-v011)/12)) # age in each month

# total recorded births at each month (not parity exactly)
datL <-
  datL %>%
  mutate(parity=ifelse(c=="B", 1, 0)) %>%
  group_by(caseid) %>%
  arrange(caseid, cmc) %>%
  mutate(parity=cumsum(parity))

print(typeof(datL))
# tag postpartum period
datL <-
  datL %>%
  group_by(caseid, parity) %>%
  mutate(riskPeriod=dplyr::row_number()) # 1 is the month of the birth


datL$Country.Name <- countryName

# save cleaned data
rm(ch, cLong, dems, dh, dLong)
saveRDS(datL, file=paste0("DHS_data/", countryName, "/", country, "FL.RDS"))

