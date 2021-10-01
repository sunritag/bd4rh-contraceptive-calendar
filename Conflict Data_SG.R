library(tidyverse)
library(stringr)

#loading data
conflict_a <- read.csv(file = 'conflict_a.csv')
conflict_b <- read.csv(file = 'conflict_b.csv')
data <- read.csv(file = 'Data Matching.csv')

#combining conflict data
conflict <- rbind(conflict_a, conflict_b)

#cleaning conflict data
conflict <- conflict %>%
  select(-c(source_article, source_office, source_date, 
            source_headline, source_original, where_coordinates,
            where_description))

#cleaning data matching file
data <- data %>%
  separate(Country.Year, c("country", "year"),"\\s[0-9]")
data <- data %>%
  mutate(year = case_when(
    (str_detect(data$year, "[0]\\d\\d") == TRUE) ~ paste0("2", year),
    (str_detect(data$year, "[0]\\d\\d") == FALSE) ~ paste0("1", year)),
    Survey.Datasets = ifelse(Survey.Datasets == "Download", 1, 0),
    SPA.Datasets = ifelse(SPA.Datasets == "Download", 1, 0))

#combining the two
countries_dhs <- data %>%
  distinct(country) %>%
  pull()

conflict <- conflict %>%
  filter(country %in% countries_dhs)

countries_conflict <- conflict %>%
  distinct(country) %>%
  pull()

conflict_pivot <- conflict %>%
  select(country, year) %>%
  group_by(country) %>%
  distinct(year)

country_conflict_dates <- paste(c(conflict_pivot$year)[data$country=="Angola"], collapse=", ")

data_matched_conflict <- data %>%
  filter(Type == "Standard DHS") %>%
  mutate(in_conflict = ifelse(country %in% countries_conflict == TRUE, "Yes", "No"))

#mutate(conflict_years = case_when(
 # (in_conflict == "Yes") ~ paste(c(conflict_pivot$year)[conflict_pivot$country==country], collapse=", "),
  #(in_conflict == "No") ~ "NA"
  

