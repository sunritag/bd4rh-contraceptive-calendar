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

##renaming countries to match the data matching file
conflict <- conflict %>%
  mutate(country = case_when(
    country == "DR Congo (Zaire)" ~ "Congo Democratic Republic", 
    country == "Ivory Coast" ~ "Cote d'Ivoire",
    country == "Kingdom of eSwatini (Swaziland)" ~ "Eswatini",
    country == "Madagascar (Malagasy)" ~ "Madagascar",
    country == "Zimbabwe (Rhodesia)" ~ "Zimbabwe",
    TRUE ~ country
  )) %>%
  arrange(year)

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

##makes a vector of all countries in the data matching file
countries_dhs <- data %>%
  distinct(country) %>%
  pull()

conflict <- conflict %>%
  filter(country %in% countries_dhs)

write.csv(conflict, "Conflict_Full_Africa.csv")

##makes a vector of all countries in the conflict data
countries_conflict <- conflict %>%
  distinct(country) %>%
  pull()

conflict_pivot <- conflict %>%
  select(country, year) %>%
  group_by(country) %>%
  distinct(year)

##countries not in conflict data but in dhs data
'%!in%' <- function(x,y)!('%in%'(x,y))
conflict_free <- data %>%
  filter(country %!in% countries_conflict) %>%
  distinct(country) %>%
  pull()

data_matched_conflict <- data %>%
  filter(Type == "Standard DHS") %>%
  filter(country %in% countries_conflict)

data_matched_conflict <- data_matched_conflict %>%
  mutate(year = str_replace_all(year, "\\d\\d[-]", ""))

#countries with the most data
most_data <- data_matched_conflict %>%
  count(country)

#conflict and data years
data_years <- data_matched_conflict %>%
  group_by(country) %>%
  summarise(first_year_data = min(as.numeric(year)),
            last_year_data = max(as.numeric(year)))
conflict_years <- conflict_pivot %>%
  group_by(country) %>%
  summarise(first_year_conflict = min(as.numeric(year)),
            last_year_conflict = max(as.numeric(year)))
years <- left_join(data_years, conflict_years)
years <- years %>%
  mutate(use = ifelse((first_year_data - 4 <= first_year_conflict) & 
           (last_year_conflict <= last_year_data), "Yes", "No"))

#country conflict episodes
conflict_by_country <- conflict %>%
  group_by(country) %>%
  summarize(year_list = list(year)) %>%
  mutate(year_list = as.character(year_list))

write.csv(conflict_by_country, "Conflict Years by Country.csv")

