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
  select(year, type_of_violence, latitude, 
         longitude, geom_wkt, country) %>%
  filter(country %in% c("Ghana", "Zambia", "Namibia"))

write.csv(conflict, file= "conflict_coordinates.csv")