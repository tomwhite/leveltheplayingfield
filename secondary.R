source('maps.R')
source('schools.R')

# Load and tidy secondary school data
secondaries_raw <- load_secondary_schools_sheet()
secondaries_tidy <- tidy_raw_data(secondaries_raw)
secondaries_tidy <- add_school_locations(secondaries_tidy)

# Summary tables
tabulate_num_pupils_summary(secondaries_tidy, "secondary", save_to_file=TRUE)
tabulate_support_category_summary(secondaries_tidy, "secondary", save_to_file=TRUE)

# Charts
plot_summary_size_distribution(secondaries_tidy, "secondary", save_to_file = TRUE)

# Maps

# Find schools with no location (should be none)
secondaries_tidy %>% filter(year == '2018-19') %>% filter(is.na(Longitude))

# Support category
# All of Wales
secondaries_tidy %>%
  filter(year == '2018-19') %>%
  map_support_categories(school_type='secondary', save_to_file=TRUE)
# A single LA
secondaries_tidy %>%
  filter(year == '2018-19') %>%
  filter(local_authority == 'Powys') %>%
  map_support_categories('Powys', 'secondary', save_to_file=TRUE)
# Per LA controls
secondaries_tidy %>%
  filter(year == '2018-19') %>%
  map_support_categories_by_local_authority('secondary', save_to_file=TRUE)

# Outturn - surplus or deficit

secondaries_tidy_geo_all_years <- secondaries_tidy %>% filter(!is.na(budget_outturn)) # drop rows with no budget_outturn
secondaries_tidy_geo_all_years$surplus_or_deficit <- if_else(secondaries_tidy_geo_all_years$budget_outturn >= 0, "Black", "Red")

# All of Wales
secondaries_tidy %>%
  filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
  mutate(surplus_or_deficit = if_else(budget_outturn >= 0, "Black", "Red")) %>%
  map_outturn_surplus_or_deficit_by_year(school_type='secondary', save_to_file=TRUE)
# A single LA
secondaries_tidy %>%
  filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
  mutate(surplus_or_deficit = if_else(budget_outturn >= 0, "Black", "Red")) %>%
  filter(local_authority == 'Powys') %>%
  map_outturn_surplus_or_deficit_by_year('Powys', 'secondary', save_to_file=TRUE)

