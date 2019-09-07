source('maps.R')
source('schools.R')

# Load and tidy secondary school data
secondaries_raw <- load_secondary_schools_sheet()
secondaries_tidy <- tidy_raw_data(secondaries_raw)

# Summary tables
tabulate_num_pupils_summary(secondaries_tidy, "secondary", save_to_file=TRUE)
tabulate_support_category_summary(secondaries_tidy, "secondary", save_to_file=TRUE)

# Charts
plot_summary_size_distribution(schools_tidy, "secondary", save_to_file = TRUE)

# Maps

# Load location data
# This was batch geocoded using https://www.doogal.co.uk/BatchGeocoding.php
school_locations <- load_school_locations()

# Join location data with schools (2018-19)
secondaries_tidy_geo <- secondaries_tidy %>%
  filter(year == '2018-19') %>%
  left_join(school_locations, by = c("lea_code" = "School.Number"))

# Join location data with schools (all years)
secondaries_tidy_geo_all_years <- secondaries_tidy %>%
  left_join(school_locations, by = c("lea_code" = "School.Number"))

secondaries_tidy_geo_all_years <- secondaries_tidy_geo_all_years %>% filter(!is.na(budget_outturn)) # drop rows with no budget_outturn
secondaries_tidy_geo_all_years$surplus_or_deficit <- if_else(secondaries_tidy_geo_all_years$budget_outturn >= 0, "Black", "Red")

# Support category
# All of Wales
map_support_categories(secondaries_tidy_geo, school_type='secondary', save_to_file=TRUE)
# A single LA
map_support_categories(secondaries_tidy_geo %>% filter(local_authority == 'Powys'), 'Powys', 'secondary', save_to_file=TRUE)
# Per LA controls
map_support_categories_by_local_authority(secondaries_tidy_geo, 'secondary', save_to_file=TRUE)

# Outturn - surplus or deficit
# All of Wales - probably don't want this
map_outturn_surplus_or_deficit_by_year(secondaries_tidy_geo_all_years, school_type='secondary', save_to_file=TRUE)
# A single LA
map_outturn_surplus_or_deficit_by_year(secondaries_tidy_geo_all_years %>% filter(local_authority == 'Powys'), 'Powys', 'secondary', save_to_file=TRUE)

