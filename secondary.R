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
plot_summary_size_distribution(schools_tidy, "secondary", save_to_file = TRUE)

# Maps

# Find schools with no location (should be none)
secondaries_tidy %>% filter(year == '2018-19') %>% filter(is.na(Longitude))

# Support category
# All of Wales
map_support_categories(secondaries_tidy %>% filter(year == '2018-19'), school_type='secondary', save_to_file=TRUE)
# A single LA
map_support_categories(secondaries_tidy %>% filter(year == '2018-19') %>% filter(local_authority == 'Powys'), 'Powys', 'secondary', save_to_file=TRUE)
# Per LA controls
map_support_categories_by_local_authority(secondaries_tidy %>% filter(year == '2018-19'), 'secondary', save_to_file=TRUE)

# Outturn - surplus or deficit

secondaries_tidy_geo_all_years <- secondaries_tidy %>% filter(!is.na(budget_outturn)) # drop rows with no budget_outturn
secondaries_tidy_geo_all_years$surplus_or_deficit <- if_else(secondaries_tidy_geo_all_years$budget_outturn >= 0, "Black", "Red")

# All of Wales
map_outturn_surplus_or_deficit_by_year(secondaries_tidy_geo_all_years, school_type='secondary', save_to_file=TRUE)
# A single LA
map_outturn_surplus_or_deficit_by_year(secondaries_tidy_geo_all_years %>% filter(local_authority == 'Powys'), 'Powys', 'secondary', save_to_file=TRUE)

