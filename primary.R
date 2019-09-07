source('maps.R')
source('schools.R')

# Load and tidy primary school data
schools_tidy <- load_all()
save(schools_tidy, file="data/schools_tidy.Rda")
#load("data/schools_tidy.Rda")

# Quality control

# Should give 5 rows - new schools that don't have a support category from My Local Schools
schools_tidy %>%
  filter(year == '2018-19') %>%
  filter(is.na(support_category))

# Summary tables
tabulate_num_pupils_summary(schools_tidy, "primary", save_to_file=TRUE)
tabulate_support_category_summary(schools_tidy, "primary", save_to_file=TRUE)

# Charts
plot_summary_size_distribution(schools_tidy, "primary", save_to_file = TRUE)

for (la in LOCAL_AUTHORITIES) {
  plot_pupil_funding_vs_year(schools_tidy, la, save_to_file = TRUE)
  plot_school_funding_vs_size(schools_tidy, la, save_to_file = TRUE)
  plot_pupil_funding_vs_outturn(schools_tidy, la, save_to_file = TRUE)
  plot_pupil_funding_vs_fsm(schools_tidy, la, save_to_file = TRUE)
}

# Maps

# Load location data
# This was batch geocoded using https://www.doogal.co.uk/BatchGeocoding.php
school_locations <- load_school_locations()

# Join location data with schools (2018-19)
primaries_tidy_geo <- schools_tidy %>%
  filter(year == '2018-19') %>%
  left_join(school_locations, by = c("lea_code" = "School.Number"))

# Join location data with schools (all years)
primaries_tidy_geo_all_years <- schools_tidy %>%
  left_join(school_locations, by = c("lea_code" = "School.Number"))

primaries_tidy_geo_all_years <- primaries_tidy_geo_all_years %>% filter(!is.na(budget_outturn)) # drop rows with no budget_outturn
primaries_tidy_geo_all_years$surplus_or_deficit <- if_else(primaries_tidy_geo_all_years$budget_outturn >= 0, "Black", "Red")

# Find unmatched schools
primaries_tidy_geo %>% filter(is.na(Longitude))

# Support category
# A single LA
map_support_categories(primaries_tidy_geo %>% filter(local_authority == 'Powys'), 'Powys', 'primary', save_to_file=TRUE)

# Outturn - surplus or deficit
# A single LA
map_outturn_surplus_or_deficit_by_year(primaries_tidy_geo_all_years %>% filter(local_authority == 'Powys'), 'Powys', 'primary', save_to_file=TRUE)

