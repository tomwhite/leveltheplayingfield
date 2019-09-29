source('maps.R')
source('schools.R')

# Load school data
all_schools <- load_all_schools()

# Summary tables
tabulate_general_summary(all_schools, NULL, save_to_file=TRUE)
tabulate_general_summary(all_schools, "primary", save_to_file=TRUE)
tabulate_general_summary(all_schools, "secondary", save_to_file=TRUE)
tabulate_general_summary(all_schools, "through", save_to_file=TRUE)
tabulate_general_summary(all_schools, "special", save_to_file=TRUE)

# Charts
plot_summary_size_distribution(all_schools, "primary", save_to_file = TRUE)
plot_summary_size_distribution(all_schools, "secondary", save_to_file = TRUE)

for (la in LOCAL_AUTHORITIES) {
  # Just primary at the moment
  primary_schools <- all_schools %>% filter(school_type == "primary")
  plot_pupil_funding_vs_year(primary_schools, la, save_to_file = TRUE)
  plot_school_funding_vs_size(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_outturn(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_per_pupil_outturn(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_fsm(primary_schools, la, save_to_file = TRUE)
}

# Maps

# Support category
map_support_categories(all_schools, 'Powys', save_to_file=TRUE)

# Occupancy
map_occupancy_by_school_type(all_schools, 'Powys', save_to_file=TRUE)

# Rural schools
map_rural_schools(all_schools, 'Powys', save_to_file=TRUE)
