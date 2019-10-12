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
  print(la)
  # Just primary at the moment
  primary_schools <- all_schools %>% filter(school_type == "primary")
  plot_pupil_funding_vs_year(primary_schools, la, save_to_file = TRUE)
  plot_school_funding_vs_size(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_outturn(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_per_pupil_outturn(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_fsm(primary_schools, la, save_to_file = TRUE)

  plot_school_vs_budget_outturn_change(all_schools, 'primary', la, save_to_file = TRUE)
  plot_school_vs_budget_outturn_change(all_schools, 'secondary', la, save_to_file = TRUE)
  
  plot_support_catagory_vs_year(all_schools, 'primary', la, save_to_file = TRUE)
  plot_support_catagory_vs_year(all_schools, 'secondary', la, save_to_file = TRUE)
}


# Maps

# All of Wales

# Support category
map_support_categories_by_local_authority(all_schools, 'secondary', save_to_file=TRUE)

# Outturn - surplus or deficit
map_outturn_surplus_or_deficit_by_year(all_schools, school_type='secondary', save_to_file=TRUE)

# Per LA

for (la in LOCAL_AUTHORITIES) {
  print(la)
  
  map_occupancy_by_school_type(all_schools, la, save_to_file=TRUE)
  
  map_outturn_surplus_or_deficit_by_school_type(all_schools, la, save_to_file=TRUE)

  map_support_categories(all_schools, la, save_to_file=TRUE)
  
  map_language_by_school_type(all_schools, la, save_to_file=TRUE)
  
  map_rural_schools(all_schools, la, save_to_file=TRUE)
}

