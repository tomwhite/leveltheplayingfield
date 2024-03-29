source('load_data.R')
source('delegation.R')
source('maps.R')
source('outturn.R')
source('peers.R')
source('population.R')
source('schools.R')

#
# Supporting or old reports - not used on website
#

# Summary tables
tabulate_general_summary(all_schools, NULL, save_to_file = TRUE)
tabulate_general_summary(all_schools, "primary", save_to_file = TRUE)
tabulate_general_summary(all_schools, "secondary", save_to_file = TRUE)
tabulate_general_summary(all_schools, "through", save_to_file = TRUE)
tabulate_general_summary(all_schools, "special", save_to_file = TRUE)

tabulate_occupancy_summary(all_schools, save_to_file = TRUE)

tabulate_per_pupil_outturn(outturn_data, "primary", save_to_file = TRUE)
tabulate_per_pupil_outturn(outturn_data, "secondary", save_to_file = TRUE)

tabulate_delegatedschoolbudgetsperpupil(delegatedschoolbudgetsperpupil, NULL, save_to_file = TRUE)
tabulate_delegatedschoolbudgetsperpupil(delegatedschoolbudgetsperpupil, "primary", save_to_file = TRUE)
tabulate_delegatedschoolbudgetsperpupil(delegatedschoolbudgetsperpupil, "secondary", save_to_file = TRUE)

tabulate_per_pupil_funding_peers_summary(all_schools, 'primary', save_to_file = TRUE)

# Charts
plot_summary_size_distribution(all_schools, "primary", save_to_file = TRUE)
plot_summary_size_distribution(all_schools, "secondary", save_to_file = TRUE)
plot_size_vs_fsm_interactive(all_schools, "primary", NULL, save_to_file = TRUE)
plot_size_vs_fsm_interactive(all_schools, "secondary", NULL, save_to_file = TRUE, num_pupils_bin_size=100, num_pupils_limit=2400)

# Maps
map_support_categories_by_local_authority(all_schools, 'secondary', save_to_file = TRUE)
map_outturn_surplus_or_deficit_by_year(all_schools, school_type='secondary', save_to_file = TRUE)
map_rural_schools(all_schools, NULL, save_to_file = TRUE)

# General charts (primary only)
primary_schools <- all_schools %>% filter(school_type == "primary")
for (la in LOCAL_AUTHORITIES) {
  print(la)

  plot_pupil_funding_vs_year(primary_schools, la, save_to_file = TRUE)
  plot_school_funding_vs_size(primary_schools, la, save_to_file = TRUE)
  plot_pupil_funding_vs_outturn(primary_schools, la, save_to_file = TRUE)
  plot_size_vs_fsm(all_schools, "primary", la, save_to_file = TRUE)
}

for (la in LOCAL_AUTHORITIES) {
  print(la)
  
  # Rural schools
  map_rural_schools(all_schools, la, save_to_file = TRUE)
}