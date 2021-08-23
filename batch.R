source('load_data.R')
source('delegation.R')
source('maps.R')
source('outturn.R')
source('peers.R')
source('population.R')
source('schools.R')

#
# Website reports
#

# All of Wales

# Population Trends
map_occupancy_by_school_type(all_schools, NULL, save_to_file = TRUE)
map_occupancy_by_school_size(all_schools, 'primary', NULL, save_to_file = TRUE)
map_occupancy_by_school_size(all_schools, 'secondary', NULL, save_to_file = TRUE)
map_occupancy_by_school_size(all_schools, 'through', NULL, save_to_file = TRUE)
map_occupancy_by_school_capacity(all_schools, 'primary', NULL, save_to_file = TRUE)
map_occupancy_by_school_capacity(all_schools, 'secondary', NULL, save_to_file = TRUE)
map_occupancy_by_school_capacity(all_schools, 'through', NULL, save_to_file = TRUE)

map_occupancy_by_local_authority(all_schools, save_to_file = TRUE)
map_occupancy(all_schools, NULL, save_to_file = TRUE)

# School Funding
map_per_pupil_funding(all_schools, 'primary', save_to_file = TRUE)
map_per_pupil_funding(all_schools, 'secondary', save_to_file = TRUE)

# School Finance
map_outturn_surplus_or_deficit_by_school_type(all_schools, NULL, save_to_file = TRUE)
map_outturn_surplus_or_deficit_by_local_authority(all_schools, save_to_file = TRUE)
map_outturn_surplus_or_deficit(all_schools, NULL, save_to_file = TRUE)

# School Support Categories
map_support_categories_by_school_type(all_schools, NULL, save_to_file = TRUE)
map_support_categories_by_local_authority(all_schools, NULL, save_to_file = TRUE)
map_support_categories(all_schools, NULL, save_to_file = TRUE)

# FSM Rates
map_fsm_by_local_authority(all_schools, save_to_file = TRUE)

# Welsh Language Provision
map_language_by_local_authority(all_schools, save_to_file = TRUE)
map_language_by_school_type(all_schools, NULL, save_to_file = TRUE)
map_language(all_schools, NULL, save_to_file = TRUE)


# Local authorities

for (la in LOCAL_AUTHORITIES) {
  print(la)

  # Population Trends
  plot_population(population, la, save_to_file = TRUE)
  plot_population_with_age(la, save_to_file = TRUE)
  map_occupancy_by_school_type(all_schools, la, save_to_file = TRUE)
  map_occupancy_by_school_size(all_schools, 'primary', la, save_to_file = TRUE)
  map_occupancy_by_school_size(all_schools, 'secondary', la, save_to_file = TRUE)
  map_occupancy_by_school_capacity(all_schools, 'primary', la, save_to_file = TRUE)
  map_occupancy_by_school_capacity(all_schools, 'secondary', la, save_to_file = TRUE)
  
  # School Funding
  plot_delegation_rate_vs_year(la_delegation_rates, la, save_to_file = TRUE)
  plot_delegatedschoolbudgetsperpupil_all_school_types(delegatedschoolbudgetsperpupil, la, save_to_file = TRUE)
  plot_delegatedschoolbudgetsperpupil_per_school_type(delegatedschoolbudgetsperpupil, la, 'primary', save_to_file = TRUE)
  plot_delegatedschoolbudgetsperpupil_per_school_type(delegatedschoolbudgetsperpupil, la, 'secondary', save_to_file = TRUE)
  map_per_pupil_funding(all_schools, 'primary', la, save_to_file = TRUE)
  map_per_pupil_funding(all_schools, 'secondary', la, save_to_file = TRUE)
  
  # School Finance
  plot_pupil_funding_vs_per_pupil_outturn(all_schools, 'primary', la, save_to_file = TRUE)
  map_outturn_surplus_or_deficit_by_school_type(all_schools, la, save_to_file = TRUE)
  plot_per_pupil_outturn_vs_year(outturn_data, 'primary', la, save_to_file = TRUE)
  plot_per_pupil_outturn_vs_year(outturn_data, 'secondary', la, save_to_file = TRUE)
  
  # School Support Categories
  map_support_categories_by_school_type(all_schools, la, save_to_file = TRUE)
  plot_support_catagory_vs_year(all_schools, 'primary', la, save_to_file = TRUE)
  plot_support_catagory_vs_year(all_schools, 'secondary', la, save_to_file = TRUE)
  
  # FSM Rates
  plot_pupil_funding_vs_fsm(all_schools, 'primary', la, save_to_file = TRUE)
  map_fsm_by_school_type(all_schools, la, save_to_file = TRUE)
  
  # Welsh Language Provision
  map_language_by_school_type(all_schools, la, save_to_file = TRUE)

}
