source('maps.R')
source('schools.R')

# Load and tidy primary school data
schools_tidy <- load_primaries()
schools_tidy <- add_school_locations(schools_tidy)
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

# Find schools with no location
schools_tidy %>% filter(year == '2018-19') %>% filter(is.na(longitude))

# Support category
# A single LA for a single year
map_support_categories(schools_tidy, 'Powys', 'primary', save_to_file=TRUE)

# Outturn - surplus or deficit
# A single LA
map_outturn_surplus_or_deficit_by_year(schools_tidy, 'Powys', 'primary', save_to_file=TRUE)
