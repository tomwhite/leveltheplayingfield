source('maps.R')
source('schools.R')

# Load and tidy primary school data
schools_tidy <- load_primaries()
save(schools_tidy, file="data/schools_tidy.Rda")
#load("data/schools_tidy.Rda")

# Summary tables
# Superceded by general table
#tabulate_num_pupils_summary(schools_tidy, "primary", save_to_file=TRUE)
#tabulate_support_category_summary(schools_tidy, "primary", save_to_file=TRUE)
#tabulate_general_summary(schools_tidy, "primary", save_to_file=TRUE)

# Charts
plot_summary_size_distribution(schools_tidy, "primary", save_to_file = TRUE)

for (la in LOCAL_AUTHORITIES) {
  plot_pupil_funding_vs_year(schools_tidy, la, save_to_file = TRUE)
  plot_school_funding_vs_size(schools_tidy, la, save_to_file = TRUE)
  plot_pupil_funding_vs_outturn(schools_tidy, la, save_to_file = TRUE)
  plot_pupil_funding_vs_fsm(schools_tidy, la, save_to_file = TRUE)
}

# Maps

# Support category
# A single LA for a single year
map_support_categories(schools_tidy, 'Powys', 'primary', save_to_file=TRUE)

# Outturn - surplus or deficit
# A single LA
map_outturn_surplus_or_deficit_by_year(schools_tidy, 'Powys', 'primary', save_to_file=TRUE)
