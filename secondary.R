source('maps.R')
source('schools.R')

# Load and tidy secondary school data
secondaries_tidy <- load_secondaries()

# Summary tables
# Superceded by general table
#tabulate_num_pupils_summary(secondaries_tidy, "secondary", save_to_file=TRUE)
#tabulate_support_category_summary(secondaries_tidy, "secondary", save_to_file=TRUE)
#tabulate_general_summary(secondaries_tidy, "secondary", save_to_file=TRUE)

# Charts
plot_summary_size_distribution(secondaries_tidy, "secondary", save_to_file = TRUE)

# Maps

# Support category
# All of Wales
map_support_categories(secondaries_tidy, school_type='secondary', save_to_file=TRUE)
# Per LA controls
map_support_categories_by_local_authority(secondaries_tidy, 'secondary', save_to_file=TRUE)

# Outturn - surplus or deficit
# All of Wales
map_outturn_surplus_or_deficit_by_year(secondaries_tidy, school_type='secondary', save_to_file=TRUE)
# A single LA
map_outturn_surplus_or_deficit_by_year(secondaries_tidy, 'Powys', 'secondary', save_to_file=TRUE)
