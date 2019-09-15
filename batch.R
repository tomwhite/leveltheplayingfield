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
