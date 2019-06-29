source('schools.R')

schools_tidy <- load_all()

plot_summary_size_distribution(schools_tidy, save_to_file = TRUE)

for (la in c("Blaenau Gwent", "Ceredigion", "Glamorgan", "Monmouthshire", "Powys", "Rhondda Cynon Taf", "Torfaen", "Wrexham")) {
  plot_pupil_funding_vs_year(schools_tidy, la, save_to_file = TRUE)
  plot_school_funding_vs_size(schools_tidy, la, save_to_file = TRUE)
  plot_pupil_funding_vs_outturn(schools_tidy, la, save_to_file = TRUE)
  plot_pupil_funding_vs_fsm(schools_tidy, la, save_to_file = TRUE)
}


