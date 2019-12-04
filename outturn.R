source('utils.R')

plot_per_pupil_outturn_vs_year <- function(outturn_data, st, la, save_to_file=FALSE) {
  # All Wales is black, LA is blue
  all_wales_per_pupil_outturn <- outturn_data %>%
    filter(school_type == st) %>%
    filter(!is.na(budget_outturn)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(year <= LATEST_OUTTURN_YEAR) %>%
    filter(local_authority == 'All')
  
  per_la_per_pupil_outturn <- outturn_data %>%
    filter(school_type == st) %>%
    filter(!is.na(budget_outturn)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(year <= LATEST_OUTTURN_YEAR) %>%
    filter(local_authority != 'All')
  
  plot = per_la_per_pupil_outturn %>%
    ggplot(aes(x=year, y=per_pupil_outturn, group=local_authority)) +
    geom_hline(yintercept = 0, color='red') +
    geom_line(alpha = 0.2) +
    geom_line(data = all_wales_per_pupil_outturn, color = 'black') +
    geom_line(data = filter(per_la_per_pupil_outturn, local_authority == la), color='blue') +
    ylab("Per-pupil budget outturn (£)") + 
    labs(title = "Per-pupil budget outturn by year",
         subtitle = paste0(la, " (blue) vs. Wales (black), ", st, " schools")) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, st, "pupil_outturn_vs_year", NULL, ".png"))
  }
  plot
}

tabulate_per_pupil_outturn <- function(outturn_data, school_type, save_to_file=FALSE) {
  
  st = school_type
  table <- outturn_data %>%
    filter(if (!is.null(st)) school_type == st else is.na(school_type)) %>%
    filter(local_authority != 'All') %>%
    filter(year == LATEST_OUTTURN_YEAR) %>%
    mutate(per_pupil_outturn_rank = rank(desc(per_pupil_outturn))) %>%
    mutate(budget_outturn = round(budget_outturn)) %>%
    mutate(per_pupil_outturn = round(per_pupil_outturn, 2)) %>%
    select(-c("school_type", "year")) %>%
    rename("Local authority" = local_authority, "Budget outturn (2018-19)" = budget_outturn, "Total pupils (2018-19)" = num_pupils, "Per-pupil budget outturn (2018-19)" = per_pupil_outturn, "Per-pupil budget outturn rank (2018-19)" = per_pupil_outturn_rank)
  
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidgetFix(dt, report_file_name(NULL, school_type, "pupil_outturn_summary", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}
