plot_delegation_rate_vs_year <- function(la_delegation_rates, la, save_to_file=FALSE) {
  # All Wales is black, LA is blue
  all_wales_delegation_rate_percent <- la_delegation_rates %>%
    group_by(year) %>%
    summarize(delegation_rate_percent=mean(delegation_rate_percent)) %>%
    mutate(local_authority = 'All')
  
  plot = la_delegation_rates %>%
    ggplot(aes(x=year, y=delegation_rate_percent, group=local_authority)) +
    geom_hline(yintercept = 85, color='green') +
    geom_line(alpha = 0.2) +
    geom_line(data = all_wales_delegation_rate_percent, color = 'black') +
    geom_line(data = filter(la_delegation_rates, local_authority == la), color='blue') +
    ylab("Delegation rate (%)") + 
    labs(title = "Delegation rate by year",
         subtitle = paste0(la, " (blue) vs. Wales (black)")) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, NULL, "delegation_rate_vs_year", NULL, ".png"))
  }
  plot
}

plot_delegatedschoolbudgetsperpupil_all_school_types <- function(delegatedschoolbudgetsperpupil, la, save_to_file=FALSE) {
  # All Wales is black, LA is blue
  plot = delegatedschoolbudgetsperpupil %>%
    filter(is.na(school_type)) %>%
    filter(local_authority != 'All') %>%
    ggplot(aes(x=year, y=delegated_school_budget_per_pupil, group=local_authority)) +
    geom_line(alpha = 0.2) +
    geom_line(data = filter(delegatedschoolbudgetsperpupil, local_authority == 'All'), color = 'black') +
    geom_line(data = filter(delegatedschoolbudgetsperpupil, local_authority == la, is.na(school_type)), color='blue') +
    ylab("Delegated school budget per pupil (£)") + 
    labs(title = "Delegated school budget per pupil by year, all school types",
         subtitle = paste0(la, " (blue) vs. Wales (black)")) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, NULL, "delegated_school_budget_per_pupil_vs_year", NULL, ".png"))
  }
  plot
}

plot_delegatedschoolbudgetsperpupil_per_school_type <- function(delegatedschoolbudgetsperpupil, la, school_type, save_to_file=FALSE) {
  st <- school_type
  
  plot = delegatedschoolbudgetsperpupil %>%
    filter(school_type == st) %>%
    filter(local_authority != 'All') %>%
    ggplot(aes(x=year, y=delegated_school_budget_per_pupil, group=local_authority)) +
    geom_line(alpha = 0.2) +
    geom_line(data = filter(delegatedschoolbudgetsperpupil, local_authority == la, school_type == st), color='blue') +
    ylab("Delegated school budget per pupil (£)") + 
    labs(title = "Delegated school budget per pupil by year",
         subtitle = paste0(la, " (blue), ", st, " schools")) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, st, "delegated_school_budget_per_pupil_vs_year", NULL, ".png"))
  }
  plot
}

tabulate_delegatedschoolbudgetsperpupil <- function(delegatedschoolbudgetsperpupil, school_type, save_to_file=FALSE) {
  
  st = school_type
  table <- delegatedschoolbudgetsperpupil %>%
    filter(if (!is.null(st)) school_type == st else is.na(school_type)) %>%
    filter(local_authority != 'All') %>%
    filter(year == LATEST_NUM_PUPILS_YEAR) %>%
    mutate(delegated_school_budget_per_pupil_rank = rank(desc(delegated_school_budget_per_pupil))) %>%
    mutate_at(c('delegated_school_budget_per_pupil'), round) %>%
    select(-c(school_type, year)) %>%
    rename("Local authority" = local_authority, "Delegated school budget per-pupil (2019-20)" = delegated_school_budget_per_pupil, "Delegated school budget per-pupil rank (2019-20)" = delegated_school_budget_per_pupil_rank)

  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidgetFix(dt, report_file_name(NULL, school_type, "delegated_school_budget_summary", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}
