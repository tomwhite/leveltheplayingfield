source('schools.R')

la_delegation_rates <- load_google_sheet('Delegation rates %') %>%
  rename(local_authority = Authority) %>%
  gather(year, delegation_rate_percent, -c(local_authority))

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

for (la in LOCAL_AUTHORITIES) {
  print(la)
  plot_delegation_rate_vs_year(la_delegation_rates, la, save_to_file = TRUE)
}
