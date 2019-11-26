plot_population <- function(population, la, save_to_file=FALSE) {
  population_countries <- population %>%
    filter(!is.na(country) & (is.na(local_authority) | country == local_authority)) %>%
    select(-c(local_authority))
  
  population_las <- filter_to_wales_local_authorities(population)
  
  wales_pop_0 <- population_countries %>% filter(country == 'Wales') %>% pull(2)
  la_pop_0 <- population_las %>% filter(local_authority == la) %>% pull(2)
  factor <- wales_pop_0 / la_pop_0
  
  population_wales <- population_countries %>%
    filter(country == 'Wales') %>%
    gather(year, population, -c(country)) %>%
    rename(local_authority = country) # hack
  
  plot = population_las %>%
    filter(local_authority == la) %>%
    gather(year, population, -c(local_authority)) %>%
    ggplot(aes(x=year, group=local_authority)) +
    geom_line(aes(y = population), color = 'blue') +
    geom_line(data = population_wales, aes(y = population / factor), color = 'black') +
    ylab(paste("Population of", la, " (blue)")) +
    scale_y_continuous(sec.axis = sec_axis(~.*factor, name = "Population of Wales (black)")) +
    labs(title = "Population by year",
         subtitle = paste0(la, " (blue) vs. Wales (black)")) +
    theme(axis.text.x=element_text(angle = 90),
          axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, "total", "population_vs_year", NULL, ".png"))
  }
  plot
}

plot_population_with_age <- function(la, save_to_file=FALSE) {
  population_with_age %>%
    filter(local_authority == la) %>%
    ggplot(aes(y = population, x = year, group= age, color = age)) +
    geom_line() +
    ylab(paste("Population of", la)) +
    labs(title = "Population by age group and year",
         subtitle = la) +
    theme(axis.text.x=element_text(angle = 90),
          axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, "total", "population_with_age_vs_year", NULL, ".png"))
  }
  plot
}
