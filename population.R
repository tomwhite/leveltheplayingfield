load_population_data <- function(csv = "data/population-estimates-by-local-authority-and-year.csv") {
  # Source: https://statswales.gov.wales/Catalogue/Population-and-Migration/Population/Estimates/Local-Authority/populationestimates-by-localauthority-year
  read_csv(csv) %>%
    rename(country = X3, local_authority = X4) %>%
    rename_all(gsub, pattern = '^Mid-year (.+)$', replacement = '\\1') %>%
    select(-c(X1, X2))
}

filter_to_wales_local_authorities <- function(all_population) {
  all_population %>%
    filter(!is.na(local_authority) & local_authority != 'Scotland' & local_authority != 'Northern Ireland') %>%
    select(-c(country))
}

population <- load_population_data()

# Plot of all LAs by year
filter_to_wales_local_authorities(population) %>%
  gather(year, population, -c(local_authority)) %>%
  ggplot(aes(x=year, y=population, group=local_authority)) +
  geom_line()

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
         subtitle = paste0(la, " (blue) vs. Wales (black), ")) +
    theme(axis.text.x=element_text(angle = 90),
          axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, "total", "population_vs_year", NULL, ".png"))
  }
  plot
}

for (la in LOCAL_AUTHORITIES) {
  print(la)
  plot_population(population, la, save_to_file = TRUE)
}

population_0_15 <- load_population_data("data/population-estimates-by-local-authority-and-year-0-15.csv") %>%
  filter_to_wales_local_authorities() %>%
  gather(year, population, -c(local_authority)) %>%
  mutate(age = '0-15')
population_16_64 <- load_population_data("data/population-estimates-by-local-authority-and-year-16-64.csv") %>%
  filter_to_wales_local_authorities() %>%
  gather(year, population, -c(local_authority)) %>%
  mutate(age = '16-64')
population_65_plus <- load_population_data("data/population-estimates-by-local-authority-and-year-65-and-over.csv") %>%
  filter_to_wales_local_authorities() %>%
  gather(year, population, -c(local_authority)) %>%
  mutate(age = '65+')

population_with_age <- population_0_15 %>%
  union(population_16_64) %>%
  union(population_65_plus)

# See http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html
population_with_age %>%
  filter(local_authority == 'Powys') %>%
  ggplot(aes(y = population, x = year, fill = age)) +
  geom_bar(stat = 'identity')

