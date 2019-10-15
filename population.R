load_population_data <- function() {
  # Source: https://statswales.gov.wales/Catalogue/Population-and-Migration/Population/Estimates/Local-Authority/populationestimates-by-localauthority-year
  read_csv("data/population-estimates-by-local-authority-and-year.csv") %>%
    rename(country = X3, local_authority = X4) %>%
    rename_all(gsub, pattern = '^Mid-year (.+)$', replacement = '\\1') %>%
    select(-c(X1, X2))
}

population <- load_population_data()

population_countries <- population %>%
  filter(!is.na(country) & (is.na(local_authority) | country == local_authority)) %>%
  select(-c(local_authority))

population_wales <- population %>%
  filter(!is.na(local_authority) & local_authority != 'Scotland' & local_authority != 'Northern Ireland') %>%
  select(-c(country))

# Plot of all LAs by year
population_wales %>%
  gather(year, population, -c(local_authority)) %>%
  ggplot(aes(x=year, y=population, group=local_authority)) +
  geom_line()

plot_population <- function(la, save_to_file=FALSE) {
  first_value <- population_countries %>% filter(country == 'Wales') %>% pull(2)
  
  # TODO: rename
  x <- population_countries %>% filter(country == 'Wales') %>%
    mutate_if(is.numeric, list(~ ./ first_value * 100)) %>%
    gather(year, population, -c(country)) %>%
    rename(local_authority = country) # hack
  
  first_value <- population_wales %>% filter(local_authority == la) %>% pull(2)
  y <- population_wales %>% filter(local_authority == la) %>%
    mutate_if(is.numeric, list(~ ./ first_value * 100)) %>%
    gather(year, population, -c(local_authority))
  
  y %>%
    ggplot(aes(x=year, y=population, group=local_authority)) +
    geom_line() +
    geom_line(data = x, color = 'green')
}


plot_population("Denbighshire")
