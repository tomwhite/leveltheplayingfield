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

population_countries <- population %>%
  filter(!is.na(country) & (is.na(local_authority) | country == local_authority)) %>%
  select(-c(local_authority))

population_wales <- filter_to_wales_local_authorities(population)

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

