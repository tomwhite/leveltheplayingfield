# Import and tidy all datasets

library(tidyverse)

source('utils.R')

load_google_sheet_locally <- function(title) {
  d <- file.path("data", "sheets")
  path <- file.path(d, paste0(title, ".Rda"))
  readRDS(path)
}

# Schools data

load_primaries <- function() {
  school_spreadsheets = list()
  i <- 1
  for (local_authority in LOCAL_AUTHORITIES) {
    school_spreadsheets[[i]] <- load_google_sheet_locally(paste(local_authority, "Primary Schools")) %>%
      add_support_categories_2019() %>%
      tidy_raw_data()
    i <- i + 1
  }
  schools_tidy <- add_school_locations(bind_rows(school_spreadsheets)) %>%
    mutate(school_type = 'primary')
  
  # QC
  # Should give 5 rows - new schools that don't have a support category from My Local Schools
  print("Schools that don't have a support category")
  schools_tidy %>%
    filter(year == LATEST_SUPPORT_CATEGORY_YEAR) %>%
    filter(is.na(support_category)) %>%
    print()
  
  # Find schools with no location
  print("Primary schools with no location")
  schools_tidy %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
  
  schools_tidy
}

load_secondaries <- function() {
  secondaries_tidy <- load_google_sheet_locally("Wales Secondary Schools") %>%
    add_support_categories_2019() %>%
    tidy_raw_data() %>%
    add_school_locations() %>%
    mutate(school_type = 'secondary')
  
  # QC
  # Find schools with no location (should be none)
  print("Secondary schools with no location")
  secondaries_tidy %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
  
  secondaries_tidy
}

load_special_schools <- function() {
  schools <- load_google_sheet_locally("Wales Special Schools") %>%
    add_support_categories_2019() %>%
    tidy_raw_data() %>%
    add_school_locations() %>%
    mutate(school_type = 'special')
  
  # QC
  # Find schools with no location
  print("Special schools with no location")
  schools %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
  
  schools
}

load_through_schools <- function() {
  schools <- load_google_sheet_locally("Wales Through Schools") %>%
    add_support_categories_2019() %>%
    add_num_pupils() %>%
    add_per_pupil_funding() %>%
    tidy_raw_data() %>%
    add_school_locations() %>%
    mutate(school_type = 'through')
  
  # QC
  # Find schools with no location
  print("Through schools with no location")
  schools %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
  
  schools
}

load_all_schools <- function() {
  load_primaries() %>%
    union_all(load_secondaries()) %>%
    union_all(load_special_schools()) %>%
    union_all(load_through_schools()) %>%
    mutate_at(c('support_category'), as.factor)
}

load_school_locations <- function() {
  # This was batch geocoded using https://www.doogal.co.uk/BatchGeocoding.php
  read_csv("data/geo/school-number-postcodes-geocoded.csv",
           skip = 1,
           col_names = c('lea_code', 'latitude', 'longitude', 'easting', 'northing')) %>%
    select(c('lea_code', 'latitude', 'longitude'))
}

load_support_categories_2019 <- function() {
  # From 2019, dowloaded from https://gov.wales/national-school-categorisation-system-support-categories?_ga=2.207195155.1087568766.1580471338-638608250.1543144354
  # and converted to csv files that are loaded here
  primary_support_category <- read_csv("data/support-category/national-school-categorisation-system-support-categories-2019-primary.csv",
                                       skip = 7,
                                       col_names = c('lea_code', 'school', 'local_authority', 'consortium', 'support_category', 'X6', 'X7', 'X8')) %>%
    select(c('lea_code', 'support_category'))
  secondary_support_category <- read_csv("data/support-category/national-school-categorisation-system-support-categories-2019-secondary.csv",
                                         skip = 8,
                                         col_names = c('lea_code', 'school', 'local_authority', 'consortium', 'support_category')) %>%
    select(c('lea_code', 'support_category'))
  special_support_category <- read_csv("data/support-category/national-school-categorisation-system-support-categories-2019-special.csv",
                                       skip = 7,
                                       col_names = c('lea_code', 'school', 'local_authority', 'consortium', 'support_category')) %>%
    select(c('lea_code', 'support_category'))
  primary_support_category %>%
    union_all(secondary_support_category) %>%
    union_all(special_support_category) %>%
    distinct(lea_code, support_category) %>% # through schools are included in both primary and secondary
    mutate(`Support category 2019` = str_extract(support_category, "^[^/]+")) %>%
    select(-c("support_category"))
}

add_support_categories_2019 <- function(schools_tidy) {
  support_categories_2019 <- load_support_categories_2019()
  schools_tidy %>%
    left_join(support_categories_2019, by = c("LEA Code" = "lea_code"))
}

# Load school-level CSV file downloaded from Stats Wales site
load_stats_wales_school_csv <- function(csv) {
  read_csv(csv) %>%
    select(-c(X1, X2, X3)) %>%
    na_if('.') %>% # dots are NA
    drop_na(X4) %>%
    separate(X4, c("stats_wales_code", "school"), " - ?", extra = "merge") # split code from school name
}

# currently only used for 2020-21 (and onwards in future)
load_num_pupils <- function() {
  load_stats_wales_school_csv("data/delegatedschoolbudgetsperpupil-by-school-num-pupils.csv") %>%
    na_if('Unallocated resources') %>% # dots are NA
    drop_na(stats_wales_code) %>%
    mutate(lea_code = to_lea_code(stats_wales_code)) %>%
    drop_na(`2020-21`) %>%
    mutate_at(c('2020-21', 'lea_code'), as_numeric_ignore_commas) %>%
    mutate_at(c('2020-21'), round) %>%
    mutate(`Pupil numbers 2020-21` = `2020-21`) %>%
    select(c(lea_code, `Pupil numbers 2020-21`))
}

add_num_pupils <- function(schools_tidy) {
  num_pupils <- load_num_pupils()
  schools_tidy %>%
    left_join(num_pupils, by = c("LEA Code" = "lea_code"))
}

# currently only used for 2020-21 (and onwards in future)
load_per_pupil_funding <- function() {
  load_stats_wales_school_csv("data/delegatedschoolbudgetsperpupil-by-school.csv") %>%
    na_if('Unallocated resources') %>% # dots are NA
    drop_na(stats_wales_code) %>%
    mutate(lea_code = to_lea_code(stats_wales_code)) %>%
    drop_na(`2020-21`) %>%
    mutate_at(c('2020-21', 'lea_code'), as_numeric_ignore_commas) %>%
    mutate_at(c('2020-21'), round) %>%
    mutate(`Per pupil funding 2020-21` = `2020-21`) %>%
    select(c(lea_code, `Per pupil funding 2020-21`))
}

add_per_pupil_funding <- function(schools_tidy) {
  per_pupil_funding <- load_per_pupil_funding()
  schools_tidy %>%
    left_join(per_pupil_funding, by = c("LEA Code" = "lea_code"))
}

tidy_raw_data <- function(schools_raw) {
  schools_raw %>%
    rename(local_authority = `Local authority`,
           lea_code = `LEA Code`,
           school = `Name of school`,
           language = `Welsh Medium`,
           capacity = `Capacity`,
           rural_school = `Rural Schools`) %>%
    rename_all(gsub, pattern = '^Pupil numbers (20.+)$', replacement = 'num_pupils#\\1') %>%
    rename_all(gsub, pattern = '^Total delegated budget (20.+)$', replacement = 'total_school_delegated_budget#\\1') %>%
    rename_all(gsub, pattern = '^Per pupil funding (20.+)$', replacement = 'per_pupil_funding#\\1') %>%
    rename_all(gsub, pattern = '^Budget outturn (20.+)$', replacement = 'budget_outturn#\\1') %>%
    rename_all(gsub, pattern = '^FSM rate 2018$', replacement = 'fsm_rate#2018-19') %>% # assume 2018 is 2018-19
    rename_all(gsub, pattern = '^FSM rate 2019$', replacement = 'fsm_rate#2019-20') %>% # assume 2019 is 2019-20
    rename_all(gsub, pattern = '^FSM rate 2020$', replacement = 'fsm_rate#2020-21') %>% # assume 2020 is 2020-21
    rename_all(gsub, pattern = '^Support category (20.+)$', replacement = 'support_category#\\1') %>%
    select(-starts_with('X')) %>% # drop any extra X columns
    select(-starts_with('...')) %>% # drop any extra ... columns
    filter(!is.na(school)) %>% # drop rows with no school name
    filter(!is.na(lea_code)) %>% # and no LEA code
    na_if('.') %>% # dots are NA
    gather(element_year, value, -c(local_authority, lea_code, school, capacity, rural_school, language)) %>%
    separate(element_year, c("element", "year"), sep = "#") %>%
    spread(element, value) %>%
    mutate_at(c('budget_outturn', 'fsm_rate', 'num_pupils', 'per_pupil_funding', 'total_school_delegated_budget'), as_numeric_ignore_commas) %>%
    mutate_at(c('support_category'), as.factor) %>%
    mutate_at(c('rural_school'), as.factor) %>%
    mutate_at(c('language'), as.factor) %>%
    mutate(language = fct_recode(language, "Welsh" = "Yes", "English" = "No", "Bilingual" = "Bilingual (A)", "Bilingual" = "Bilingual (B)", "Bilingual" = "Bilingual (C)")) %>%
    mutate(size=cut(num_pupils, breaks=c(-Inf, 50, 100, 200, 400, Inf), labels=c("<50","50-100", "100-200", "200-400", ">400")))  %>%
    mutate(num_pupils_on_fsm=fsm_rate * num_pupils / 100.0) %>%
    mutate(support_category_days = case_when(support_category == 'Green' ~ 4,
                                             support_category == 'Yellow' ~ 10,
                                             support_category == 'Amber' ~ 15,
                                             support_category == 'Red' ~ 25,
                                             TRUE ~ NA_real_)) %>%
    filter(!is.na(year) & year != '2021-22') # drop blank years and years with no data
}

add_school_locations <- function(schools_tidy) {
  school_locations <- load_school_locations()
  schools_tidy %>%
    left_join(school_locations)
}

all_schools <- load_all_schools()

# Population data

load_population_data <- function(csv = "data/populationestimates-by-localauthority-year.csv") {
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

load_population_with_age <- function() {
  population_0_15 <- load_population_data("data/populationestimates-by-localauthority-year-0-15.csv") %>%
    filter_to_wales_local_authorities() %>%
    gather(year, population, -c(local_authority)) %>%
    mutate(age = '0-15')
  population_16_64 <- load_population_data("data/populationestimates-by-localauthority-year-16-64.csv") %>%
    filter_to_wales_local_authorities() %>%
    gather(year, population, -c(local_authority)) %>%
    mutate(age = '16-64')
  population_65_plus <- load_population_data("data/populationestimates-by-localauthority-year-65-and-over.csv") %>%
    filter_to_wales_local_authorities() %>%
    gather(year, population, -c(local_authority)) %>%
    mutate(age = '65+')
  
  population_0_15 %>%
    union(population_16_64) %>%
    union(population_65_plus)
}

population <- load_population_data()
population_with_age <- load_population_with_age()

# Local authority delegated school budget data

la_delegation_rates <- load_google_sheet_locally('Delegation rates %') %>%
  rename(local_authority = Authority) %>%
  gather(year, delegation_rate_percent, -c(local_authority))

load_delegatedschoolbudgetsperpupil_data <- function(csv = "data/delegatedschoolbudgetsperpupil-by-sector.csv") {
  read_csv(csv) %>%
    rename(country = X1) %>%
    separate(X3, c("LA", "school_type"), " - ") %>% # split local authority from school type ('sector')
    mutate(local_authority = ifelse(!is.na(LA), LA, ifelse(!is.na(X2), X2, 'All'))) %>% # X2 is local authority
    select(-c(country, X2, LA)) %>%
    na_if('.') %>% # dots are NA
    gather(year, delegated_school_budget_per_pupil, -c(local_authority, school_type)) %>%
    mutate_at(c("school_type"), tolower) %>%
    mutate_at(c('delegated_school_budget_per_pupil'), as_numeric_ignore_commas)
}

delegatedschoolbudgetsperpupil <- load_delegatedschoolbudgetsperpupil_data()


# Load pupil numbers - note it's from the delegatedschoolbudgetsperpupil page

load_num_pupils_per_la <- function(csv = "data/delegatedschoolbudgetsperpupil-by-sector-num-pupils.csv") {
  read_csv(csv) %>%
    rename(country = X1) %>%
    separate(X3, c("LA", "school_type"), " - ") %>% # split local authority from school type ('sector')
    mutate(local_authority = ifelse(!is.na(LA), LA, ifelse(!is.na(X2), X2, 'All'))) %>% # X2 is local authority
    select(-c(country, X2, LA)) %>%
    na_if('.') %>% # dots are NA
    gather(year, num_pupils, -c(local_authority, school_type)) %>%
    mutate_at(c("school_type"), tolower) %>%
    mutate_at(c('num_pupils'), as_numeric_ignore_commas) %>%
    mutate(num_pupils = round(num_pupils))
}

num_pupils_per_la <- load_num_pupils_per_la()

# Local authority outturn data

load_outturn_data <- function(csv = "data/levelofreservescarriedforward-by-sector.csv") {
  
  outturn_by_year <- read_csv("data/levelofreservescarriedforward-by-sector.csv") %>%
    rename(country = X1) %>%
    separate(X3, c("LA", "school_type"), " - ") %>% # split local authority from school type ('sector')
    mutate(local_authority = ifelse(!is.na(LA), LA, ifelse(!is.na(X2), X2, 'All'))) %>% # X2 is local authority
    select(-c(country, X2, LA)) %>%
    na_if('.') %>% # dots are NA
    gather(year, budget_outturn, -c(local_authority, school_type)) %>%
    mutate_at(c("school_type"), tolower) %>%
    mutate_at(c('budget_outturn'), as_numeric_ignore_commas) %>%
    mutate_at(c('budget_outturn'), ~.*1000) # multiply to get values in £
  
  outturn_totals_by_year <- outturn_by_year %>% filter(!is.na(school_type)) %>%
    group_by(year, school_type) %>%
    summarize(budget_outturn = sum(budget_outturn)) %>%
    filter(!is.na(budget_outturn)) %>%
    mutate(local_authority = 'All')
  
  outturn_by_year %>%
    union(outturn_totals_by_year) %>%
    left_join(num_pupils_per_la) %>%
    mutate(per_pupil_outturn = budget_outturn / num_pupils)
}

outturn_data <- load_outturn_data()
