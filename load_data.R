# Import and tidy all datasets

library(tidyverse)

source('utils.R')

load_google_sheet_locally <- function(title) {
  d <- file.path("data", "sheets")
  path <- file.path(d, paste0(title, ".Rda"))
  readRDS(path)
}

# Schools data

# Load sheet and drop unused columns, fix types, etc.
load_google_sheet_locally_and_normalize <- function(sheet, school_type) {
  load_google_sheet_locally(sheet) %>%
    mutate(`School type` = school_type) %>%
    select(-starts_with('X')) %>% # drop any extra X columns
    select(-starts_with('...')) %>% # drop any extra ... columns
    filter(!is.na(`Name of school`)) %>% # drop rows with no school name
    filter(!is.na(`LEA Code`)) %>% # and no LEA code
    na_if('.') %>% # dots are NA
    mutate_at(vars(starts_with("Pupil numbers")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("Total delegated budget")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("Per pupil funding")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("Budget outturn")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("FSM rate")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("Capacity")), as_numeric_ignore_commas)
}

# Merge all the Google sheets into a single dataframe
load_merged_google_sheets <- function() {
  school_spreadsheets = list()
  i <- 1
  for (local_authority in LOCAL_AUTHORITIES) {
    school_spreadsheets[[i]] <- load_google_sheet_locally_and_normalize(paste(local_authority, "Primary Schools"), "primary") %>%
      # remove unused columns
      select(-starts_with("Local authority FSM rate")) %>%
      select(-starts_with("Wales FSM rate"))
    #print(local_authority)
    #print(ncol(school_spreadsheets[[i]]))
    i <- i + 1
  }
  
  # Use rbind to detect when columns don't match
  primary <- do.call("rbind", school_spreadsheets)
  
  secondary <- load_google_sheet_locally_and_normalize("Wales Secondary Schools", "secondary") %>%
    # remove unused columns
    select(-c("Pupil numbers 2015-16", "Total delegated budget 2015-16", "Per pupil funding 2015-16")) %>%
    select(-starts_with("Local authority FSM rate")) %>%
    select(-starts_with("Wales FSM rate")) %>%
    select(-starts_with("Occupancy"))
  special <- load_google_sheet_locally_and_normalize("Wales Special Schools", "special")
  through <- load_google_sheet_locally_and_normalize("Wales Through Schools", "through")

  rbind(primary, secondary, special, through) %>%
    # remove columns that we get from Stats Wales directly now
    select(-starts_with('Pupil numbers')) %>%
    select(-starts_with('Total delegated budget')) %>%
    select(-starts_with('Per pupil funding')) %>%
    select(-starts_with('Budget outturn'))
}

# Merge all schools data into a single dataframe - this is a good view to export as a spreadsheet for manual checking
load_merged_data <- function() {
  load_merged_google_sheets() %>%
    add_school_locations2() %>%
    add_support_categories_2019() %>%
    add_num_pupils() %>%
    add_per_pupil_funding() %>%
    add_total_delegated_budget() %>%
    add_budget_outturn() %>%
    # reorder columns
    relocate(`School type`) %>%
    relocate(starts_with('Support category'), .after=`Name of school`) %>%
    relocate(starts_with('FSM rate'), .after=`Name of school`) %>%
    relocate(starts_with('Budget outturn'), .after=`Name of school`) %>%
    relocate(starts_with('Per pupil funding'), .after=`Name of school`) %>%
    relocate(starts_with('Total delegated budget'), .after=`Name of school`) %>%
    relocate(starts_with('Pupil numbers'), .after=`Name of school`) %>%
    # reorder rows
    arrange(`School type`, `Local authority`, `Name of school`)
}

#all <- load_merged_data()
#all %>% write_csv("schools.csv", na="")

all_schools2 <- load_merged_data() %>%
  tidy_merged_data()

# Tidy merged data so it is in a form to run analyses
tidy_merged_data <- function(schools_merged) {
  schools_merged %>%
    rename(school_type = `School type`,
           local_authority = `Local authority`,
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
    gather(element_year, value, -c(school_type, local_authority, lea_code, school, capacity, rural_school, language, latitude, longitude)) %>%
    separate(element_year, c("element", "year"), sep = "#") %>%
    spread(element, value) %>%
    mutate_at(c('budget_outturn', 'fsm_rate', 'num_pupils', 'per_pupil_funding', 'total_school_delegated_budget'), as_numeric_ignore_commas) %>%
    mutate_at(c('school_type'), as.factor) %>%
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

# load_consolidated_data <- function(sheet, school_type) {
#   load_google_sheet_locally(sheet) %>%
#     mutate(`School type` = school_type) %>%
#     select(-starts_with('X')) %>% # drop any extra X columns
#     select(-starts_with('...')) %>% # drop any extra ... columns
#     filter(!is.na(`Name of school`)) %>% # drop rows with no school name
#     filter(!is.na(`LEA Code`)) %>% # and no LEA code
#     na_if('.') %>% # dots are NA
#     add_support_categories_2019() %>%
#     add_num_pupils() %>%
#     add_per_pupil_funding() %>%
#     add_total_delegated_budget() %>%
#     add_budget_outturn() %>%
#     # reorder columns
#     relocate(`School type`) %>%
#     relocate(starts_with('Support category'), .after=`Name of school`) %>%
#     relocate(starts_with('FSM rate'), .after=`Name of school`) %>%
#     relocate(starts_with('Budget outturn'), .after=`Name of school`) %>%
#     relocate(starts_with('Per pupil funding'), .after=`Name of school`) %>%
#     relocate(starts_with('Total delegated budget'), .after=`Name of school`) %>%
#     relocate(starts_with('Pupil numbers'), .after=`Name of school`) %>%
#     # reorder rows
#     arrange(`School type`, `Local authority`, `Name of school`)
# }
#
# load_primaries <- function() {
#   school_spreadsheets = list()
#   i <- 1
#   for (local_authority in LOCAL_AUTHORITIES) {
#     school_spreadsheets[[i]] <- load_google_sheet_locally(paste(local_authority, "Primary Schools")) %>%
#       add_support_categories_2019() %>%
#       tidy_raw_data()
#     i <- i + 1
#   }
#   schools_tidy <- add_school_locations(bind_rows(school_spreadsheets)) %>%
#     mutate(school_type = 'primary')
#   
#   # QC
#   # Should give 5 rows - new schools that don't have a support category from My Local Schools
#   print("Schools that don't have a support category")
#   schools_tidy %>%
#     filter(year == LATEST_SUPPORT_CATEGORY_YEAR) %>%
#     filter(is.na(support_category)) %>%
#     print()
#   
#   # Find schools with no location
#   print("Primary schools with no location")
#   schools_tidy %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
#   
#   schools_tidy
# }
# 
# load_secondaries <- function() {
#   secondaries_tidy <- load_consolidated_data("Wales Secondary Schools", "secondary") %>%
#     # remove unused columns
#     select(-starts_with("Local authority FSM rate")) %>%
#     select(-starts_with("Wales FSM rate")) %>%
#     select(-starts_with("Occupancy")) %>%
#     tidy_raw_data() %>%
#     add_school_locations()
#   
#   # QC
#   # Find schools with no location (should be none)
#   print("Secondary schools with no location")
#   secondaries_tidy %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
#   
#   secondaries_tidy
# }
# 
# load_special_schools <- function() {
#   schools <- load_consolidated_data("Wales Special Schools", "special") %>%
#     tidy_raw_data() %>%
#     add_school_locations()
#   
#   # QC
#   # Find schools with no location
#   print("Special schools with no location")
#   schools %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
#   
#   schools
# }
# 
# load_through_schools <- function() {
#   schools <- load_consolidated_data("Wales Through Schools", "through") %>%
#     tidy_raw_data() %>%
#     add_school_locations()
#   
#   # QC
#   # Find schools with no location
#   print("Through schools with no location")
#   schools %>% filter(year == LATEST_YEAR) %>% filter(is.na(longitude)) %>% print()
#   
#   schools
# }
# 
# load_all_schools <- function() {
#   load_primaries() %>%
#     union_all(load_secondaries()) %>%
#     union_all(load_special_schools()) %>%
#     union_all(load_through_schools()) %>%
#     mutate_at(c('support_category'), as.factor)
# }

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

# Some data fixes
fix_stats_wales_schools <- function(stats_wales_schools) {
  stats_wales_schools %>%
    # Stats Wales has the old code for Banw/Llanerfyl New School
    mutate(stats_wales_code = replace(stats_wales_code, stats_wales_code == "5245201", "5243323")) %>%
    # Merge rows for recoded Parc Y Bont school
    mutate(stats_wales_code = replace(stats_wales_code, stats_wales_code == "5123034", "5122228")) %>%
    mutate(school = replace(school, school == "Ysgol Parc Y Bont", "Ysgol Gynradd Parc Y Bont")) %>%
    group_by(stats_wales_code) %>%
    fill(everything(), .direction = "updown") %>% # within each group
    ungroup() %>%
    distinct(stats_wales_code, .keep_all = TRUE)
}

load_num_pupils <- function() {
  load_stats_wales_school_csv("data/delegatedschoolbudgetsperpupil-by-school-num-pupils.csv") %>%
    fix_stats_wales_schools() %>%
    filter(!grepl('Unallocated resources', school)) %>%
    drop_na(stats_wales_code) %>%
    mutate(lea_code = to_lea_code(stats_wales_code)) %>%
    mutate_at("lea_code", as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), round) %>%
    rename_at(vars(starts_with("20")), function(x){paste0("Pupil numbers ", x)}) %>%
    select(-c("stats_wales_code", "school"))
}

add_num_pupils <- function(schools_tidy) {
  num_pupils <- load_num_pupils()
  schools_tidy %>%
    left_join(num_pupils, by = c("LEA Code" = "lea_code"))
}
  
load_per_pupil_funding <- function() {
  load_stats_wales_school_csv("data/delegatedschoolbudgetsperpupil-by-school.csv") %>%
    fix_stats_wales_schools() %>%
    na_if('Unallocated resources') %>% # dots are NA
    drop_na(stats_wales_code) %>%
    mutate(lea_code = to_lea_code(stats_wales_code)) %>%
    mutate_at("lea_code", as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), round) %>%
    rename_at(vars(starts_with("20")), function(x){paste0("Per pupil funding ", x)}) %>%
    select(-c("stats_wales_code", "school"))
}

add_per_pupil_funding <- function(schools_tidy) {
  per_pupil_funding <- load_per_pupil_funding()
  schools_tidy %>%
    left_join(per_pupil_funding, by = c("LEA Code" = "lea_code"))
}

load_total_delegated_budget <- function() {
  load_stats_wales_school_csv("data/delegatedschoolbudgetsperpupil-by-school-total-budget.csv") %>%
    fix_stats_wales_schools() %>%
    filter(!grepl('Unallocated resources', school)) %>%
    drop_na(stats_wales_code) %>%
    mutate(lea_code = to_lea_code(stats_wales_code)) %>%
    mutate_at("lea_code", as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), ~.*1000) %>% # multiply to get values in £
    mutate_at(vars(starts_with("20")), round) %>%
    rename_at(vars(starts_with("20")), function(x){paste0("Total delegated budget ", x)}) %>%
    select(-c("stats_wales_code", "school"))
}

add_total_delegated_budget <- function(schools_tidy) {
  total_delegated_budget <- load_total_delegated_budget()
  schools_tidy %>%
    left_join(total_delegated_budget, by = c("LEA Code" = "lea_code"))
}

load_budget_outturn <- function() {
  load_stats_wales_school_csv("data/levelofreservescarriedforward-by-school.csv") %>%
    fix_stats_wales_schools() %>%
    filter(!grepl('Unallocated resources', school)) %>%
    drop_na(stats_wales_code) %>%
    mutate(lea_code = to_lea_code(stats_wales_code)) %>%
    mutate_at("lea_code", as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), as_numeric_ignore_commas) %>%
    mutate_at(vars(starts_with("20")), ~.*1000) %>% # multiply to get values in £
    mutate_at(vars(starts_with("20")), round) %>%
    rename_at(vars(starts_with("20")), function(x){paste0("Budget outturn ", x)}) %>%
    select(-c("stats_wales_code", "school"))
}

add_budget_outturn <- function(schools_tidy) {
  budget_outturn <- load_budget_outturn()
  schools_tidy %>%
    left_join(budget_outturn, by = c("LEA Code" = "lea_code"))
}

tidy_raw_data <- function(schools_raw) {
  schools_raw %>%
    rename(school_type = `School type`,
           local_authority = `Local authority`,
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
    gather(element_year, value, -c(school_type, local_authority, lea_code, school, capacity, rural_school, language)) %>%
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

add_school_locations2 <- function(schools) {
  school_locations <- load_school_locations()
  schools %>%
    left_join(school_locations, by = c("LEA Code" = "lea_code"))
}

#all_schools <- load_all_schools()
all_schools <- all_schools2

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
  select(-starts_with('X')) %>% # drop any extra X columns
  select(-starts_with('...')) %>% # drop any extra ... columns
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
