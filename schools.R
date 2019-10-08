library(tidyverse)
library(broom)
library(googlesheets)
library(knitr)
library(scales)
library(DT)
library(htmlTable)

REPORTS_DIR = "~/projects-workspace/leveltheplayingfield/docs"
LOCAL_AUTHORITIES = c("Blaenau Gwent",
                      "Bridgend",
                      "Caerphilly",
                      "Cardiff",
                      "Carmarthenshire",
                      "Ceredigion",
                      "Conwy",
                      "Denbighshire",
                      "Flintshire",
                      "Gwynedd",
                      "Isle of Anglesey",
                      "Merthyr Tydfil",
                      "Monmouthshire",
                      "Neath Port Talbot",
                      "Newport",
                      "Pembrokeshire",
                      "Powys",
                      "Rhondda Cynon Taf",
                      "Swansea",
                      "Torfaen",
                      "Vale of Glamorgan",
                      "Wrexham")
SCHOOL_SIZE_COLOURS = c("<50" = "#F8766D", "50-100" = "#A3A500", "100-200" = "#00BF7D", "200-400" = "#00B0F6", ">400" = "#E76BF3")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  # https://stackoverflow.com/a/29876220
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

as_numeric_ignore_commas <- function(x){
  as.numeric(gsub("\\,", "", x))
}

load_google_sheet <- function(title) {
  gs_auth() # authorize with google
  gs_title(title) %>% gs_read(ws = "Sheet1")
}

load_local_authority_sheet <- function(local_authority) {
  load_google_sheet(paste(local_authority, "Primary Schools"))
}

load_secondary_schools_sheet <- function() {
  load_google_sheet("Wales Secondary Schools")
}

load_primaries <- function() {
  school_spreadsheets = list()
  i <- 1
  for (local_authority in LOCAL_AUTHORITIES) {
    school_spreadsheets[[i]] <- tidy_raw_data(load_local_authority_sheet(local_authority))
    i <- i + 1
  }
  schools_tidy <- add_school_locations(bind_rows(school_spreadsheets)) %>%
    mutate(school_type = 'primary')
  
  # QC
  # Should give 5 rows - new schools that don't have a support category from My Local Schools
  schools_tidy %>%
    filter(year == '2018') %>%
    filter(is.na(support_category)) %>%
    print()
  
  # Find schools with no location
  schools_tidy %>% filter(year == '2018-19') %>% filter(is.na(longitude)) %>% print()
  
  schools_tidy
}

load_secondaries <- function() {
  secondaries_tidy <- tidy_raw_data(load_secondary_schools_sheet()) %>%
    add_school_locations() %>%
    mutate(school_type = 'secondary')
  
  # QC
  # Find schools with no location (should be none)
  secondaries_tidy %>% filter(year == '2018-19') %>% filter(is.na(longitude)) %>% print()
  
  secondaries_tidy
}

load_special_schools <- function() {
  schools <- tidy_raw_data(load_google_sheet("Wales Special Schools")) %>%
    add_school_locations() %>%
    mutate(school_type = 'special')
  
  # QC
  # Find schools with no location
  schools %>% filter(year == '2018-19') %>% filter(is.na(longitude)) %>% print()
  
  schools
}

load_through_schools <- function() {
  schools <- tidy_raw_data(load_google_sheet("Wales Through Schools")) %>%
    add_school_locations() %>%
    mutate(school_type = 'through')
  
  # QC
  # Find schools with no location
  schools %>% filter(year == '2018-19') %>% filter(is.na(longitude)) %>% print()
  
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

tidy_raw_data <- function(schools_raw) {
  schools_raw %>%
    rename(local_authority = `Local authority`,
           lea_code = `LEA Code`,
           school = `Name of school`,
           welsh_medium = `Welsh Medium`,
           capacity = `Capacity`,
           rural_school = `Rural Schools`) %>%
    rename_all(gsub, pattern = '^Pupil numbers (20.+)$', replacement = 'num_pupils#\\1') %>%
    rename_all(gsub, pattern = '^Total delegated budget (20.+)$', replacement = 'total_school_delegated_budget#\\1') %>%
    rename_all(gsub, pattern = '^Per pupil funding (20.+)$', replacement = 'per_pupil_funding#\\1') %>%
    rename_all(gsub, pattern = '^Budget outturn (20.+)$', replacement = 'budget_outturn#\\1') %>%
    rename_all(gsub, pattern = '^FSM rate 2018$', replacement = 'fsm_rate#2018-19') %>% # assume 2018 is 2018-19
    rename_all(gsub, pattern = '^Support category (20.+)$', replacement = 'support_category#\\1') %>%
    select(-starts_with('X')) %>% # drop any extra X columns
    select(-c('welsh_medium')) %>% # drop for the moment
    filter(!is.na(school)) %>% # drop rows with no school name
    filter(!is.na(lea_code)) %>% # and no LEA code
    na_if('.') %>% # dots are NA
    gather(element_year, value, -c(local_authority, lea_code, school, capacity, rural_school)) %>%
    separate(element_year, c("element", "year"), sep = "#") %>%
    spread(element, value) %>%
    mutate_at(c('budget_outturn', 'fsm_rate', 'num_pupils', 'per_pupil_funding', 'total_school_delegated_budget'), as_numeric_ignore_commas) %>%
    mutate_at(c('support_category'), as.factor) %>%
    mutate_at(c('rural_school'), as.factor) %>%
    mutate(size=cut(num_pupils, breaks=c(-Inf, 50, 100, 200, 400, Inf), labels=c("<50","50-100", "100-200", "200-400", ">400")))  %>%
    mutate(num_pupils_on_fsm=fsm_rate * num_pupils / 100.0) %>%
    mutate(support_category_days = case_when(support_category == 'Green' ~ 4,
                                             support_category == 'Yellow' ~ 10,
                                             support_category == 'Amber' ~ 15,
                                             support_category == 'Red' ~ 25,
                                             TRUE ~ NA_real_)) %>%
    filter(!is.na(year) & year != '2020-21' & year != '2021-22') # drop blank years and years with no data
}

add_school_locations <- function(schools_tidy) {
  school_locations <- load_school_locations()
  schools_tidy %>%
    left_join(school_locations)
}

standardise_la_name <- function(la) {
  gsub(" ", "_", tolower(la))
}

report_file_name <- function(la, school_type, report_name, extension) {
  # create a standard file name for a report
  st <- if (is.null(school_type)) 'all_schools' else school_type
  dir.create(file.path(REPORTS_DIR), showWarnings = FALSE)
  if (is.null(la)) {
    d <- file.path(REPORTS_DIR, "wales")
    dir.create(d, showWarnings = FALSE)
    paste0(d, "/", st, "_", report_name, extension)
  } else {
    dir.create(file.path(REPORTS_DIR, "local_authorities"), showWarnings = FALSE)
    la <- standardise_la_name(la)
    d <- file.path(REPORTS_DIR, "local_authorities", standardise_la_name(la))
    dir.create(d, showWarnings = FALSE)
    paste0(d, "/", la, "_", st, "_", report_name, extension)
  }
}

plot_summary_size_distribution <- function(schools_tidy, school_type, save_to_file=FALSE) {
  st <- school_type
  plot = schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == '2018-19') %>%
    ggplot(aes(num_pupils)) +
    geom_histogram(binwidth=25, colour="black", fill="white") +
    facet_wrap(~ local_authority, ncol=4)
  if (save_to_file) {
    ggsave(report_file_name(NULL, school_type, "size_distribution", ".png"))
  }
  plot
}

plot_pupil_funding_vs_year <- function(schools_tidy, la, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter(str_detect(year, '-')) %>% # only school years
    ggplot(aes(x=year, y=per_pupil_funding, group=school, color=size)) +
    geom_line() +
    geom_point() +
    ylab("Per-pupil funding (£)") + 
    theme(axis.title.x=element_blank()) +
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_year", ".png"))
  }
  plot
}

plot_school_funding_vs_size <- function(schools_tidy, la, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter(year == "2019-20") %>%
    ggplot(aes(x=num_pupils, y=total_school_delegated_budget)) +
    geom_point() +
    xlab("Number of pupils") +
    ylab("Total school delegated funding (£)")
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "school_funding_vs_size", ".png"))
  }
  plot
}

plot_pupil_funding_vs_outturn <- function(schools_tidy, la, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter (!is.na(budget_outturn)) %>%
    ggplot(aes(x=budget_outturn, y=per_pupil_funding)) +
    geom_point(aes(color=size)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ year, ncol=1, drop=TRUE) +
    xlab("Budget outturn (£)") +
    ylab("Per-pupil funding (£)") +
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_outturn", ".png"))
  }
  plot
}

plot_pupil_funding_vs_per_pupil_outturn <- function(schools_tidy, la, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter (!is.na(budget_outturn)) %>%
    ggplot(aes(x=budget_outturn/num_pupils, y=per_pupil_funding)) +
    geom_point() +
    geom_smooth(method=lm) +
    xlab("Per-pupil budget outturn (£)") +
    ylab("Per-pupil funding (£)")
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_pupil_outturn", ".png"))
  }
  plot
}

plot_pupil_funding_vs_fsm <- function(schools_tidy, la, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter(year == "2018-19") %>% # FSM rate refers to 2018-19
    ggplot(aes(x=fsm_rate, y=per_pupil_funding)) +
    geom_point(aes(color=size, size=num_pupils_on_fsm)) +
    geom_smooth(method=lm) +
    xlab("Percentage of pupils on free school meals") +
    ylab("Per-pupil funding (£)") +
    labs(color="Size of school", size="Number of pupils on FSM") +
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_fsm", ".png"))
  }
  plot
}

plot_support_catagory_vs_year <- function(schools_tidy, st, la, save_to_file=FALSE) {
  # All Wales is black, LA is blue
  all_wales_support_category <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(!is.na(support_category)) %>%
    group_by(year) %>%
    summarize(mean_support_category_days=mean(support_category_days)) %>%
    mutate(local_authority = 'All')
  
  per_la_support_category <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(!is.na(support_category)) %>%
    group_by(local_authority, year) %>%
    summarize(mean_support_category_days=mean(support_category_days))
  
  plot = per_la_support_category %>%
    ggplot(aes(x=year, y=mean_support_category_days, group=local_authority)) +
    geom_hline(yintercept = 4, color='green') +
    geom_hline(yintercept = 10, color='yellow') +
    geom_hline(yintercept = 15, color='orange') +
    geom_line(alpha = 0.2) +
    geom_line(data = all_wales_support_category, color = 'black') +
    geom_line(data = filter(per_la_support_category, local_authority == la), color='blue') +
    ylab("Average support category days") + 
    scale_y_continuous(breaks = seq(4, 25)) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, st, "support_category_vs_year", ".png"))
  }
  plot
}

tabulate_num_pupils_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  # summary of min, max, mean, median number of pupils per LA per year
  table <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    group_by(local_authority, year) %>%
    summarize(schools=n(), total_pupils=sum(num_pupils), smallest=min(num_pupils), largest=max(num_pupils), mean=round(mean(num_pupils), 0), median=round(median(num_pupils), 0)) %>%
    rename("Local authority" = local_authority, "Year" = year, "Schools" = schools, "Total pupils" = total_pupils, "Smallest" = smallest, "Largest" = largest, "Mean" = mean, "Median" = median)
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidget(dt, report_file_name(NULL, school_type, "num_pupils_summary", ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}

tabulate_support_category_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  # ranking of support category per LA for 2018-19
  table <- schools_tidy %>%
    filter(year == '2018-19') %>%
    mutate(support_category_number = case_when(support_category == 'Green' ~ 1,
                                               support_category == 'Yellow' ~ 2,
                                               support_category == 'Amber' ~ 3,
                                               support_category == 'Red' ~ 4,
                                               TRUE ~ NA_real_)) %>%
    group_by(local_authority) %>%
    summarize(schools=n(), mean_support_category=mean(support_category_number)) %>%
    round_df(2)
  #dt <- datatable(table, rownames= FALSE, options = list(
  #  pageLength = 30,
  #  order = list(list(2, 'asc'))
  #))
  ht <- htmlTable(table %>% arrange(mean_support_category),
                  header = c("Local authority", "Num schools", "Mean support category"))
  if (save_to_file) {
    ht %>% cat(., file = report_file_name(NULL, school_type, "support_category_summary", ".html"))
  }
  ht
}

tabulate_general_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  # summary of main indicators (latest year available)
  
  st <- school_type
  summary_size <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == '2018-19') %>%
    group_by(local_authority) %>%
    summarize(schools=n(), total_pupils=sum(num_pupils), mean=round(mean(num_pupils), 0)) %>%
    mutate(mean_rank = rank(desc(mean))) %>%
    rename("Local authority" = local_authority, "Schools (2018-19)" = schools, "Total pupils (2018-19)" = total_pupils, "Mean pupils (2018-19)" = mean, "Mean pupils rank (2018-19)" = mean_rank)
  
  summary_support_category <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == '2018') %>%
    mutate(support_category_days = case_when(support_category == 'Green' ~ 4,
                                             support_category == 'Yellow' ~ 10,
                                             support_category == 'Amber' ~ 15,
                                             support_category == 'Red' ~ 25,
                                             TRUE ~ NA_real_)) %>%
    group_by(local_authority) %>%
    summarize(mean=round(mean(support_category_days), 2)) %>%
    mutate(mean_rank = min_rank(mean)) %>%
    rename("Local authority" = local_authority, "Mean support category days (2018)" = mean, "Mean support category days rank (2018)" = mean_rank)
  
  summary_per_pupil_outturn <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == '2017-18') %>%
    group_by(local_authority) %>%
    summarize(mean=round(mean(budget_outturn / num_pupils), 0)) %>%
    mutate(mean_rank = rank(desc(mean))) %>%
    rename("Local authority" = local_authority, "Mean per-pupil budget outturn (2017-18)" = mean, "Mean per-pupil budget outturn rank (2017-18)" = mean_rank)

  table <- summary_size %>%
    left_join(summary_support_category) %>%
    left_join(summary_per_pupil_outturn)
  
  # TODO: handle NAs in fit?
  # if (is.null(st) || (st != 'through' && st != 'special')) {
  #   summary_per_pupil_fsm <- schools_tidy %>%
  #     filter(!is.na(local_authority)) %>%
  #     filter(!is.na(num_pupils)) %>%
  #     filter(if (!is.null(st)) school_type == st else TRUE) %>%
  #     filter(year == '2018-19') %>%
  #     nest(-local_authority) %>%
  #     mutate(
  #       fit = map(data, ~ lm(per_pupil_funding ~ fsm_rate, data = .x)),
  #       tidied = map(fit, tidy)
  #     ) %>% 
  #     unnest(tidied) %>%
  #     filter(term == 'fsm_rate') %>%
  #     select(c('local_authority', 'estimate')) %>%
  #     mutate(estimate_rank = rank(desc(estimate))) %>%
  #     mutate(estimate = round(estimate, 1)) %>%
  #     rename("Local authority" = local_authority, "Per-pupil funding increase per FSM % increase (2018-19)" = estimate, "Per-pupil funding increase per FSM % increase rank (2018-19)" = estimate_rank)
  #   
  #   table <- table %>%
  #     left_join(summary_per_pupil_fsm)
  # }
  
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidget(dt, report_file_name(NULL, school_type, "general_summary", ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}