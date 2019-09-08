library(tidyverse)
library(googlesheets)
library(knitr)
library(scales)
library(DT)

REPORTS_DIR = "~/projects-workspace/leveltheplayingfield/docs/"
LOCAL_AUTHORITIES = c("Anglesey",
                      "Blaenau Gwent",
                      "Bridgend",
                      "Caerphilly",
                      "Cardiff",
                      "Carmarthenshire",
                      "Ceredigion",
                      "Conwy",
                      "Denbighshire",
                      "Flintshire",
                      "Glamorgan",
                      "Gwynedd",
                      "Merthyr Tydfil",
                      "Monmouthshire",
                      "Neath Port Talbot",
                      "Newport",
                      "Pembrokeshire",
                      "Powys",
                      "Rhondda Cynon Taf",
                      "Swansea",
                      "Torfaen",
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

load_google_sheet <- function(title) {
  gs_auth() # authorize with google
  gs_title(title) %>% gs_read(ws = "Sheet1", skip = 1)
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
    filter(year == '2018-19') %>%
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
           school = `Name of school`) %>%
    rename_all(gsub, pattern = '^2018$', replacement = 'fsm_rate#2018-19') %>% # only have FSM for 2018-19
    rename_all(gsub, pattern = '^(20.+)_1$', replacement = 'total_school_delegated_budget#\\1') %>%
    rename_all(gsub, pattern = '^(20.+)_2$', replacement = 'per_pupil_funding#\\1') %>%
    rename_all(gsub, pattern = '^(20.+)_3$', replacement = 'budget_outturn#\\1') %>%
    rename_all(gsub, pattern = '^(20.+)$', replacement = 'num_pupils#\\1') %>%
    mutate_at(vars(contains("#")), as.numeric) %>%
    rename('support_category#2018-19' = `X30`) %>% # only have support category for 2018-19
    select(-c(`Yes/No`)) %>% # drop Welsh medium column for the moment 
    filter(!is.na(school)) %>% # drop rows with no school name
    filter(!is.na(lea_code)) %>% # and no LEA code
    na_if('.') %>% # dots are NA
    gather(element_year, value, -c(local_authority, lea_code, school)) %>%
    separate(element_year, c("element", "year"), sep = "#") %>%
    spread(element, value) %>%
    mutate_at(c('budget_outturn', 'fsm_rate', 'num_pupils', 'per_pupil_funding', 'total_school_delegated_budget'), as.numeric) %>%
    mutate_at(c('support_category'), as.factor) %>%
    mutate(size=cut(num_pupils, breaks=c(-Inf, 50, 100, 200, 400, Inf), labels=c("<50","50-100", "100-200", "200-400", ">400")))  %>%
    mutate(num_pupils_on_fsm=fsm_rate * num_pupils / 100.0) %>%
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
  if (is.null(la)) {
    d <- paste0(REPORTS_DIR, "summary")
    dir.create(d, showWarnings = FALSE)
    paste0(d, "/", school_type, "_", report_name, extension)
  } else {
    la <- standardise_la_name(la)
    d <- file.path(REPORTS_DIR, standardise_la_name(la))
    dir.create(d, showWarnings = FALSE)
    paste0(d, "/", la, "_", school_type, "_", report_name, extension)
  }
}

plot_summary_size_distribution <- function(schools_tidy, school_type, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(!is.na(local_authority)) %>%
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
    ggplot(aes(x=year, y=per_pupil_funding, group=school, color=size)) +
    geom_line() +
    geom_point() +
    ylab("Per-pupil funding (£)") + 
    theme(axis.title.x=element_blank()) +
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    dir.create(file.path(REPORTS_DIR, standardise_la_name(la)), showWarnings = FALSE)
    ggsave(paste0(REPORTS_DIR, standardise_la_name(la), "/", standardise_la_name(la), "_pupil_funding_vs_year.png"))
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
    dir.create(file.path(REPORTS_DIR, standardise_la_name(la)), showWarnings = FALSE)
    ggsave(paste0(REPORTS_DIR, standardise_la_name(la), "/", standardise_la_name(la), "_school_funding_vs_size.png"))
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
    dir.create(file.path(REPORTS_DIR, standardise_la_name(la)), showWarnings = FALSE)
    ggsave(paste0(REPORTS_DIR, standardise_la_name(la), "/", standardise_la_name(la), "_pupil_funding_vs_outturn.png"))
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
    dir.create(file.path(REPORTS_DIR, standardise_la_name(la)), showWarnings = FALSE)
    ggsave(paste0(REPORTS_DIR, standardise_la_name(la), "/", standardise_la_name(la), "_pupil_funding_vs_fsm.png"))
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
    saveWidget(dt, report_file_name(NULL, school_type, "num_pupils_summary", ".html"))
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
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 30,
    order = list(list(2, 'asc'))
  ))
  if (save_to_file) {
    saveWidget(dt, report_file_name(NULL, school_type, "support_category_summary", ".html"))
  }
  dt
}