REPORTS_DIR = "~/projects-workspace/leveltheplayingfield/docs"
POSTS_DIR = paste0(REPORTS_DIR, "/posts")

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

LATEST_YEAR = '2018-19'
LATEST_NUM_PUPILS_YEAR = '2019-20'
LATEST_OUTTURN_YEAR = '2018-19'
LATEST_SUPPORT_CATEGORY_YEAR = '2018'
LATEST_FSM_YEAR = '2018-19'

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

format_gbp <- function(amount) {
  # format an amount in GBP (£)
  ifelse(amount < 0, paste0("-", dollar(abs(amount), prefix="£")), dollar(amount, prefix="£"))
}

standardise_la_name <- function(la) {
  gsub(" ", "_", tolower(la))
}

report_file_name <- function(la, school_type, report_name, year, extension) {
  # create a standard file name for a report
  st <- if (is.null(school_type)) 'all_schools' else school_type
  yr <- if (is.null(year)) '' else paste0('_', year)
  dir.create(file.path(REPORTS_DIR), showWarnings = FALSE)
  if (is.null(la)) {
    d <- file.path(REPORTS_DIR, "wales")
    dir.create(d, showWarnings = FALSE)
    paste0(d, "/", st, "_", report_name, yr, extension)
  } else {
    dir.create(file.path(REPORTS_DIR, "local_authorities"), showWarnings = FALSE)
    la <- standardise_la_name(la)
    d <- file.path(REPORTS_DIR, "local_authorities", standardise_la_name(la))
    dir.create(d, showWarnings = FALSE)
    paste0(d, "/", la, "_", st, "_", report_name, yr, extension)
  }
}

blog_post_file_name <- function(prefix, la, school_type, report_name, year, extension) {
  # create a standard file name for a report
  st <- if (is.null(school_type)) 'all_schools' else school_type
  yr <- if (is.null(year)) '' else paste0('_', year)
  dir.create(file.path(POSTS_DIR), showWarnings = FALSE)
  if (is.null(la)) {
    paste0(POSTS_DIR, "/", prefix, "_", st, "_", report_name, yr, extension)
  } else {
    la <- standardise_la_name(la)
    paste0(POSTS_DIR, "/", prefix, "_", la, "_", st, "_", report_name, yr, extension)
  }
}