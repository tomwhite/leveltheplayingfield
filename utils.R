# Constants and utility functions

REPORTS_DIR = "./docs"
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

LATEST_YEAR = '2019-20' # old
LATEST_NUM_PUPILS_YEAR = '2021-22'
LATEST_OUTTURN_YEAR = '2019-20'
LATEST_SUPPORT_CATEGORY_YEAR = '2019'
LATEST_FSM_YEAR = '2020-21'
LATEST_LANGUAGE_YEAR = '2019-20'

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

# From https://github.com/ramnathv/htmlwidgets/issues/299
saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  saveWidget(widget,file=file,...)
}

to_lea_code <- function(stats_wales_code) {
  str_replace_all(stats_wales_code, c("^545" = "677", # Blaenau Gwent
                                      "^536" = "672", # Bridgend
                                      "^544" = "676", # Caerphilly
                                      "^552" = "681", # Cardiff
                                      "^530" = "669", # Carmarthenshire
                                      "^526" = "667", # Ceredigion
                                      "^516" = "662", # Conwy
                                      "^518" = "663", # Denbighshire
                                      "^520" = "664", # Flintshire
                                      "^514" = "661", # Gwynedd
                                      "^512" = "660", # Isle of Anglesey
                                      "^542" = "675", # Merthyr Tydfil
                                      "^548" = "679", # Monmouthshire
                                      "^534" = "671", # Neath Port Talbot
                                      "^550" = "680", # Newport
                                      "^528" = "668", # Pembrokeshire
                                      "^524" = "666", # Powys
                                      "^540" = "674", # Rhondda Cynon Taf
                                      "^532" = "670", # Swansea
                                      "^546" = "678", # Torfaen
                                      "^538" = "673", # Vale of Glamorgan
                                      "^522" = "665"  # Wrexham
                                      )
                  )
}