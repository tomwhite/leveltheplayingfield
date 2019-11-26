# Download data from Google Sheets and save to a local file to make analysis reproducible.
# Most users should not need to run this.
# Note that this requires read permission.

library(googlesheets)

d <- file.path("data", "sheets")
dir.create(d, showWarnings = FALSE)

load_google_sheet <- function(title) {
  gs_auth() # authorize with google
  gs_title(title) %>% gs_read(ws = "Sheet1")
}

save_google_sheet_locally <- function(title) {
  df <- load_google_sheet(title)
  path <- file.path(d, paste0(title, ".Rda"))
  saveRDS(df, file=path)
}

save_google_sheet_locally('Delegation rates %')

