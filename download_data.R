# Download data from Google Sheets and save to a local file to make analysis reproducible.
# Most users should not need to run this.
# Note that this requires read permission.

library(googlesheets)

load_google_sheet <- function(title) {
  gs_auth() # authorize with google
  gs_title(title) %>% gs_read(ws = "Sheet1")
}

save_google_sheet_locally <- function(title) {
  df <- load_google_sheet(title)
  d <- file.path("data", "sheets")
  dir.create(d, showWarnings = FALSE)
  path <- file.path(d, paste0(title, ".Rda"))
  saveRDS(df, file=path)
}

# Save all sheets locally

for (local_authority in LOCAL_AUTHORITIES) {
  save_google_sheet_locally(paste(local_authority, "Primary Schools"))
}

save_google_sheet_locally("Wales Secondary Schools")
save_google_sheet_locally("Wales Through Schools")
save_google_sheet_locally("Wales Special Schools")

save_google_sheet_locally('Delegation rates %')

