#
# Which schools have been on red or amber for the last 5 years?
#

source('load_data.R')
source('maps.R')
source('schools.R')
source('utils.R')

PREFIX = "2020-01-09-stuck_schools"

x <- all_schools %>%
  filter(support_category == "Red" | support_category == "Amber") %>%
  filter(year >= 2014) %>%
  count(school) %>%
  filter(n == 5)

stuck_schools = all_schools %>%
  filter(year == LATEST_SUPPORT_CATEGORY_YEAR) %>%
  inner_join(x, by = c("school"))

map_stuck_schools <- function(stuck_schools, la = NULL, save_to_file=FALSE) {
  yr = LATEST_SUPPORT_CATEGORY_YEAR
  html_legend <- "Support Category</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png' width='12' height='20'>Green<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Yellow<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>Amber<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Red"
  schools_tidy_filtered = stuck_schools
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', local_authority), icon=~support_category_icons[support_category], group = st)
  }
  map <- map %>%
    addLayersControl(overlayGroups = school_types, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, blog_post_file_name(PREFIX, la, NULL, "support_category", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_stuck_schools(stuck_schools, save_to_file=TRUE)
