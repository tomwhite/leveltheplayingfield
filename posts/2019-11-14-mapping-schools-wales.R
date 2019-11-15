# Customised version of maps with only secondary schools selected

source('maps.R')
source('schools.R')

PREFIX = "2019-11-14-mapping-schools-wales"

map_outturn_surplus_or_deficit_by_school_type_secondary_only <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_OUTTURN_YEAR
  html_legend <- "Budget Outturn</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Over statutory limit<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png' width='12' height='20'>Surplus<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Deficit"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    rowwise() %>% # needed since following mutate uses a function
    mutate(surplus_or_deficit = to_surplus_or_deficit_category(school_type, budget_outturn))
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(budget_outturn)), icon=~surplus_or_deficit_icons[surplus_or_deficit], group = st)
  }
  map <- map %>%
    addLayersControl(overlayGroups = school_types, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend) %>%
    hideGroup("primary") %>%
    hideGroup("special") %>%
    hideGroup("through")
  if (save_to_file) {
    saveWidget(map, blog_post_file_name(PREFIX, la, NULL, "outturn_surplus_or_deficit", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_support_categories_by_school_type_secondary_only <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_SUPPORT_CATEGORY_YEAR
  html_legend <- "Support Category</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png' width='12' height='20'>Green<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Yellow<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>Amber<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Red"
  schools_tidy_filtered = schools_tidy %>%
    filter(year == yr) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE)
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, icon=~support_category_icons[support_category], group = st)
  }
  map <- map %>%
    addLayersControl(overlayGroups = school_types, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend) %>%
    hideGroup("primary") %>%
    hideGroup("special") %>%
    hideGroup("through")
  if (save_to_file) {
    saveWidget(map, blog_post_file_name(PREFIX, la, NULL, "support_category", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_outturn_surplus_or_deficit_by_school_type_secondary_only(all_schools, 'Powys', save_to_file=TRUE)
map_support_categories_by_school_type_secondary_only(all_schools, 'Pembrokeshire', save_to_file=TRUE)
map_support_categories_by_school_type_secondary_only(all_schools, 'Caerphilly', save_to_file=TRUE)
