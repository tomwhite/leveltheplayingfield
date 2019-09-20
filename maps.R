library(htmlwidgets)
library(leaflet)

# Colours must be restricted to those at https://github.com/pointhi/leaflet-color-markers
make_coloured_icon <- function(colour) {
  paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-', colour, '.png')
  makeIcon(paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-', colour, '.png'),
           paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-', colour, '.png'),
           25, 41, 12, 41)
}

# Support category colours
support_category_icons <- iconList(
  Green = make_coloured_icon('green'),
  Yellow = make_coloured_icon('yellow'),
  Amber = make_coloured_icon('orange'),
  Red = make_coloured_icon('red')
)

# Surplus = black, deficit = red
surplus_or_deficit_icons <- iconList(
  Black = make_coloured_icon('black'),
  Red = make_coloured_icon('red')
)

# Occupancy bands
occupancy_band_icons <- iconList(
  '<50%' = make_coloured_icon('red'),
  '50-75%' = make_coloured_icon('orange'),
  '75-100%' = make_coloured_icon('blue'),
  '>100%' = make_coloured_icon('violet')
)

map_support_categories <- function(secondaries_tidy_geo, la = NULL, school_type, save_to_file=FALSE) {
  map <- secondaries_tidy_geo %>%
    filter(year == '2018') %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude, popup = ~school, label=~school, icon=~support_category_icons[support_category])
  if (save_to_file) {
    saveWidget(map, report_file_name(la, school_type, "support_category", ".html"))
  }
  map
}

map_support_categories_by_local_authority <- function(secondaries_tidy_geo, school_type, save_to_file=FALSE) {
  las <- as.character(unique(secondaries_tidy_geo$local_authority))
  secondaries_tidy_geo_filtered = secondaries_tidy_geo %>%
    filter(year == '2018')
  map <- secondaries_tidy_geo_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = secondaries_tidy_geo_filtered[secondaries_tidy_geo_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, icon=~support_category_icons[support_category], group=la)
  }
  map <- map %>% addLayersControl(
    overlayGroups = las,
    options = layersControlOptions(collapsed = FALSE)
  )
  if (save_to_file) {
    saveWidget(map, report_file_name(NULL, school_type, "support_category_with_la", ".html"))
  }
  map
}

map_outturn_surplus_or_deficit_by_year <- function(secondaries_tidy_geo_all_years, la = NULL, school_type, save_to_file=FALSE) {
  years <- unique(secondaries_tidy_geo_all_years$year)
  years <- years[str_detect(years, '-')] # only school years
  secondaries_tidy_geo_all_years_filtered <- secondaries_tidy_geo_all_years %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    mutate(surplus_or_deficit = if_else(budget_outturn >= 0, "Black", "Red"))
  map <- secondaries_tidy_geo_all_years_filtered %>%
    leaflet() %>%
    addTiles()
  for (year in years) {
    d = secondaries_tidy_geo_all_years_filtered[secondaries_tidy_geo_all_years_filtered$year == year,]
    map <- map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', budget_outturn), icon=~surplus_or_deficit_icons[surplus_or_deficit], group = year)
  }
  map <- map %>% addLayersControl(
    baseGroups = years,
    options = layersControlOptions(collapsed = FALSE)
  )
  if (save_to_file) {
    saveWidget(map, report_file_name(la, school_type, "outturn_surplus_or_deficit", ".html"))
  }
  map
}

map_occupancy_by_school_type <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  school_types <- as.character(unique(schools_tidy$school_type))
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == '2019-20') %>%
    filter(!is.na(num_pupils)) %>% # drop rows with no num_pupils
    filter(!is.na(capacity)) %>% # drop rows with no capacity
    mutate(occupancy = 100.0 * num_pupils / capacity) %>%
    mutate(occupancy_band = cut(occupancy, breaks=c(-Inf, 50, 75, 100, Inf), labels=c("<50%","50-75%", "75-100%", ">100%")))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(occupancy, 1), '%'), icon=~occupancy_band_icons[occupancy_band], group = st)
  }
  map <- map %>% addLayersControl(
    overlayGroups = school_types,
    options = layersControlOptions(collapsed = FALSE)
  )
  if (save_to_file) {
    saveWidget(map, report_file_name(la, NULL, "occupancy", ".html"))
  }
  map
}
