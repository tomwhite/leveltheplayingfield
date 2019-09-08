library(htmlwidgets)
library(leaflet)

# Colours must be restricted to those at https://github.com/pointhi/leaflet-color-markers
makeColouredIcon <- function(colour) {
  paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-', colour, '.png')
  makeIcon(paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-', colour, '.png'),
           paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-', colour, '.png'),
           25, 41, 12, 41)
}

# Support category colours
support_category_icons <- iconList(
  Green = makeColouredIcon('green'),
  Yellow = makeColouredIcon('yellow'),
  Amber = makeColouredIcon('orange'),
  Red = makeColouredIcon('red')
)

# Surplace = black, deficit = red
surplus_or_deficit_icons <- iconList(
  Black = makeColouredIcon('black'),
  Red = makeColouredIcon('red')
)

map_support_categories <- function(secondaries_tidy_geo, la = NULL, school_type, save_to_file=FALSE) {
  map <- leaflet(data = secondaries_tidy_geo) %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude, popup = ~school, label=~school, icon=~support_category_icons[support_category])
  if (save_to_file) {
    saveWidget(map, report_file_name(la, school_type, "support_category", ".html"))
  }
  map
}

map_support_categories_by_local_authority <- function(secondaries_tidy_geo, school_type, save_to_file=FALSE) {
  las <- as.character(unique(secondaries_tidy_geo$local_authority))
  map <- leaflet(data = secondaries_tidy_geo) %>% addTiles()
  for (la in las) {
    d = secondaries_tidy_geo[secondaries_tidy_geo$local_authority == la,]
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
  map <- leaflet(data = secondaries_tidy_geo_all_years) %>% addTiles()
  for (year in years) {
    d = secondaries_tidy_geo_all_years[secondaries_tidy_geo_all_years$year == year,]
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
