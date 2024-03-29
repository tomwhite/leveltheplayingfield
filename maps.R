library(htmlwidgets)
library(leaflet)

source('utils.R')

# Colours must be restricted to those at https://github.com/pointhi/leaflet-color-markers
make_coloured_icon <- function(colour) {
  paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-', colour, '.png')
  makeIcon(paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-', colour, '.png'),
           paste0('https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-', colour, '.png'),
           25, 41, 12, 41)
}

# Support category colours
support_category_factors <- c("Green", "Yellow", "Amber", "Red")
support_category_icons <- iconList(
  Green = make_coloured_icon('green'),
  Yellow = make_coloured_icon('yellow'),
  Amber = make_coloured_icon('orange'),
  Red = make_coloured_icon('red')
)
support_category_palette <- colorFactor(c("green", "yellow", "orange", "red"), domain = support_category_factors, ordered = TRUE)

# Per-pupil funding colours
per_pupil_funding_icons <- iconList(
  q4 = make_coloured_icon('violet'),
  q3 = make_coloured_icon('grey'),
  q2 = make_coloured_icon('grey'),
  q1 = make_coloured_icon('red')
)

# Surplus = black, deficit = red
surplus_or_deficit_factors <- c("Over statutory limit", "Surplus", "Deficit")
surplus_or_deficit_icons <- iconList(
  `Over statutory limit` = make_coloured_icon('yellow'),
  Surplus = make_coloured_icon('black'),
  Deficit = make_coloured_icon('red')
)
surplus_or_deficit_palette <- colorFactor(c("yellow", "black", "red"), domain = surplus_or_deficit_factors, ordered = TRUE)

to_surplus_or_deficit_category <- function(school_type, budget_outturn) {
  if (school_type == 'primary') {
    return (case_when(budget_outturn < 0 ~ "Deficit", budget_outturn <= 50000 ~ "Surplus", TRUE ~ "Over statutory limit"))
  } else if (school_type == 'secondary') {
    return (case_when(budget_outturn < 0 ~ "Deficit", budget_outturn <= 100000 ~ "Surplus", TRUE ~ "Over statutory limit"))
  }
  if_else(budget_outturn < 0, "Deficit", "Surplus")
}

# Occupancy bands
occupancy_band_factors <- c("<50%", "50-75%", "75-100%", ">100%")
occupancy_band_icons <- iconList(
  '<50%' = make_coloured_icon('red'),
  '50-75%' = make_coloured_icon('orange'),
  '75-100%' = make_coloured_icon('blue'),
  '>100%' = make_coloured_icon('violet')
)
occupancy_band_palette <- colorFactor(c("red", "orange", "blue", "#982FC7"), domain = occupancy_band_factors, ordered = TRUE)

# Language colours, Welsh = red, English = grey, Bilingual = violet, Dual = blue
language_factors <- c("Welsh", "English", "Bilingual", "Dual")
language_icons <- iconList(
  Welsh = make_coloured_icon('red'),
  English = make_coloured_icon('grey'),
  Bilingual = make_coloured_icon('violet'),
  Dual = make_coloured_icon('violet')
)
language_palette <- colorFactor(c("red", "#333333", "#982FC7", "#982FC7"), domain = language_factors, ordered = TRUE)

# Rural = green, not-rural = blue
rural_factors <- c("yes", "No")
rural_icons <- iconList(
  Yes = make_coloured_icon('green'),
  No = make_coloured_icon('blue')
)
rural_palette <- colorFactor(c("green", "blue"), domain = rural_factors, ordered = TRUE)

# FSM bands
fsm_band_factors <- c("<10%", "10-20%", "20-30%", "30-40%", "40-50%", ">50%")
fsm_band_icons <- iconList(
  '<10%' = make_coloured_icon('grey'),
  '10-20%' = make_coloured_icon('blue'),
  '20-30%' = make_coloured_icon('yellow'),
  '30-40%' = make_coloured_icon('orange'),
  '40-50%' = make_coloured_icon('red'),
  '>50%' = make_coloured_icon('violet')
)
fsm_band_palette <- colorFactor(c("grey", "blue", "yellow", "orange", "red", "violet"), domain = fsm_band_factors, ordered = TRUE)

map_support_categories_by_school_type <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_SUPPORT_CATEGORY_YEAR
  html_legend <- "Support Category</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png' width='12' height='20'>Green<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Yellow<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>Amber<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Red"
  schools_tidy_filtered = schools_tidy %>%
    filter(year == yr) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(!is.na(support_category))
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
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "support_category", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_support_categories <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_SUPPORT_CATEGORY_YEAR
  html_legend <- "Support Category</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png' width='12' height='20'>Green<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Yellow<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>Amber<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Red"
  schools_tidy_filtered = schools_tidy %>%
    filter(year == yr) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(!is.na(support_category))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (sc in support_category_factors) {
    d = schools_tidy_filtered[schools_tidy_filtered$support_category == sc,]
    map <- map %>% addCircleMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, group = sc, color = ~support_category_palette(support_category), radius = 4, stroke = FALSE, fillOpacity = 0.8)
  }
  map <- map %>%
    addLayersControl(overlayGroups = support_category_factors, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "support_category_alt", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_support_categories_by_local_authority <- function(secondaries_tidy_geo, school_type, save_to_file=FALSE) {
  yr = LATEST_SUPPORT_CATEGORY_YEAR
  html_legend <- "Support Category</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png' width='12' height='20'>Green<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Yellow<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>Amber<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Red"
  st <- school_type
  las <- as.character(unique(secondaries_tidy_geo$local_authority))
  secondaries_tidy_geo_filtered = secondaries_tidy_geo %>%
    filter(year == yr) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(!is.na(support_category))
  map <- secondaries_tidy_geo_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = secondaries_tidy_geo_filtered[secondaries_tidy_geo_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, icon=~support_category_icons[support_category], group=la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(NULL, school_type, "support_category_with_la", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_outturn_surplus_or_deficit_by_year <- function(secondaries_tidy_geo_all_years, la = NULL, school_type, save_to_file=FALSE) {
  st <- school_type
  years <- unique(secondaries_tidy_geo_all_years$year)
  years <- years[str_detect(years, '-')] # only school years
  secondaries_tidy_geo_all_years_filtered <- secondaries_tidy_geo_all_years %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    rowwise() %>% # needed since following mutate uses a function
    mutate(surplus_or_deficit = to_surplus_or_deficit_category(school_type, budget_outturn))
  map <- secondaries_tidy_geo_all_years_filtered %>%
    leaflet() %>%
    addTiles()
  for (year in years) {
    d = secondaries_tidy_geo_all_years_filtered[secondaries_tidy_geo_all_years_filtered$year == year,]
    map <- map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(budget_outturn)), icon=~surplus_or_deficit_icons[surplus_or_deficit], group = year)
  }
  map <- map %>%
    addLayersControl(baseGroups = years, options = layersControlOptions(collapsed = FALSE))
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, school_type, "outturn_surplus_or_deficit", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_per_pupil_funding <- function(schools_tidy, school_type, la = NULL, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  st = school_type
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(school_type == st) %>%
    filter(!is.na(per_pupil_funding))
  q <- quantile(schools_tidy_filtered$per_pupil_funding)
  q1 <- format_gbp(round(q[['25%']], 0))
  q2 <- format_gbp(round(q[['50%']], 0))
  q3 <- format_gbp(round(q[['75%']], 0))
  html_legend <- str_interp("Per-pupil funding</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>Top 25% (>${q3})<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>Middle 50% (${q1}-${q3})<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Bottom 25% (<${q1})")
  schools_tidy_filtered <- schools_tidy_filtered %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) # filter by LA last since the bands are computed across all LAs
  schools_tidy_filtered$per_pupil_funding_band <- cut(schools_tidy_filtered$per_pupil_funding, breaks=c(-Inf, q[['25%']], q[['50%']], q[['75%']], Inf), labels=c("q1","q2", "q3", "q4"))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  if (is.null(la)) {
    map <- map %>% addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE))
  }
  for (lauth in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == lauth,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(per_pupil_funding)), icon=~per_pupil_funding_icons[per_pupil_funding_band], group = lauth)
  }
  map <- map %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, school_type, "per_pupil_funding", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_outturn_surplus_or_deficit_by_school_type <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
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
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "outturn_surplus_or_deficit", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_outturn_surplus_or_deficit_by_local_authority <- function(schools_tidy, save_to_file=FALSE) {
  yr = LATEST_OUTTURN_YEAR
  html_legend <- "Budget Outturn</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Over statutory limit<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png' width='12' height='20'>Surplus<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Deficit"
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    rowwise() %>% # needed since following mutate uses a function
    mutate(surplus_or_deficit = to_surplus_or_deficit_category(school_type, budget_outturn))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(budget_outturn)), icon=~surplus_or_deficit_icons[surplus_or_deficit], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(NULL, NULL, "outturn_surplus_or_deficit_with_la", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_outturn_surplus_or_deficit <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
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
  for (sd in surplus_or_deficit_factors) {
    d = schools_tidy_filtered[schools_tidy_filtered$surplus_or_deficit == sd,]
    map <- map %>% addCircleMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(budget_outturn)), group = sd, color = ~surplus_or_deficit_palette(surplus_or_deficit), radius = 4, stroke = FALSE, fillOpacity = 0.8)
  }
  map <- map %>%
    addLayersControl(overlayGroups = surplus_or_deficit_factors, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "outturn_surplus_or_deficit_alt", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_occupancy_by_school_type <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  html_legend <- "Occupancy</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>&lt;50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>50-75%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>75-100%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;100%"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(num_pupils)) %>% # drop rows with no num_pupils
    filter(!is.na(capacity)) %>% # drop rows with no capacity
    mutate(occupancy = 100.0 * num_pupils / capacity) %>%
    mutate(occupancy_band = cut(occupancy, breaks=c(-Inf, 50, 75, 100, Inf), labels=c("<50%","50-75%", "75-100%", ">100%")))
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(occupancy, 1), '%'), icon=~occupancy_band_icons[occupancy_band], group = st)
  }
  map <- map %>%
    addLayersControl(overlayGroups = school_types, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "occupancy", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_occupancy_by_school_size <- function(schools_tidy, school_type, la = NULL, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  st = school_type
  html_legend <- "Occupancy</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>&lt;50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>50-75%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>75-100%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;100%"
  if (school_type == 'primary') {
    size_breaks = c(-Inf, 60, 120, 180, 240, 300, 360, 420, Inf)
    size_bands <- c("<60","60-120", "120-180", "180-240", "240-300", "300-360", "360-420", ">420")
  } else {
    size_breaks = c(-Inf, 600, 1200, 1800, 2400)
    size_bands <- c("<600","600-1200", "1200-1800", "1800-2400")
  }
  schools_tidy_filtered <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(num_pupils)) %>% # drop rows with no num_pupils
    filter(!is.na(capacity)) %>% # drop rows with no capacity
    mutate(occupancy = 100.0 * num_pupils / capacity) %>%
    mutate(occupancy_band = cut(occupancy, breaks=c(-Inf, 50, 75, 100, Inf), labels=c("<50%","50-75%", "75-100%", ">100%"))) %>%
    mutate(size_band=cut(num_pupils, breaks=size_breaks, labels=size_bands))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (sb in size_bands) {
    d = schools_tidy_filtered[schools_tidy_filtered$size_band == sb,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(occupancy, 1), '%, ', num_pupils, ' pupils'), icon=~occupancy_band_icons[occupancy_band], group = sb)
  }
  map <- map %>%
    addLayersControl(overlayGroups = size_bands, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, school_type, "occupancy_with_school_size", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_occupancy_by_school_capacity <- function(schools_tidy, school_type, la = NULL, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  st = school_type
  html_legend <- "Occupancy</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>&lt;50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>50-75%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>75-100%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;100%"
  if (school_type == 'primary') {
    size_breaks = c(-Inf, 60, 120, 180, 240, 300, 360, 420, Inf)
    size_bands <- c("<60","60-120", "120-180", "180-240", "240-300", "300-360", "360-420", ">420")
  } else {
    size_breaks = c(-Inf, 600, 1200, 1800, 2400)
    size_bands <- c("<600","600-1200", "1200-1800", "1800-2400")
  }
  schools_tidy_filtered <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(num_pupils)) %>% # drop rows with no num_pupils
    filter(!is.na(capacity)) %>% # drop rows with no capacity
    mutate(occupancy = 100.0 * num_pupils / capacity) %>%
    mutate(occupancy_band = cut(occupancy, breaks=c(-Inf, 50, 75, 100, Inf), labels=c("<50%","50-75%", "75-100%", ">100%"))) %>%
    mutate(size_band=cut(capacity, breaks=size_breaks, labels=size_bands))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (sb in size_bands) {
    d = schools_tidy_filtered[schools_tidy_filtered$size_band == sb,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(occupancy, 1), '%, capacity ', capacity), icon=~occupancy_band_icons[occupancy_band], group = sb)
  }
  map <- map %>%
    addLayersControl(overlayGroups = size_bands, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, school_type, "occupancy_with_school_capacity", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_occupancy_by_local_authority <- function(schools_tidy, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  html_legend <- "Occupancy</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>&lt;50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>50-75%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>75-100%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;100%"
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(!is.na(num_pupils)) %>% # drop rows with no num_pupils
    filter(!is.na(capacity)) %>% # drop rows with no capacity
    mutate(occupancy = 100.0 * num_pupils / capacity) %>%
    mutate(occupancy_band = cut(occupancy, breaks=c(-Inf, 50, 75, 100, Inf), labels=c("<50%","50-75%", "75-100%", ">100%")))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(occupancy, 1), '%'), icon=~occupancy_band_icons[occupancy_band], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(NULL, NULL, "occupancy_with_la", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_occupancy <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  html_legend <- "Occupancy</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>&lt;50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>50-75%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>75-100%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;100%"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(num_pupils)) %>% # drop rows with no num_pupils
    filter(!is.na(capacity)) %>% # drop rows with no capacity
    mutate(occupancy = 100.0 * num_pupils / capacity) %>%
    mutate(occupancy_band = cut(occupancy, breaks=c(-Inf, 50, 75, 100, Inf), labels=c("<50%","50-75%", "75-100%", ">100%")))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (ob in occupancy_band_factors) {
    d = schools_tidy_filtered[schools_tidy_filtered$occupancy_band == ob,]
    map <- map %>% addCircleMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(occupancy, 1), '%'), group = ob, color = ~occupancy_band_palette(occupancy_band), radius = 4, stroke = FALSE, fillOpacity = 0.8)
  }
  map <- map %>%
    addLayersControl(overlayGroups = occupancy_band_factors, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "occupancy_alt", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_language_by_school_type <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_LANGUAGE_YEAR
  html_legend <- "Language Provision</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Welsh<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>English<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>Bilingual/Dual<br/>"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(school_type != 'special') %>% # special schools don't have a language
    filter(!is.na(language))
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, icon=~language_icons[language], group = st)
  }
  map <- map %>%
    addLayersControl(overlayGroups = school_types, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "language", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_language_by_local_authority <- function(schools_tidy, save_to_file=FALSE) {
  yr = LATEST_LANGUAGE_YEAR
  html_legend <- "Language Provision</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Welsh<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>English<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>Bilingual/Dual<br/>"
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(school_type != 'special') %>% # special schools don't have a language
    filter(!is.na(language))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, icon=~language_icons[language], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(NULL, NULL, "language_with_la", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_language <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_LANGUAGE_YEAR
  html_legend <- "Language Provision</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Welsh<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>English<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>Bilingual/Dual<br/>"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(school_type != 'special') %>% # special schools don't have a language
    filter(!is.na(language))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (lang in language_factors) {
    d = schools_tidy_filtered[schools_tidy_filtered$language == lang,]
    map <- map %>% addCircleMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~school, group = lang, color = ~language_palette(language), radius = 4, stroke = FALSE, fillOpacity = 0.8)
  }
  map <- map %>%
    addLayersControl(overlayGroups = language_factors, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "language_alt", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_rural_schools <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_YEAR
  html_legend <- "Rural Schools</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png' width='12' height='20'>Yes<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>No"
  map <- schools_tidy %>%
    filter(year == yr) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(school_type == 'primary') %>% # rural schools are primaries
    mutate(rural_school = replace(rural_school, is.na(rural_school), 'No')) %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude, popup = ~school, label=~school, icon=~rural_icons[rural_school]) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "rural_schools", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_fsm_by_school_type <- function(schools_tidy, la = NULL, save_to_file=FALSE) {
  yr = LATEST_FSM_YEAR
  html_legend <- "FSM rate</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>&lt;10%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>10-20%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>20-30%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>30-40%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>40-50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;50%"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(fsm_rate)) %>% # drop rows with no FSM
    mutate(fsm_band = cut(fsm_rate, breaks=c(-Inf, 10, 20, 30, 40, 50, Inf), labels=fsm_band_factors))
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (st in school_types) {
    d = schools_tidy_filtered[schools_tidy_filtered$school_type == st,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(fsm_rate, 1), '%'), icon=~fsm_band_icons[fsm_band], group = st)
  }
  map <- map %>%
    addLayersControl(overlayGroups = school_types, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(la, NULL, "fsm", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_fsm_by_local_authority <- function(schools_tidy, save_to_file=FALSE) {
  yr = LATEST_FSM_YEAR
  html_legend <- "FSM rate</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>&lt;10%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>10-20%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>20-30%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>30-40%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>40-50%<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>&gt;50%"
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(!is.na(fsm_rate)) %>% # drop rows with no FSM
    mutate(fsm_band = cut(fsm_rate, breaks=c(-Inf, 10, 20, 30, 40, 50, Inf), labels=fsm_band_factors))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(fsm_rate, 1), '%'), icon=~fsm_band_icons[fsm_band], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(NULL, NULL, "fsm_with_la", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}
