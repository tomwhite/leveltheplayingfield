library(leaflet)

# Load location data
# This was batch geocoded using https://www.doogal.co.uk/BatchGeocoding.php
school_locations <- read.csv("~/projects-workspace/leveltheplayingfield/data/geo/school-number-postcodes-geocoded.csv", header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) %>%
  mutate_at("Latitude", as.numeric) %>%
  mutate_at("Longitude", as.numeric)

# Show a basic map with markers for all of Wales
leaflet(data = school_locations) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude)

# Filter to Powys
powys_schools_with_lat_long <- schools_tidy %>%
  filter(!is.na(local_authority)) %>%
  filter(year == '2018-19') %>%
  filter(local_authority == 'Powys') %>%
  inner_join(school_locations, by = c("lea_code" = "School.Number"))

# Show a basic map with markers for Powys
leaflet(data = powys_schools_with_lat_long) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~school)

# Show a basic map with circle markers for Powys
leaflet(data = powys_schools_with_lat_long) %>%
  addTiles() %>%
  addCircles(~Longitude, ~Latitude, popup = ~school, weight = 4, radius=50, 
           color="red", stroke = TRUE, fillOpacity = 0.8)

# Show a map with markers indicating school size
getColor <- function(schools) {
  sapply(schools$num_pupils, function(num_pupils) {
    if(num_pupils <= 50) {
      "green"
    } else if(num_pupils <= 100) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(powys_schools_with_lat_long)
)

leaflet(data = powys_schools_with_lat_long) %>%
  addTiles() %>%
  addAwesomeMarkers(~Longitude, ~Latitude, popup = ~school, label=~as.character(num_pupils), icon=icons)
