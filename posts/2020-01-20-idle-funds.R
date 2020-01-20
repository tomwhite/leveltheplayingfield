#
# A map of primary schools with between 0 and 50K of reserves, banded into >5%, 5-10%, >20% of budget
#

source('load_data.R')
source('maps.R')
source('schools.R')
source('utils.R')

PREFIX = "2020-01-20-idle_funds"

# Colours
idle_funds_icons <- iconList(
  "5-10%" = make_coloured_icon('yellow'),
  "10-20%" = make_coloured_icon('orange'),
  ">20%" = make_coloured_icon('red')
)

map_schools_with_idle_funds <- function(schools_tidy, school_type, save_to_file=FALSE) {
  yr = LATEST_OUTTURN_YEAR
  html_legend <- "Outturn % of budget</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>5-10%<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png' width='12' height='20'>10-20%<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>&gt;20%"
  st = school_type
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(school_type == st) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    filter(!is.na(total_school_delegated_budget)) %>% # drop rows with no budget
    filter(budget_outturn > 0 & budget_outturn < 50000) %>%
    mutate(budget_outturn_pct_of_delegated_budget = 100 * budget_outturn / total_school_delegated_budget) %>%
    filter(budget_outturn_pct_of_delegated_budget >= 5) %>%
    mutate(budget_outturn_pct_band = cut(budget_outturn_pct_of_delegated_budget, breaks=c(-Inf, 5, 10, 20, Inf), labels=c("<5","5-10%", "10-20%", ">20%")))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste0(school, ', ', round(budget_outturn_pct_of_delegated_budget, 1), "%"), icon=~idle_funds_icons[budget_outturn_pct_band], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, blog_post_file_name(PREFIX, NULL, NULL, "budget_outturn", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

map_schools_with_idle_funds(all_schools, 'primary', save_to_file = TRUE)
