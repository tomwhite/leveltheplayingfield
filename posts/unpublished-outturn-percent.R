####
# Distribution of surplus/deficit as a percentage of per-pupil funding
####

source('maps.R')
source('schools.R')

PREFIX = "unpublished-outturn-percent"

schools <- all_schools %>% filter(school_type == 'secondary')
#schools <- all_schools

calculate_budget_outturn_pct <- function(schools, budget_outturn_year = LATEST_OUTTURN_YEAR, total_school_delegated_budget_year = "2019-20") {
  x <- schools %>%
    filter(!is.na(budget_outturn)) %>%
    filter(year == budget_outturn_year) %>%
    select(c(lea_code, budget_outturn))
  
  y <- schools %>%
    filter(!is.na(total_school_delegated_budget)) %>%
    filter(year == total_school_delegated_budget_year) %>% 
    select(c(lea_code, total_school_delegated_budget))
  
  x %>%
    left_join(y) %>%
    filter(!is.na(budget_outturn)) %>%
    filter(!is.na(total_school_delegated_budget)) %>%
    mutate(budget_outturn_pct = 100.0 * budget_outturn / total_school_delegated_budget) %>%
    left_join(all_schools %>% filter(year == budget_outturn_year), by = "lea_code", suffix = c("", ".y")) %>%
    select(-c(budget_outturn.y, total_school_delegated_budget.y))  
}

schools_with_budget_outturn_pct <- calculate_budget_outturn_pct(schools)

# Extremes
schools_with_budget_outturn_pct %>%
  filter(budget_outturn_pct > 50 | budget_outturn_pct < -50) %>%
  select(c(local_authority, school, school_type, total_school_delegated_budget, budget_outturn, budget_outturn_pct))

# Plot distribution
schools_with_budget_outturn_pct %>%
  filter(budget_outturn_pct > -50 & budget_outturn_pct < 50) %>%
  ggplot(aes(budget_outturn_pct)) +
  geom_histogram(binwidth = 1, colour="black", fill="white")

quantile(schools_with_budget_outturn_pct$budget_outturn)
quantile(schools_with_budget_outturn_pct$budget_outturn_pct)
quantile(schools_with_budget_outturn_pct$total_school_delegated_budget)

# Which percentile does the primary £50,000 (or £100,000 for secondary) excess budget outturn occur at?
# See https://stats.stackexchange.com/questions/50080/estimate-quantile-of-value-in-a-vector
ecdf_fun <- function(x,perc) ecdf(x)(perc)
ecdf_fun(schools_with_budget_outturn_pct$budget_outturn, 100000)
# answer is 66% for primary and 65% for secondary
# (also around 16% of all schools are in deficit)
quantile(schools_with_budget_outturn_pct$budget_outturn, c(0.66))
quantile(schools_with_budget_outturn_pct$budget_outturn_pct, c(0.66)) # 7.5% primary, 2.5% secondary, 6.9% all

# Make maps for all of Wales with four categories:
# Deficit, Surplus < x%, Surplus < y%, Surplus > y%
# Where x = 2.5%, y = 5%, and x = 7.5%, y = 15%
# So this allows us to look at primaries as secondaries, and secondaries as primaries - in budget surplus terms at least

budget_outturn_pct_icons_2_5 <- iconList(
  `Surplus (>5%)` = make_coloured_icon('yellow'),
  `Surplus (2.5-5%)` = make_coloured_icon('blue'),
  `Surplus (<2.5%)` = make_coloured_icon('black'),
  `Deficit` = make_coloured_icon('red')
)

map_outturn_surplus_or_deficit_pct_by_local_authority_2_5 <- function(schools_tidy, school_type, save_to_file=FALSE) {
  st <- school_type
  yr = LATEST_OUTTURN_YEAR
  html_legend <- "Budget Outturn</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Surplus (>5%)<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>Surplus (2.5-5%)<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png' width='12' height='20'>Surplus (<2.5%)<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Deficit"
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    mutate(budget_outturn_pct_band = cut(budget_outturn_pct, breaks=c(-Inf, 0, 2.5, 5, Inf), labels=c("Deficit", "Surplus (<2.5%)", "Surplus (2.5-5%)", "Surplus (>5%)")))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(budget_outturn), ',', paste0(round(budget_outturn_pct, 1), "%")), icon=~budget_outturn_pct_icons_2_5[budget_outturn_pct_band], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidget(map, blog_post_file_name(PREFIX, NULL, st, "outturn_surplus_or_deficit_with_la_2_5", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

budget_outturn_pct_icons_7_5 <- iconList(
  `Surplus (>15%)` = make_coloured_icon('yellow'),
  `Surplus (7.5-15%)` = make_coloured_icon('blue'),
  `Surplus (<7.5%)` = make_coloured_icon('black'),
  `Deficit` = make_coloured_icon('red')
)

map_outturn_surplus_or_deficit_pct_by_local_authority_7_5 <- function(schools_tidy, school_type, save_to_file=FALSE) {
  st <- school_type
  yr = LATEST_OUTTURN_YEAR
  html_legend <- "Budget Outturn</br>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-yellow.png' width='12' height='20'>Surplus (>15%)<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png' width='12' height='20'>Surplus (7.5-15%)<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png' width='12' height='20'>Surplus (<7.5%)<br/>
  <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Deficit"
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(!is.na(budget_outturn)) %>% # drop rows with no budget_outturn
    mutate(budget_outturn_pct_band = cut(budget_outturn_pct, breaks=c(-Inf, 0, 7.5, 15, Inf), labels=c("Deficit", "Surplus (<7.5%)", "Surplus (7.5-15%)", "Surplus (>15%)")))
  las <- as.character(unique(schools_tidy$local_authority))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles()
  for (la in las) {
    d = schools_tidy_filtered[schools_tidy_filtered$local_authority == la,]
    map = map %>% addMarkers(data = d, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', format_gbp(budget_outturn), ',', paste0(round(budget_outturn_pct, 1), "%")), icon=~budget_outturn_pct_icons_7_5[budget_outturn_pct_band], group = la)
  }
  map <- map %>%
    addLayersControl(overlayGroups = las, options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidget(map, blog_post_file_name(PREFIX, NULL, st, "outturn_surplus_or_deficit_with_la_7_5", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}

schools_with_budget_outturn_pct <- calculate_budget_outturn_pct(all_schools)

map_outturn_surplus_or_deficit_pct_by_local_authority_2_5(schools_with_budget_outturn_pct, 'primary', save_to_file = TRUE)
map_outturn_surplus_or_deficit_pct_by_local_authority_2_5(schools_with_budget_outturn_pct, 'secondary', save_to_file = TRUE)
map_outturn_surplus_or_deficit_pct_by_local_authority_7_5(schools_with_budget_outturn_pct, 'primary', save_to_file = TRUE)
map_outturn_surplus_or_deficit_pct_by_local_authority_7_5(schools_with_budget_outturn_pct, 'secondary', save_to_file = TRUE)
