#
# Reports for the response to the Transforming Education in Powys (Feb 2020)
#

source('load_data.R')
source('maps.R')
source('schools.R')
source('utils.R')

PREFIX = "2020-02-15-transforming-education-in-powys-response"

compute_school_funding_redistribution  <- function(schools_tidy, st, la, yr) {
  schools_tidy_filtered <- schools_tidy %>%
    filter(school_type %in% st) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(total_school_delegated_budget)) %>%
    mutate(school = str_replace(school, "Ysgol Trefonnen Church in Wales Community Primary/Llandrindod Wells C.I.W. School", "Ysgol Trefonnen C.I.W. Community Primary"))
  
  total_delegated_budget <- schools_tidy_filtered %>%
    group_by(local_authority) %>%
    summarize(total = sum(total_school_delegated_budget)) %>%
    pull()
  
  total_num_pupils <- schools_tidy_filtered %>%
    group_by(local_authority) %>%
    summarize(total = sum(num_pupils)) %>%
    pull()
  
  schools_tidy_filtered %>%
    mutate(num_pupils_pct = 100 * num_pupils / total_num_pupils) %>%
    mutate(delegated_budget_pct = 100 * total_school_delegated_budget / total_delegated_budget) %>%
    mutate(budget_difference = total_school_delegated_budget - (num_pupils_pct * total_delegated_budget / 100)) %>%
    mutate(budget_difference_pct = 100 * budget_difference / total_school_delegated_budget)  
}

plot_school_funding_redistribution <- function(schools_tidy, st, la, yr=LATEST_NUM_PUPILS_YEAR, save_to_file=FALSE) {
  plot <- compute_school_funding_redistribution(schools_tidy, st, la, yr) %>%
    ggplot(aes(reorder(school, budget_difference), budget_difference)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Actual weighted redistribution (£)") +
    labs(title = paste0("Actual weighted redistribution of funding based on ", la, " formula (", yr, ")"),
         subtitle = paste0(la, " ", st, " schools")) +
    theme(axis.title.y=element_blank()) +
    scale_y_continuous(labels = comma) +
    ylim(-1400000, 700000)
  if (save_to_file) {
    ggsave(blog_post_file_name(PREFIX, la, st[1][1], "school_funding_redistribution", yr, ".png"))
  }
  plot
}

plot_school_funding_redistribution_pct <- function(schools_tidy, st, la, yr=LATEST_NUM_PUPILS_YEAR, save_to_file=FALSE) {
  plot <- compute_school_funding_redistribution(schools_tidy, st, la, yr) %>%
    ggplot(aes(reorder(school, budget_difference_pct), budget_difference_pct)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Actual weighted redistribution as percentage of budget") +
    labs(title = paste0("Actual weighted redistribution of funding based on ", la, " formula (", yr, ")"),
         subtitle = paste0(la, " ", st, " schools")) +
    theme(axis.title.y=element_blank()) +
    scale_y_continuous(labels = comma) +
    ylim(-40, 30)
  if (save_to_file) {
    ggsave(blog_post_file_name(PREFIX, la, st[1][1], "school_funding_redistribution_pct", yr, ".png"))
  }
  plot
}

# Tweaked for primary - should merge into similar fn above
plot_school_funding_redistribution_pct_pri <- function(schools_tidy, st, la, yr=LATEST_NUM_PUPILS_YEAR, save_to_file=FALSE) {
  plot <- compute_school_funding_redistribution(schools_tidy, st, la, yr) %>%
    ggplot(aes(reorder(school, budget_difference_pct), budget_difference_pct)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Actual weighted redistribution as percentage of budget") +
    labs(title = paste0("Actual weighted redistribution of funding based on ", la, " formula (", yr, ")"),
         subtitle = paste0(la, " ", st, " schools")) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_text(size=7)) +
    scale_y_continuous(labels = comma) +
    ylim(-30, 55)
  if (save_to_file) {
    ggsave(blog_post_file_name(PREFIX, la, st[1][1], "school_funding_redistribution_pct", yr, ".png"))
  }
  plot
}

for (yr in c("2016-17", "2019-20")) {
  plot_school_funding_redistribution_pct_pri(all_schools, c('primary'), 'Powys', yr, save_to_file=TRUE)

  plot_school_funding_redistribution(all_schools, c('secondary', 'through'), 'Powys', yr, save_to_file=TRUE)
  plot_school_funding_redistribution(all_schools, c('secondary', 'through'), 'Gwynedd', yr, save_to_file=TRUE)
  
  plot_school_funding_redistribution_pct(all_schools, c('secondary', 'through'), 'Powys', yr, save_to_file=TRUE)
  plot_school_funding_redistribution_pct(all_schools, c('secondary', 'through'), 'Gwynedd', yr, save_to_file=TRUE)
}

map_language_welsh_or_bilingual <- function(schools_tidy, st, la = NULL, save_to_file=FALSE) {
  yr = LATEST_YEAR
  html_legend <- "Language Provision</br>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Welsh<br/>
<img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>Bilingual<br/>"
  schools_tidy_filtered <- schools_tidy %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr) %>%
    filter(school_type != 'special') %>% # special schools don't have a language
    filter(!is.na(language)) %>%
    filter(school_type == st) %>%
    filter(language %in% c("Welsh", "Dual", "Bilingual"))
  school_types <- as.character(unique(schools_tidy_filtered$school_type))
  map <- schools_tidy_filtered %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude, popup = ~school, label=~school, icon=~language_icons[language])
  map <- map %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, blog_post_file_name(PREFIX, la, st, "language_welsh_or_bilingual", yr, ".html"))
  }
  map
}

map_language_welsh_or_bilingual(all_schools, 'primary', 'Powys', save_to_file=TRUE)
