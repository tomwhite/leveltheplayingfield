library(DescTools) # for Gini

# Propagate the latest FSM rates to the latest num pupils year
get_filled_in_fsm_rate <- function(schools_tidy, st, la) {
  schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == LATEST_FSM_YEAR | year == LATEST_NUM_PUPILS_YEAR) %>%
    group_by(local_authority, school) %>%
    fill(fsm_rate, .direction="updown") %>%
    ungroup() %>%
    filter(year == LATEST_NUM_PUPILS_YEAR) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(fsm_rate))
}

plot_size_vs_fsm <- function(schools_tidy, st, la, save_to_file=FALSE, num_pupils_bin_size=30, num_pupils_limit=700) {
  yr = LATEST_NUM_PUPILS_YEAR
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(school_type == st) %>%
    filter(!is.na(per_pupil_funding))
  q <- quantile(schools_tidy_filtered$per_pupil_funding)
  q0 <- format_gbp(round(q[['0%']], 0))
  q1 <- format_gbp(round(q[['25%']], 0))
  q2 <- format_gbp(round(q[['50%']], 0))
  q3 <- format_gbp(round(q[['75%']], 0))
  q4 <- format_gbp(round(q[['100%']], 0))
  
  SCHOOL_SIZE_QUARTILE_COLOURS = c("#C92D43", "#999999", "#757575", "#9A25C8")
  SCHOOL_SIZE_QUARTILE_LABELS = c(str_interp("${q0} - ${q1}"), str_interp("${q1} - ${q2}"), str_interp("${q2} - ${q3}"), str_interp("${q3} - ${q4}"))
  
  plot <- get_filled_in_fsm_rate(schools_tidy, st, la) %>%
    mutate(per_pupil_funding_band = cut(per_pupil_funding, breaks=c(-Inf, q[['25%']], q[['50%']], q[['75%']], Inf), labels = SCHOOL_SIZE_QUARTILE_LABELS)) %>%
    ggplot(aes(num_pupils, fsm_rate, color = per_pupil_funding_band, text = paste0(format_gbp(per_pupil_funding), ", ", local_authority, ", ", school))) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, num_pupils_limit, num_pupils_bin_size), minor_breaks = NULL, limits = c(0, num_pupils_limit)) +
    scale_y_continuous(breaks = seq(0, 70, 10), minor_breaks = NULL, limits = c(0, 75)) +
    scale_colour_manual("Per-pupil funding (£)", values = SCHOOL_SIZE_QUARTILE_COLOURS) + 
    xlab("Number of pupils") +
    ylab("Percentage of pupils on free school meals") +
    labs(title = "Distribution of school size and free school meals (2019-20)",
         subtitle = paste0(if (!is.null(la)) la else "Wales", " ", st, " schools"))
  if (save_to_file) {
    ggsave(report_file_name(la, st, "size_vs_fsm", yr, ".png"))
  }  
  plot
}

plot_size_vs_fsm_interactive <- function(schools_tidy, st, la, save_to_file=FALSE, num_pupils_bin_size=30, num_pupils_limit=700) {
  library(plotly)
  
  yr = LATEST_NUM_PUPILS_YEAR
  plot <- plot_size_vs_fsm(all_schools, st, la, save_to_file = FALSE, num_pupils_bin_size=num_pupils_bin_size, num_pupils_limit=num_pupils_limit) %>%
    ggplotly()
  if (save_to_file) {
    saveWidgetFix(plot, report_file_name(la, st, "size_vs_fsm", yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  plot
}

tabulate_per_pupil_funding_peers_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  st <- school_type
  
  table <- get_filled_in_fsm_rate(schools_tidy, st, NULL) %>%
    mutate(num_pupils_bin = cut_width(num_pupils, width = 30, center = 15, closed="left")) %>%
    mutate(fsm_rate_bin = cut_width(fsm_rate, width = 10, center = 5, closed="left")) %>%
    group_by(num_pupils_bin, fsm_rate_bin) %>%
    summarize(n = n(),
              min_per_pupil_funding = min(per_pupil_funding),
              max_per_pupil_funding = max(per_pupil_funding),
              range_per_pupil_funding = max(per_pupil_funding) - min(per_pupil_funding),
              # use toString in below since per_pupil_funding is not necessarily unique 
              min_la = toString(local_authority[which(per_pupil_funding == min(per_pupil_funding))]),
              min_school = toString(school[which(per_pupil_funding == min(per_pupil_funding))]),
              max_la = toString(local_authority[which(per_pupil_funding == max(per_pupil_funding))]),
              max_school = toString(school[which(per_pupil_funding == max(per_pupil_funding))]),
              gini = round(Gini(per_pupil_funding), 2)) %>%
    mutate(Map = paste0("https://tomwhite.github.io/leveltheplayingfield/wales/", st, "_map_per_pupil_funding_peers_", num_pupils_bin, "_", fsm_rate_bin, "_", yr, ".html")) %>%
    mutate(Table = paste0("https://tomwhite.github.io/leveltheplayingfield/wales/", st, "_per_pupil_funding_peers_", num_pupils_bin, "_", fsm_rate_bin, "_", yr, ".html")) %>%
    rename("Num pupils group (2019-20)" = num_pupils_bin,
           "FSM rate group (2018-19)" = fsm_rate_bin,
           "Num schools" = n,
           "Min PPF (2019-20)" = min_per_pupil_funding,
           "Max PPF" = max_per_pupil_funding,
           "Range PPF" = range_per_pupil_funding,
           "Min LA" = min_la,
           "Min school" = min_school,
           "Max LA" = max_la,
           "Max school" = max_school,
           "Gini coefficient" = gini)
  
  # Create one map per row
  by(table, 1:nrow(table), function(row) map_per_pupil_funding_peers(all_schools, st, row$`Num pupils group (2019-20)`, row$`FSM rate group (2018-19)`, save_to_file = TRUE))

  # Create one table per row
  by(table, 1:nrow(table), function(row) tabulate_per_pupil_funding_peers(all_schools, st, row$`Num pupils group (2019-20)`, row$`FSM rate group (2018-19)`, save_to_file = TRUE))
  
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 200,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidgetFix(dt, report_file_name(NULL, school_type, "per_pupil_funding_peers_summary", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}

tabulate_per_pupil_funding_peers <- function(schools_tidy, school_type, num_pupils_band, fsm_rate_band, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  st = school_type
  schools_subset_one_bin <- get_filled_in_fsm_rate(schools_tidy, st, NULL) %>%
    mutate(num_pupils_bin = cut_width(num_pupils, width = 30, center = 15, closed="left")) %>%
    mutate(fsm_rate_bin = cut_width(fsm_rate, width = 10, center = 5, closed="left")) %>%
    filter(num_pupils_bin == num_pupils_band) %>%
    filter(fsm_rate_bin == fsm_rate_band) %>%
    select(-c(rural_school, year, size, support_category, support_category_days, latitude, longitude, school_type))
  schools_subset_one_bin$per_pupil_funding_band <- cut(schools_subset_one_bin$per_pupil_funding, breaks=c(-Inf, q[['25%']], q[['50%']], q[['75%']], Inf), labels=c("q1","q2", "q3", "q4"))
  
  table <- schools_subset_one_bin
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 200,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidgetFix(dt, report_file_name(NULL, school_type, paste0("per_pupil_funding_peers_", num_pupils_band, "_", fsm_rate_band), yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}

map_per_pupil_funding_peers <- function(schools_tidy, school_type, num_pupils_band, fsm_rate_band, save_to_file=FALSE) {
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
  
  html_legend <- str_interp("Per-pupil funding<br/>school size band ${num_pupils_band}<br/>FSM rate band ${fsm_rate_band}</br>
                            <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png' width='12' height='20'>Top 25% (>${q3})<br/>
                            <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-grey.png' width='12' height='20'>Middle 50% (${q1}-${q3})<br/>
                            <img src='https://cdn.rawgit.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png' width='12' height='20'>Bottom 25% (<${q1})")
  
  schools_subset_one_bin <- get_filled_in_fsm_rate(schools_tidy, st, NULL) %>%
    mutate(num_pupils_bin = cut_width(num_pupils, width = 30, center = 15, closed="left")) %>%
    mutate(fsm_rate_bin = cut_width(fsm_rate, width = 10, center = 5, closed="left")) %>%
    filter(num_pupils_bin == num_pupils_band) %>%
    filter(fsm_rate_bin == fsm_rate_band)
  schools_subset_one_bin$per_pupil_funding_band <- cut(schools_subset_one_bin$per_pupil_funding, breaks=c(-Inf, q[['25%']], q[['50%']], q[['75%']], Inf), labels=c("q1","q2", "q3", "q4"))
  map <- schools_subset_one_bin %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(data = schools_subset_one_bin, ~longitude, ~latitude, popup = ~school, label=~paste(school, ',', local_authority, ',', format_gbp(per_pupil_funding), ',', num_pupils, ',', fsm_rate), icon=~per_pupil_funding_icons[per_pupil_funding_band]) %>%
    addControl(html = html_legend)
  if (save_to_file) {
    saveWidgetFix(map, report_file_name(NULL, st, paste0("map_per_pupil_funding_peers_", num_pupils_band, "_", fsm_rate_band), yr, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  map
}
