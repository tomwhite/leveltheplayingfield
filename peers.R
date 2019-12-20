library(DescTools) # for Gini

plot_size_vs_fsm <- function(schools_tidy, st, la, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  schools_tidy_filtered <- schools_tidy %>%
    filter(year == yr) %>%
    filter(school_type == st) %>%
    filter(!is.na(per_pupil_funding))
  q <- quantile(schools_tidy_filtered$per_pupil_funding)
  q1 <- format_gbp(round(q[['25%']], 0))
  q2 <- format_gbp(round(q[['50%']], 0))
  q3 <- format_gbp(round(q[['75%']], 0))
  
  SCHOOL_SIZE_QUARTILE_COLOURS = c("q1" = "#C92D43", "q2" = "#757575", "q3" = "#757575", "q4" = "#9A25C8")
  
  plot <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == LATEST_FSM_YEAR) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(fsm_rate)) %>%
    mutate(per_pupil_funding_band = cut(per_pupil_funding, breaks=c(-Inf, q[['25%']], q[['50%']], q[['75%']], Inf), labels=c("q1","q2", "q3", "q4"))) %>%
    ggplot(aes(num_pupils, fsm_rate, color = per_pupil_funding_band)) +
    geom_point() +
    scale_x_continuous(breaks = seq(0, 700, 30), minor_breaks = NULL, limits = c(0, 700)) +
    scale_y_continuous(breaks = seq(0, 70, 10), minor_breaks = NULL, limits = c(0, 75)) +
    scale_colour_manual(values = SCHOOL_SIZE_QUARTILE_COLOURS) + 
    xlab("Number of pupils") +
    ylab("Percentage of pupils on free school meals") +
    labs(title = "Distribution of school size and free school meals",
         subtitle = paste0(if (!is.null(la)) la else "Wales", " ", st, " schools"))
  if (save_to_file) {
    ggsave(report_file_name(la, st, "size_vs_fsm", LATEST_FSM_YEAR, ".png"))
  }  
  plot
}

tabulate_per_pupil_funding_peers_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  st <- school_type
  
  table <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == LATEST_FSM_YEAR) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(fsm_rate)) %>%
    mutate(num_pupils_bin = cut_width(num_pupils, width = 30, center = 15, closed="left")) %>%
    mutate(fsm_rate_bin = cut_width(fsm_rate, width = 10, center = 5, closed="left")) %>%
    group_by(num_pupils_bin, fsm_rate_bin) %>%
    summarize(n = n(),
              min_per_pupil_funding = min(per_pupil_funding),
              max_per_pupil_funding = max(per_pupil_funding),
              range_per_pupil_funding = max(per_pupil_funding) - min(per_pupil_funding),
              min_la = local_authority[which(per_pupil_funding == min(per_pupil_funding))],
              min_school = school[which(per_pupil_funding == min(per_pupil_funding))],
              max_la = local_authority[which(per_pupil_funding == max(per_pupil_funding))],
              max_school = school[which(per_pupil_funding == max(per_pupil_funding))],
              gini = round(Gini(per_pupil_funding), 2)) %>%
    mutate(Map = paste0("https://tomwhite.github.io/leveltheplayingfield/wales/", st, "_map_per_pupil_funding_peers_", num_pupils_bin, "_", fsm_rate_bin, "_", LATEST_NUM_PUPILS_YEAR, ".html")) %>%
    mutate(Table = paste0("https://tomwhite.github.io/leveltheplayingfield/wales/", st, "_per_pupil_funding_peers_", num_pupils_bin, "_", fsm_rate_bin, "_", LATEST_NUM_PUPILS_YEAR, ".html")) %>%
    rename("Num pupils group" = num_pupils_bin,
           "FSM rate group" = fsm_rate_bin,
           "Num schools" = n,
           "Min PPF" = min_per_pupil_funding,
           "Min PPF" = max_per_pupil_funding,
           "Range PPF" = range_per_pupil_funding,
           "Min LA" = min_la,
           "Min school" = min_school,
           "Max LA" = max_la,
           "Max school" = max_school,
           "Gini coefficient" = gini)
  
  # Create one map per row
  by(table, 1:nrow(table), function(row) map_per_pupil_funding_peers(all_schools, st, row$`Num pupils group`, row$`FSM rate group`, save_to_file = TRUE))

  # Create one table per row
  by(table, 1:nrow(table), function(row) tabulate_per_pupil_funding_peers(all_schools, st, row$`Num pupils group`, row$`FSM rate group`, save_to_file = TRUE))
  
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
  schools_subset_one_bin <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == LATEST_FSM_YEAR) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(fsm_rate)) %>%
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
  
  schools_subset_one_bin <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == LATEST_FSM_YEAR) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(fsm_rate)) %>%
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