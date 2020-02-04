#
# Bar charts comparing PPF quartiles for each LA
#

source('load_data.R')
source('maps.R')
source('schools.R')
source('utils.R')

PREFIX = "2020-02-04-ppf-quartiles"

plot_percentage_of_pupils_in_ppf_bands_by_la <- function(schools_tidy, st, order='q1', fsm_only=FALSE, save_to_file=FALSE) {
  yr = LATEST_NUM_PUPILS_YEAR
  x <- schools_tidy %>%
    filter(year == yr) %>%
    filter(school_type == st) %>%
    filter(!is.na(per_pupil_funding))
  q <- quantile(x$per_pupil_funding)
  q0 <- format_gbp(round(q[['0%']], 0))
  q1 <- format_gbp(round(q[['25%']], 0))
  q2 <- format_gbp(round(q[['50%']], 0))
  q3 <- format_gbp(round(q[['75%']], 0))
  q4 <- format_gbp(round(q[['100%']], 0))
  
  SCHOOL_SIZE_QUARTILE_COLOURS = c("#C92D43", "#999999", "#757575", "#9A25C8")
  SCHOOL_SIZE_QUARTILE_LABELS = c(str_interp("${q0} - ${q1}"), str_interp("${q1} - ${q2}"), str_interp("${q2} - ${q3}"), str_interp("${q3} - ${q4}"))
  
  y <- x %>%
    mutate(per_pupil_funding_band = cut(per_pupil_funding, breaks=c(-Inf, q[['25%']], q[['50%']], q[['75%']], Inf), labels = c("q1", "q2", "q3", "q4"))) %>%
    mutate(per_pupil_funding_band = fct_rev(per_pupil_funding_band)) %>%
    group_by(local_authority, per_pupil_funding_band)
  
  if (fsm_only) {
    y <- y %>%
      summarize(count=sum(num_pupils_on_fsm, na.rm=TRUE)) # count schools with no FSM numbers as 0
  } else {
    y <- y %>%
      summarize(count=sum(num_pupils))
  }
  y <- y %>%
    group_by(local_authority) %>%
    mutate(perc= 100 * count/sum(count)) %>%
    ungroup()
  
  if (order == 'q1') {
    la_order <- y %>% select(-c(count)) %>% spread(per_pupil_funding_band, perc) %>%
      replace(., is.na(.), 0) %>%
      arrange(desc(q1), q4) %>%
      pull(local_authority)
  } else {
    la_order <- y %>% select(-c(count)) %>% spread(per_pupil_funding_band, perc) %>%
      replace(., is.na(.), 0) %>%
      arrange(desc(q4), q1) %>%
      pull(local_authority)
  }
  
  if (fsm_only) {
    who <- "FSM pupils"
  } else {
    who <- "pupils"
  }
  
  plot <- y %>%
    mutate(local_authority = fct_relevel(local_authority, la_order)) %>% # sort
    ggplot(aes(local_authority, perc, fill=per_pupil_funding_band)) +
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(values=rev(SCHOOL_SIZE_QUARTILE_COLOURS), labels=rev(SCHOOL_SIZE_QUARTILE_LABELS), name="Per-pupil funding band") +
    xlab("Local authority") +
    ylab(paste0("Percentage of ", who)) +
    labs(title = paste("Proportion of", st, who, "by funding band for each local authority")) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  if (save_to_file) {
    if (fsm_only) {
      who <- "fsm_pupils"
    } else {
      who <- "pupils"
    }
    ggsave(blog_post_file_name(PREFIX, NULL, st, paste0("percentage_of_", who, "_in_ppf_bands_by_la_", order), yr, ".png"))
  }
  plot
}

plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'primary', order = 'q1', save_to_file = TRUE)
plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'primary', order = 'q4', save_to_file = TRUE)
plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'secondary', order = 'q1', save_to_file = TRUE)
plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'secondary', order = 'q4', save_to_file = TRUE)

plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'primary', order = 'q1', fsm_only = TRUE, save_to_file = TRUE)
plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'primary', order = 'q4', fsm_only = TRUE, save_to_file = TRUE)
plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'secondary', order = 'q1', fsm_only = TRUE, save_to_file = TRUE)
plot_percentage_of_pupils_in_ppf_bands_by_la(all_schools, 'secondary', order = 'q4', fsm_only = TRUE, save_to_file = TRUE)
