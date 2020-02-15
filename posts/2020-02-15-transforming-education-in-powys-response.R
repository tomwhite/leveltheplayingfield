#
# Reports for the response to the Transforming Education in Powys (Feb 2020)
#

source('load_data.R')
source('maps.R')
source('schools.R')
source('utils.R')

PREFIX = "2020-02-15-transforming-education-in-powys-response"

compute_school_funding_redistribution  <- function(schools_tidy, st, la) {
  yr = LATEST_NUM_PUPILS_YEAR
  
  schools_tidy_filtered <- schools_tidy %>%
    filter(school_type %in% st) %>%
    filter(if (!is.null(la)) local_authority == la else TRUE) %>%
    filter(year == yr)
  
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

plot_school_funding_redistribution <- function(schools_tidy, st, la, save_to_file=FALSE) {
  plot <- compute_school_funding_redistribution(schools_tidy, st, la) %>%
    ggplot(aes(reorder(school, budget_difference), budget_difference)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Actual weighted redistribution (£)") +
    labs(title = paste0("Actual weighted redistribution of funding based on ", la, " funding formula (", yr, ")"),
         subtitle = paste0(la, " ", st, " schools")) +
    theme(axis.title.y=element_blank()) +
    scale_y_continuous(labels = comma) +
    ylim(-1400000, 700000)
  if (save_to_file) {
    ggsave(blog_post_file_name(PREFIX, la, st, "school_funding_redistribution", yr, ".png"))
  }
  plot
}

plot_school_funding_redistribution_pct <- function(schools_tidy, st, la, save_to_file=FALSE) {
  plot <- compute_school_funding_redistribution(schools_tidy, st, la) %>%
    ggplot(aes(reorder(school, budget_difference_pct), budget_difference_pct)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Actual weighted redistribution as percentage of budget") +
    labs(title = paste0("Actual weighted redistribution of funding based on ", la, " funding formula (", yr, ")"),
         subtitle = paste0(la, " ", st, " schools")) +
    theme(axis.title.y=element_blank()) +
    scale_y_continuous(labels = comma) +
    ylim(-40, 30)
  if (save_to_file) {
    ggsave(blog_post_file_name(PREFIX, la, st, "school_funding_redistribution_pct", yr, ".png"))
  }
  plot
}

plot_school_funding_redistribution(all_schools, c('secondary', 'through'), 'Powys')
plot_school_funding_redistribution(all_schools, c('secondary', 'through'), 'Gwynedd')

plot_school_funding_redistribution_pct(all_schools, c('secondary', 'through'), 'Powys')
plot_school_funding_redistribution_pct(all_schools, c('secondary', 'through'), 'Gwynedd')
