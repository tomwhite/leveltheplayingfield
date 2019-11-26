library(tidyverse)
library(broom)
library(googlesheets)
library(knitr)
library(scales)
library(DT)
library(htmlTable)
library(reshape2)

source('utils.R')

SCHOOL_SIZE_COLOURS = c("<50" = "#F8766D", "50-100" = "#A3A500", "100-200" = "#00BF7D", "200-400" = "#00B0F6", ">400" = "#E76BF3")

plot_summary_size_distribution <- function(schools_tidy, school_type, save_to_file=FALSE) {
  yr <- LATEST_YEAR
  st <- school_type
  plot = schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == yr) %>%
    ggplot(aes(num_pupils)) +
    geom_histogram(binwidth=25, colour="black", fill="white") +
    facet_wrap(~ local_authority, ncol=4)
  if (save_to_file) {
    ggsave(report_file_name(NULL, school_type, "size_distribution", yr, ".png"))
  }
  plot
}

plot_pupil_funding_vs_year <- function(schools_tidy, la, save_to_file=FALSE) {
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter(str_detect(year, '-')) %>% # only school years
    ggplot(aes(x=year, y=per_pupil_funding, group=school, color=size)) +
    geom_line() +
    geom_point() +
    ylab("Per-pupil funding (£)") + 
    theme(axis.title.x=element_blank()) +
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_year", NULL, ".png"))
  }
  plot
}

plot_school_funding_vs_size <- function(schools_tidy, la, save_to_file=FALSE) {
  yr <- LATEST_NUM_PUPILS_YEAR
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter(year == yr) %>%
    ggplot(aes(x=num_pupils, y=total_school_delegated_budget)) +
    geom_point() +
    xlab("Number of pupils") +
    ylab("Total school delegated funding (£)")
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "school_funding_vs_size", yr, ".png"))
  }
  plot
}

plot_pupil_funding_vs_outturn <- function(schools_tidy, la, save_to_file=FALSE) {
  yr = LATEST_OUTTURN_YEAR
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter (!is.na(budget_outturn)) %>%
    filter(year == yr) %>%
    ggplot(aes(x=budget_outturn, y=per_pupil_funding)) +
    geom_point(aes(color=size)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    facet_wrap(~ year, ncol=1, drop=TRUE) +
    xlab("Budget outturn (£)") +
    ylab("Per-pupil funding (£)") +
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_outturn", yr, ".png"))
  }
  plot
}

plot_pupil_funding_vs_per_pupil_outturn <- function(schools_tidy, la, save_to_file=FALSE) {
  yr = LATEST_OUTTURN_YEAR
  x <- schools_tidy %>%
    filter(local_authority == la) %>%
    filter (!is.na(budget_outturn)) %>%
    filter(year == yr)
  coef <- cor(x$budget_outturn/x$num_pupils, x$per_pupil_funding, method = "pearson", use = "complete.obs")
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter (!is.na(budget_outturn)) %>%
    filter(year == yr) %>%
    ggplot(aes(x=budget_outturn/num_pupils, y=per_pupil_funding)) +
    geom_point() +
    geom_smooth(method=lm) +
    xlab("Per-pupil budget outturn (£)") +
    ylab("Per-pupil funding (£)") +
    labs(title = "Relationship between per-pupil funding and budget outturn",
         subtitle = paste0(la, ", ", yr, ", correlation ", round(coef, 2)))
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_pupil_outturn", yr, ".png"))
  }
  plot
}

plot_per_pupil_outturn_vs_year <- function(schools_tidy, st, la, save_to_file=FALSE) {
  # All Wales is black, LA is blue
  all_wales_per_pupil_outturn <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(!is.na(budget_outturn)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(year <= LATEST_OUTTURN_YEAR) %>%
    group_by(year) %>%
    summarize(mean_per_pupil_outturn=mean(budget_outturn/num_pupils)) %>%
    mutate(local_authority = 'All')
  
  per_la_per_pupil_outturn <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(!is.na(budget_outturn)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(year <= LATEST_OUTTURN_YEAR) %>%
    group_by(local_authority, year) %>%
    summarize(mean_per_pupil_outturn=mean(budget_outturn/num_pupils))
  
  plot = per_la_per_pupil_outturn %>%
    ggplot(aes(x=year, y=mean_per_pupil_outturn, group=local_authority)) +
    geom_hline(yintercept = 0, color='red') +
    geom_line(alpha = 0.2) +
    geom_line(data = all_wales_per_pupil_outturn, color = 'black') +
    geom_line(data = filter(per_la_per_pupil_outturn, local_authority == la), color='blue') +
    ylab("Average per-pupil budget outturn (£)") + 
    labs(title = "Average per-pupil budget outturn by year",
         subtitle = paste0(la, " (blue) vs. Wales (black), ", st, " schools")) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, st, "pupil_outturn_vs_year", NULL, ".png"))
  }
  plot
}

plot_pupil_funding_vs_fsm <- function(schools_tidy, la, save_to_file=FALSE) {
  yr = LATEST_FSM_YEAR
  x <- schools_tidy %>%
    filter(local_authority == la) %>%
    filter(year == yr)
  coef <- cor(x$fsm_rate, x$per_pupil_funding, method = "pearson", use = "complete.obs")
  plot = schools_tidy %>%
    filter(local_authority == la) %>%
    filter(year == yr) %>%
    ggplot(aes(x=fsm_rate, y=per_pupil_funding)) +
    geom_point(aes(color=size, size=num_pupils_on_fsm)) +
    geom_smooth(method=lm) +
    xlab("Percentage of pupils on free school meals") +
    ylab("Per-pupil funding (£)") +
    labs(color="Size of school",
         size="Number of pupils on FSM",
         title = "Relationship between per-pupil funding and free school meals",
         subtitle = paste0(la, ", ", yr, ", correlation ", round(coef, 2)))
    scale_colour_manual(values = SCHOOL_SIZE_COLOURS)
  if (save_to_file) {
    ggsave(report_file_name(la, "primary", "pupil_funding_vs_fsm", yr, ".png"))
  }
  plot
}

plot_support_catagory_vs_year <- function(schools_tidy, st, la, save_to_file=FALSE) {
  # All Wales is black, LA is blue
  all_wales_support_category <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(!is.na(support_category)) %>%
    group_by(year) %>%
    summarize(mean_support_category_days=mean(support_category_days)) %>%
    mutate(local_authority = 'All')
  
  per_la_support_category <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(!is.na(support_category)) %>%
    group_by(local_authority, year) %>%
    summarize(mean_support_category_days=mean(support_category_days))
  
  plot = per_la_support_category %>%
    ggplot(aes(x=year, y=mean_support_category_days, group=local_authority)) +
    geom_hline(yintercept = 4, color='green') +
    geom_hline(yintercept = 10, color='yellow') +
    geom_hline(yintercept = 15, color='orange') +
    geom_line(alpha = 0.2) +
    geom_line(data = all_wales_support_category, color = 'black') +
    geom_line(data = filter(per_la_support_category, local_authority == la), color='blue') +
    ylab("Average support category days") + 
    scale_y_continuous(breaks = seq(4, 25)) +
    labs(title = "Average support category days by year",
         subtitle = paste0(la, " (blue) vs. Wales (black), ", st, " schools")) +
    theme(axis.title.x=element_blank())
  if (save_to_file) {
    ggsave(report_file_name(la, st, "support_category_vs_year", NULL, ".png"))
  }
  plot
}

plot_school_vs_budget_outturn_change <- function(schools_tidy, st, la, save_to_file=FALSE) {
  # Budget outturn trend arrows
  # see https://stackoverflow.com/questions/38104901/ggplot2-show-difference-in-values-over-time-with-an-arrow
  
  x <- schools_tidy %>%
    filter(school_type == st) %>%
    filter(local_authority == la) %>%
    mutate(per_pupil_budget_outturn = budget_outturn / num_pupils) %>%
    select(c(school, year, per_pupil_budget_outturn)) %>%
    filter(year == "2016-17" | year == "2017-18") %>%
    spread(year, per_pupil_budget_outturn) %>% # put years back into columns
    filter(!is.na(`2016-17`)) %>%
    filter(!is.na(`2017-18`)) %>%
    mutate(diff = `2017-18` - `2016-17`) %>%
    mutate(direction = ifelse(diff > 0, "Increase", "Decrease")) %>%
    melt(id = c("school", "direction", "diff"))
  plot <- ggplot(x, aes(x = value, y = reorder(school, diff), group = school)) + 
    geom_path(aes(color = direction), arrow = arrow(angle = 15, length = unit(0.15, "inches"), type = "open")) +
    xlim(-1000, 2000) +
    xlab("Per-pupil budget outturn (£)") +
    ylab("School") +
    labs(color = "Change in per-pupil budget outturn",
         title = "Change in per-pupil budget outturn (2016-17 to 2017-18) for each school",
         subtitle = paste0(la, ", ", st, " schools")) +
    theme(axis.text.y=element_blank(), axis.ticks=element_blank()) +
    scale_colour_manual(values=c("Decrease" = "red", "Increase" = "green"))
  if (save_to_file) {
    ggsave(report_file_name(la, st, "school_vs_budget_outturn_change", "2017-18", ".png"))
  }
  plot
}

tabulate_num_pupils_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  # summary of min, max, mean, median number of pupils per LA per year
  table <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    group_by(local_authority, year) %>%
    summarize(schools=n(), total_pupils=sum(num_pupils), smallest=min(num_pupils), largest=max(num_pupils), mean=round(mean(num_pupils), 0), median=round(median(num_pupils), 0)) %>%
    rename("Local authority" = local_authority, "Year" = year, "Schools" = schools, "Total pupils" = total_pupils, "Smallest" = smallest, "Largest" = largest, "Mean" = mean, "Median" = median)
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidget(dt, report_file_name(NULL, school_type, "num_pupils_summary", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}

tabulate_general_summary <- function(schools_tidy, school_type, save_to_file=FALSE) {
  # summary of main indicators (latest year available)
  
  st <- school_type
  summary_size <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == LATEST_YEAR) %>%
    group_by(local_authority) %>%
    summarize(schools=n(), total_pupils=sum(num_pupils), mean=round(mean(num_pupils), 0)) %>%
    mutate(mean_rank = rank(desc(mean))) %>%
    rename("Local authority" = local_authority, "Schools (2018-19)" = schools, "Total pupils (2018-19)" = total_pupils, "Mean pupils (2018-19)" = mean, "Mean pupils rank (2018-19)" = mean_rank)
  
  summary_support_category <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == LATEST_SUPPORT_CATEGORY_YEAR) %>%
    filter(!is.na(support_category_days)) %>% # ignore missing (e.g. for new schools)
    group_by(local_authority) %>%
    summarize(mean=round(mean(support_category_days), 2)) %>%
    mutate(mean_rank = min_rank(mean)) %>%
    rename("Local authority" = local_authority, "Mean support category days (2018)" = mean, "Mean support category days rank (2018)" = mean_rank)
  
  summary_per_pupil_outturn <- schools_tidy %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(if (!is.null(st)) school_type == st else TRUE) %>%
    filter(year == LATEST_OUTTURN_YEAR) %>%
    group_by(local_authority) %>%
    summarize(mean=round(mean(budget_outturn / num_pupils), 0)) %>%
    mutate(mean_rank = rank(desc(mean))) %>%
    rename("Local authority" = local_authority, "Mean per-pupil budget outturn (2018-19)" = mean, "Mean per-pupil budget outturn rank (2018-19)" = mean_rank)

  table <- summary_size %>%
    left_join(summary_support_category) %>%
    left_join(summary_per_pupil_outturn)
  
  # TODO: handle NAs in fit?
  # if (is.null(st) || (st != 'through' && st != 'special')) {
  #   summary_per_pupil_fsm <- schools_tidy %>%
  #     filter(!is.na(local_authority)) %>%
  #     filter(!is.na(num_pupils)) %>%
  #     filter(if (!is.null(st)) school_type == st else TRUE) %>%
  #     filter(year == LATEST_FSM_YEAR) %>%
  #     nest(-local_authority) %>%
  #     mutate(
  #       fit = map(data, ~ lm(per_pupil_funding ~ fsm_rate, data = .x)),
  #       tidied = map(fit, tidy)
  #     ) %>% 
  #     unnest(tidied) %>%
  #     filter(term == 'fsm_rate') %>%
  #     select(c('local_authority', 'estimate')) %>%
  #     mutate(estimate_rank = rank(desc(estimate))) %>%
  #     mutate(estimate = round(estimate, 1)) %>%
  #     rename("Local authority" = local_authority, "Per-pupil funding increase per FSM % increase (2018-19)" = estimate, "Per-pupil funding increase per FSM % increase rank (2018-19)" = estimate_rank)
  #   
  #   table <- table %>%
  #     left_join(summary_per_pupil_fsm)
  # }
  
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidget(dt, report_file_name(NULL, school_type, "general_summary", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}

tabulate_occupancy_summary <- function(schools_tidy, save_to_file=FALSE) {
  table <- schools_tidy %>%
    filter(year == LATEST_NUM_PUPILS_YEAR) %>%
    filter(!is.na(local_authority)) %>%
    filter(!is.na(num_pupils)) %>%
    filter(!is.na(capacity)) %>%
    group_by(local_authority, school_type) %>%
    summarize(total_capacity = round(sum(capacity), 0), total_pupils = sum(num_pupils)) %>%
    mutate(surplus_places = total_capacity - total_pupils) %>%
    mutate(occupancy_percent = round(100 * total_pupils / total_capacity, 1)) %>% 
    rename("Local authority" = local_authority, "School type" = school_type, "Total capacity" = total_capacity, "Total pupils" = total_pupils, "Surplus places" = surplus_places, "Occupancy percent" = occupancy_percent)
  dt <- datatable(table, rownames= FALSE, options = list(
    pageLength = 100,
    order = list(list(0, 'asc'))
  ))
  if (save_to_file) {
    saveWidget(dt, report_file_name(NULL, NULL, "occupancy_summary", NULL, ".html"), selfcontained = FALSE, libdir = "lib")
  }
  dt
}
