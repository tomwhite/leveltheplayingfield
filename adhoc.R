# Ad hoc exploratory analysis

source('load_data.R')
source('schools.R')
source('utils.R')

# TODO: how many schools have identical postcode and therefore lat/lon? Could cause a problem for maps.

# Total school numbers
all_schools %>%
  filter(year == '2018-19') %>%
  group_by(school_type) %>%
  tally()

# Strip chart
secondaries_tidy$surplus_or_deficit <- if_else(secondaries_tidy$budget_outturn >= 0, "Black", "Red")

secondaries_tidy %>%
  filter(year == "2017-18") %>%
  ggplot(aes(x = per_pupil_funding, y = factor(1))) +
  geom_jitter(aes(color = surplus_or_deficit), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("black", "red"))

secondaries_tidy %>%
  filter(year == "2017-18") %>%
  ggplot(aes(x = per_pupil_funding, y = surplus_or_deficit)) +
  geom_jitter(aes(color = surplus_or_deficit), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("black", "red"))

secondaries_tidy %>%
  filter(year == "2017-18") %>%
  ggplot(aes(x = num_pupils, y = surplus_or_deficit)) +
  geom_jitter(aes(color = surplus_or_deficit), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("black", "red"))

secondaries_tidy %>%
  filter(year == "2017-18") %>%
  ggplot(aes(x = budget_outturn / num_pupils, y = per_pupil_funding)) +
  geom_point() +
  geom_smooth(method=lm)

# Capacity distribution

schools_tidy %>%
  filter(local_authority == 'Powys') %>%
  filter(year == '2018-19') %>%
  mutate(occupancy = 100.0 * num_pupils / capacity) %>%
  ggplot(aes(occupancy)) +
  geom_histogram(binwidth = 5, colour="black", fill="white")

schools_tidy %>%
  filter(year == '2018-19') %>%
  filter(!is.na(num_pupils)) %>%
  filter(!is.na(capacity)) %>%
  mutate(occupancy = 100.0 * num_pupils / capacity) %>%
  ggplot(aes(reorder(local_authority, occupancy, FUN = median), occupancy)) +
  geom_boxplot() +
  geom_jitter(width=0.05,alpha=0.2) +
  coord_flip()

# Rural schools

# Only primaries
all_schools %>%
  filter(year == '2018-19') %>%
  filter(rural_school == 'Yes') %>%
  group_by(school_type) %>%
  tally()

# Rural schools are small
all_schools %>%
  filter(year == '2018-19') %>%
  filter(school_type == 'primary') %>%
  ggplot(aes(x = rural_school, y = num_pupils)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, size = 1) +
  coord_flip()

# Rural schools have lower occupancy
all_schools %>%
  filter(year == '2018-19') %>%
  filter(school_type == 'primary') %>%
  mutate(occupancy = 100.0 * num_pupils / capacity) %>%
  ggplot(aes(x = rural_school, y = occupancy)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, size = 1) +
  coord_flip()

# Small schools have lower occupancy
all_schools %>%
  filter(year == '2018-19') %>%
  filter(school_type == 'primary') %>%
  mutate(occupancy = 100.0 * num_pupils / capacity) %>%
  ggplot(aes(x = capacity, y = occupancy)) +
  geom_point() +
  geom_smooth(method=lm)

# Language
all_schools %>%
  filter(year == '2018-19') %>%
  filter(school_type != 'special') %>%
  group_by(language) %>%
  tally()

# FSM

# Calculate the additional funding per pupil (in £) for every additional percentage point of FSM

# Use broom 
library(broom)
schools_tidy %>%
  filter(year == "2018-19") %>%
  nest(-local_authority) %>%
  mutate(
    fit = map(data, ~ lm(per_pupil_funding ~ fsm_rate, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>%
  filter(term == 'fsm_rate') %>%
  ggplot(aes(reorder(local_authority, estimate), estimate)) +
  geom_point() +
  coord_flip()

# Budget outturn trend arrows
plot_school_vs_budget_outturn_change(all_schools, 'primary', 'Gwynedd')

# Per-pupil outturn change pct - primary vs secondary
# Are some LAs improving one at the expense of the other?

x <- all_schools %>%
  filter(!is.na(budget_outturn)) %>%
  filter(!is.na(num_pupils)) %>%
  filter(year <= LATEST_OUTTURN_YEAR) %>%
  group_by(local_authority, year, school_type) %>%
  summarize(mean_per_pupil_outturn=mean(budget_outturn/num_pupils)) %>%
  filter(school_type == 'primary' | school_type == 'secondary') %>%
  filter(year != '2017-18') %>%
  spread(year, mean_per_pupil_outturn) %>% # put years back into columns
  mutate(diff = `2018-19` - `2016-17`) %>%
  select(c(local_authority, school_type, diff)) %>%
  spread(school_type, diff) # put school type back into columns

x %>%
  ggplot(aes(x = primary, y = secondary), group = local_authority) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_text(aes(label=local_authority), hjust = 0, nudge_x = 5) +
  xlab("Change in primary per-pupil budget outturn 2016-17 to 2018-19 (£)") + 
  ylab("Change in secondary per-pupil budget outturn 2016-17 to 2018-19 (£)")


#
# Distribution of per-pupil funding
#

all_schools_latest <- all_schools %>% filter(year == LATEST_NUM_PUPILS_YEAR)

# Which (non-special) schools have very high per-pupil funding?
all_schools_latest %>%
  filter(school_type != 'special') %>%
  filter(per_pupil_funding > 8000) %>%
  select(c(local_authority, school, school_type, num_pupils, per_pupil_funding)) %>%
  arrange(desc(per_pupil_funding))

# Which have very low per-pupil funding?
all_schools_latest %>%
  filter(per_pupil_funding < 3000) %>%
  select(c(local_authority, school, school_type, num_pupils, per_pupil_funding)) %>%
  arrange(per_pupil_funding)

# Plot all schools (except special)
all_schools_latest %>%
  filter(school_type != 'special') %>%
  ggplot(aes(per_pupil_funding, fill=school_type)) +
  geom_histogram(binwidth = 200, position="dodge")

library(ggridges)
all_schools_latest %>%
  ggplot(aes(per_pupil_funding, y=school_type, fill=school_type)) +
  geom_density_ridges() +
  theme_ridges()

all_schools_latest %>%
  filter(school_type != 'special') %>%
  filter(per_pupil_funding < 10000) %>%
  ggplot(aes(per_pupil_funding, y=school_type, fill=school_type)) +
  geom_density_ridges() +
  theme_ridges()

# Plot primary
all_schools_latest %>%
  filter(school_type == 'primary') %>%
  ggplot(aes(per_pupil_funding)) +
  geom_histogram(binwidth = 100, colour="black", fill="white")

quantile((all_schools_latest %>% filter(school_type == 'primary'))$per_pupil_funding, na.rm = TRUE)

# Plot secondary
all_schools_latest %>%
  filter(school_type == 'secondary') %>%
  ggplot(aes(per_pupil_funding)) +
  geom_histogram(binwidth = 50, colour="black", fill="white")

quantile((all_schools_latest %>% filter(school_type == 'secondary'))$per_pupil_funding)

#
# Population
#

# Plot of population for each LA by year
filter_to_wales_local_authorities(population) %>%
  gather(year, population, -c(local_authority)) %>%
  ggplot(aes(x=year, y=population, group=local_authority)) +
  geom_line()

population_with_age_wales <- population_with_age %>%
  group_by(year, age) %>%
  summarise(population = sum(population))

population_with_age_wales %>%
  ggplot(aes(y = population, x = year, group= age, color = age)) +
  geom_line() +
  theme(axis.text.x=element_text(angle = 90),
        axis.title.x=element_blank())

# See http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html

# Absolute population numbers
population_with_age %>%
  filter(local_authority == 'Powys') %>%
  ggplot(aes(y = population, x = year, fill = reorder(age, desc(age)))) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle = 90),
        axis.title.x=element_blank())

# Percentage bands for each age group
population_with_age %>%
  filter(local_authority == 'Powys') %>%
  ggplot(aes(y = population, x = year, fill = reorder(age, desc(age)))) +
  geom_bar(position = "fill", stat = 'identity') +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x=element_text(angle = 90),
        axis.title.x=element_blank())

#
# Comparing PPF for similar primaries across Wales 
#

yr = LATEST_FSM_YEAR
st = 'primary'
schools_subset <- all_schools %>%
  filter(!is.na(local_authority)) %>%
  filter(if (!is.null(st)) school_type == st else TRUE) %>%
  filter(year == yr) %>%
  filter(!is.na(num_pupils)) %>%
  filter(!is.na(fsm_rate))

# num_pupils distribution
schools_subset %>%
  ggplot(aes(num_pupils)) +
  geom_histogram(binwidth=30, colour="black", fill="white")

# fsm distribution
schools_subset %>%
  ggplot(aes(fsm_rate)) +
  geom_histogram(binwidth=5, colour="black", fill="white")

# 2d dist
schools_subset %>%
  ggplot(aes(num_pupils, fsm_rate)) +
  geom_point()

# Summary table for num pupils and fsm bins with min and mix PPF
x <- schools_subset %>%
  mutate(num_pupils_bin = cut_width(num_pupils, width = 30, center = 15, closed="left")) %>%
  mutate(fsm_rate_bin = cut_width(fsm_rate, width = 10, center = 5, closed="left")) %>%
  group_by(num_pupils_bin, fsm_rate_bin) %>%
  filter(n() >= 5) %>%
  summarize(n = n(), min_ppf = min(per_pupil_funding), max_ppf = max(per_pupil_funding), delta_ppf = max(per_pupil_funding) - min(per_pupil_funding)) %>%
  arrange(desc(delta_ppf))

schools_subset_one_bin <- schools_subset %>%
  mutate(num_pupils_bin = cut_width(num_pupils, width = 30, center = 15, closed="left")) %>%
  mutate(fsm_rate_bin = cut_width(fsm_rate, width = 10, center = 5, closed="left")) %>%
  filter(num_pupils_bin == num_pupils_band) %>%
  filter(fsm_rate_bin == fsm_rate_band)

y <- schools_subset_one_bin %>% 
  select(-c(rural_school, year, size, support_category_days, latitude, longitude, school_type)) %>%
  arrange(desc(per_pupil_funding))

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

map_per_pupil_funding_peers(all_schools, 'primary', '[120,150)', '[30,40)', save_to_file = TRUE)
map_per_pupil_funding_peers(all_schools, 'primary', '[150,180)', '[30,40)')
map_per_pupil_funding_peers(all_schools, 'primary', '[30,60)', '[0,10)')

by(x, 1:nrow(x), function(row) map_per_pupil_funding_peers(all_schools, 'primary', row$num_pupils_bin, row$fsm_rate_bin, save_to_file = TRUE))

###


