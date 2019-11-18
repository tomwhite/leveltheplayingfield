source('schools.R')

# TODO: how many schools have identical postcode and therefor lat/lon? Could cause a problem for maps.

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
