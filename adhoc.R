
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
  ggplot(aes(x = num_pupils, y = occupancy)) +
  geom_point() +
  geom_smooth(method=lm)
  