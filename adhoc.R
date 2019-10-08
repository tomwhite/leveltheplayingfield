source('schools.R')

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
# see https://stackoverflow.com/questions/38104901/ggplot2-show-difference-in-values-over-time-with-an-arrow

x <- all_schools %>%
  filter(school_type == 'secondary') %>%
  filter(local_authority == 'Powys') %>%
  select(c(school, year, budget_outturn)) %>%
  filter(year == "2016-17" | year == "2017-18") %>%
  spread(year, budget_outturn) %>% # put years back into columns
  mutate(direction = ifelse(`2017-18` - `2016-17` > 0, "Up", "Down")) %>%
  melt(id = c("school", "direction"))
  
ggplot(x, aes(x=value, y = school, color = variable, group = school )) + 
  geom_point(size=4) + 
  geom_path(aes(color = direction), arrow=arrow())

ggplot2df <- read.table(text = "question y2015 y2016
q1 90 50
                        q2 80 60
                        q3 70 90
                        q4 90 60
                        q5 30 20", header = TRUE)

library(reshape2)
df <- ggplot2df %>% 
  mutate(direction = ifelse(y2016 - y2015 > 0, "Up", "Down"))%>%
  melt(id = c("question", "direction"))
