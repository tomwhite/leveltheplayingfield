# A few helper functions for finding data discrepancies

# We use the sheets as the definitive list of schools, so we want to be sure new ones are
# added there.

# schools from merged sheets
sheets <- load_merged_google_sheets()

# schools from stats wales
school_list_from_delegated <- load_stats_wales_school_csv("data/delegatedschoolbudgetsperpupil-by-school-num-pupils.csv") %>%
  filter(!grepl('Unallocated resources', school)) %>%
  drop_na(stats_wales_code) %>%
  mutate(lea_code = to_lea_code(stats_wales_code)) %>%
  mutate_at('lea_code', as_numeric_ignore_commas) %>%
  drop_na(`2020-21`) %>%
  select(c(lea_code, stats_wales_code, school))

school_list_from_outturn <- load_stats_wales_school_csv("data/levelofreservescarriedforward-by-school.csv") %>%
  filter(!grepl('Unallocated resources', school)) %>%
  drop_na(stats_wales_code) %>%
  mutate(lea_code = to_lea_code(stats_wales_code)) %>%
  mutate_at('lea_code', as_numeric_ignore_commas) %>%
  drop_na(`2019-20`) %>%
  select(c(lea_code, stats_wales_code, school))

school_list_from_stats_wales = union(school_list_from_delegated, school_list_from_outturn)

# Add these missing schools to sheets
missing_schools <- school_list_from_stats_wales %>%
  anti_join(sheets, by = c("lea_code" = "LEA Code")) %>%
  arrange(lea_code)
missing_schools %>% write_csv("missing_schools.csv")

# schools from address list
school_list_from_address_list_20190318 <- read_csv("data/geo/address-list-schools-wales-maintained-20190318.csv") %>%
  select(c(`School Number`, `School Name`, Postcode)) %>%
  drop_na(`School Number`)
school_list_from_address_list_20210204 <- read_csv("data/geo/address-list-schools-wales-maintained-20210204.csv") %>%
  select(c(`School Number`, `School Name`, Postcode)) %>%
  drop_na(`School Number`)
new_schools_from_address_list <- school_list_from_address_list_20210204 %>%
  anti_join(school_list_from_address_list_20190318, by = c("School Number"))
closed_schools_from_address_list <- school_list_from_address_list_20190318 %>%
  anti_join(school_list_from_address_list_20210204, by = c("School Number"))

school_list_from_address_list = union(school_list_from_address_list_20190318, school_list_from_address_list_20210204)

# See if you can find these addresses independently?
schools_missing_from_address_list <- sheets %>%
  anti_join(school_list_from_address_list, by = c("LEA Code" = "School Number"))

# Locations
school_locations <- load_school_locations()
missing_locations <- sheets %>%
  anti_join(school_locations, by = c("LEA Code" = "lea_code")) %>%
  select(c(`Local authority`, `LEA Code`, `Name of school`))

missing_locations2 <- school_list_from_address_list %>%
  anti_join(school_locations, by = c("School Number" = "lea_code"))
missing_locations2 %>% 
  select(c("Postcode", "School Number")) %>%
  write_csv("missing_locations.csv")

