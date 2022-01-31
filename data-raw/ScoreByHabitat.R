library(tidyverse)
library(readxl)
library(janitor)


HabitatScores <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                            sheet = "middelscorer og gnsn artsantal") %>%
  clean_names() %>%
  drop_na %>% dplyr::mutate(x2 = str_remove_all(x2, "\\*"),
                            x2 = str_remove_all(x2, "\\)"),
                            x2 = str_remove_all(x2, "\\(")) %>%
  rename(habitat_name = x2) %>%
  mutate(Code = case_when(habitatnaturtype %in% c("1330", "1340") ~ "strandeng_13",
                          habitatnaturtype %in% c("2130", "2140", "2180", "2190", "2250", "2310",
                                                  "2320", "2330") ~ "klitter_21",
                          habitatnaturtype %in% c("4010", "4030") ~ "hede_40",
                          habitatnaturtype %in% c("6120", "6210", "6230") ~ "overdrev_62",
                          habitatnaturtype %in% c("6410") ~ "ferskeng_64",
                          habitatnaturtype %in% c("7110", "7120", "7140", "7150") ~ "hoj_mose_71",
                          habitatnaturtype %in% c("7210", "7220", "7230") ~ "lav_mose_72",
                          habitatnaturtype %in% c("9110", "9120", "9130", "9150", "9160", "9170", "9190", "91D0",
                                                  "91E0") ~ "skov_91")) %>%
  relocate(Code, .after = habitatnaturtype)


ScoreByHabitat <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                             sheet = "bilag 1 artsscorer") %>%
  clean_names() %>%
  dplyr::mutate(dansk_navn = str_remove_all(dansk_navn, "\\r\\n"),
                videnskabeligt_navn = str_remove_all(videnskabeligt_navn, "\\r\\n")) %>%
  pivot_longer(stenstrand_12:skov_91, names_to = "Habitat", values_to = "Artscore") %>%
  dplyr::select(videnskabeligt_navn, dansk_navn, Artscore, bilagsart_1_ikke_bilagsart_0, Habitat) %>%
  rename(Scientific_name = videnskabeligt_navn, Danish_name = dansk_navn, Bilagsart = bilagsart_1_ikke_bilagsart_0, Code = Habitat) %>%
  full_join(HabitatScores)


usethis::use_data(ScoreByHabitat, overwrite = TRUE, internal = T)
