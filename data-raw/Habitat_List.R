library(tidyverse)
library(readxl)
library(janitor)


Habitat_List <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
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
  relocate(Code, .after = habitatnaturtype) %>%
  dplyr::select(habitatnaturtype, habitat_name) %>%
  drop_na() %>%
  rename(Code = habitatnaturtype)

usethis::use_data(Habitat_List, overwrite = TRUE, internal = F)
