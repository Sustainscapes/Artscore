
Species_List <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                             sheet = "bilag 1 artsscorer") %>%
  clean_names() %>%
  dplyr::mutate(dansk_navn = str_remove_all(dansk_navn, "\\r\\n"),
                videnskabeligt_navn = str_remove_all(videnskabeligt_navn, "\\r\\n")) %>%
  dplyr::select(videnskabeligt_navn, dansk_navn) %>%
  dplyr::distinct() %>%
  rename(Scientific_name = videnskabeligt_navn, Danish_name = dansk_navn) %>%
  dplyr::distinct()

usethis::use_data(Species_List, overwrite = TRUE, internal = F)
