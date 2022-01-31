library(tidyverse)
library(readxl)
library(janitor)


Sheets <- readxl::excel_sheets("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx")

Sheets <- Sheets[1:7]

### Sheet1

StruktureIndeksScores <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                    sheet = Sheets[1], col_types = c("text",
                                                                     "skip", "text", "text",
                                                                     "skip", "skip", "skip", "skip")) %>%
  clean_names()

Variables <- StruktureIndeksScores %>%
  dplyr::filter(is.na(x2)) %>%
  pull(score) %>%
  str_remove_all("\\r\\n")

Variables = c(rep(Variables[1:12], each = 5),rep(Variables[13:14], each = 3))

Habitat1 <- StruktureIndeksScores$x2[1] %>% readr::parse_number()

Habitat2 <- StruktureIndeksScores$x3[1] %>% readr::parse_number()

Scores <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x2)) %>%
  pull(score) %>%
  str_remove_all("\\r\\n")

Scores <- Scores[-1]

Values1 <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x2)) %>%
  pull(x2)

Values1 <- Values1[-1] %>% as.numeric()

Values2 <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x3)) %>%
  pull(x3)

Values2 <- Values2[-1] %>% as.numeric()


Final_Sheet1 <- data.frame(Variables = c(Variables, Variables), Values = c(Values1, Values2),
                    Scores = c(Scores,Scores),
                    Habitat = c(rep(Habitat1, times = length(Values1)),rep(Habitat2, times = length(Values2)))) %>%
  rename(Subvariables = Variables) %>%
  mutate(Habitat = as.character(Habitat))

Subweights_Sheet1 <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                             col_types = c("skip", "skip", "skip",
                                           "skip", "skip", "text", "numeric",
                                           "numeric"), skip = 1) %>%
  drop_na() %>% pivot_longer(-`Habitattype\r\n`, names_to = "Habitat", values_to = "Weight") %>%
  rename(Variables = `Habitattype\r\n`) %>%
  mutate(Variables = str_remove_all(Variables, "\\r\\n")) %>%
  mutate(Subvariables = case_when(Variables == "Bar jord" ~ "Uden vegetationsdække",
                               Variables == "lave urter" ~ "Græs/urteveg. under 15 cm",
                               Variables == "middel urter" ~ "Græs/urtevegetation 15-50 cm",
                               Variables == "høje urter" ~ "Græs/urtevegetation over 50 cm",
                               Variables == "dværgbuske" ~ "Dværgbuske",
                               Variables == "vedplanter" ~  "Vedplanter (kronedække)",
                               Variables == "invasive planter" ~ "Forekomst af invasive arter",
                               Variables == "afvanding" ~ "Afvanding og vandindvinding",
                               Variables == "vandløb" ~ "Vandløb",
                               Variables == "kystsikring" ~ "Kystsikring",
                               Variables == "afgræsning" ~ "Græsning/høslæt",
                               Variables == "eutrofiering" ~ "Gødskning el. sprøjteskader",
                               Variables == "positive strukturer" ~  "Positive strukturer",
                               Variables == "negative strukturer" ~ "Negative strukturer"),
         Variables = case_when(Variables == "Vegetationsstruktur" ~ "Vegetationsstruktur",
                               Variables == "Bar jord" ~ "Vegetationsstruktur",
                                  Variables == "lave urter" ~ "Vegetationsstruktur",
                                  Variables == "middel urter" ~ "Vegetationsstruktur",
                                  Variables == "høje urter" ~ "Vegetationsstruktur",
                                  Variables == "dværgbuske" ~ "Vegetationsstruktur",
                                  Variables == "vedplanter" ~  "Vegetationsstruktur",
                                  Variables == "invasive planter" ~ "Vegetationsstruktur",
                                  Variables == "Hydrologi" ~ "Hydrologi",
                                  Variables == "afvanding" ~ "Hydrologi",
                                  Variables == "vandløb" ~ "Hydrologi",
                                  Variables == "kystsikring" ~ "Hydrologi",
                                  Variables == "Landbrugspåvirkninger" ~ "Landbrugspåvirkninger",
                                  Variables == "afgræsning" ~ "Landbrugspåvirkninger",
                                  Variables == "eutrofiering" ~ "Landbrugspåvirkninger",
                                  Variables == "Naturtypekarak. strukturer" ~  "Naturtypekarak. strukturer",
                                  Variables == "positive strukturer" ~  "Naturtypekarak. strukturer",
                                  Variables == "negative strukturer" ~ "Naturtypekarak. strukturer"))


Weights_Sheet1 <- Subweights_Sheet1 %>%
  dplyr::filter(is.na(Subvariables)) %>%
  dplyr::select(-Subvariables)

Subweights_Sheet1 <-  Subweights_Sheet1 %>%
  dplyr::filter(!is.na(Subvariables)) %>%
  rename(Subweights = Weight) %>%
  full_join(Weights_Sheet1) %>%
  relocate(Subweights, .after = Weight) %>%
  relocate(Habitat, .before = everything()) %>%
  mutate(Habitat = as.character(Habitat)) %>%
  full_join(Final_Sheet1)

Final_Sheet1 <- Subweights_Sheet1

### Sheet2

StruktureIndeksScores <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                    sheet = Sheets[2]) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  clean_names()

Variables <- StruktureIndeksScores %>%
  dplyr::filter(is.na(x3)) %>%
  pull(score) %>%
  str_remove_all("\\r\\n")

Variables = c(rep(Variables[1:12], each = 5),rep(Variables[13:14], each = 3))

Habitat1 <- StruktureIndeksScores$x3[1] %>% readr::parse_number()

Habitat2 <- StruktureIndeksScores$x4[1] %>% readr::parse_number()

Habitat3 <- StruktureIndeksScores$x5[1] %>% readr::parse_number()

Habitat4 <- StruktureIndeksScores$x6[1] %>% readr::parse_number()

Scores <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x3)) %>%
  pull(score) %>%
  str_remove_all("\\r\\n")

Scores <- Scores[-1]

Values <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x3)) %>%
  dplyr::select(x3:x6)

colnames(Values) <- Values[1,]

Values <- Values[-1,] %>% mutate_all(as.numeric) %>%
  pivot_longer(everything(), names_to = "Habitat", values_to = "Values") %>%
  mutate(Habitat = parse_number(Habitat)) %>%
  arrange(Habitat)

Final_Sheet2 <- data.frame(Variables = rep(Variables, 4),
                           Values = Values$Values,
                           Scores = rep(Scores, 4),
                           Habitat = Values$Habitat) %>%
  rename(Subvariables = Variables) %>%
  mutate(Habitat = as.character(Habitat))


Subweights_Sheet2 <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                skip = 1, sheet = Sheets[2]) %>%
  dplyr::select("Habitattype\r\n...8":"2250") %>%
  drop_na() %>% pivot_longer(-`Habitattype\r\n...8`, names_to = "Habitat", values_to = "Weight") %>%
  rename(Variables = `Habitattype\r\n...8`) %>%
  mutate(Variables = str_remove_all(Variables, "\\r\\n")) %>%
  mutate(Subvariables = case_when(Variables == "bar jord" ~ "Uden vegetationsdække",
                                  Variables == "lave urter" ~ "Græs/urteveg. under 15 cm",
                                  Variables == "middel urter" ~ "Græs/urtevegetation 15-50 cm",
                                  Variables == "høje urter" ~ "Græs/urtevegetation over 50 cm",
                                  Variables == "dværgbuske" ~ "Dværgbuske",
                                  Variables == "vedplanter" ~  "Vedplanter (kronedække)",
                                  Variables == "invasive planter" ~ "Forekomst af invasive arter",
                                  Variables == "afvanding" ~ "Afvanding og vandindvinding",
                                  Variables == "vandløb" ~ "Vandløb",
                                  Variables == "kystsikring" ~ "Kystsikring",
                                  Variables == "afgræsning" ~ "Græsning/høslæt",
                                  Variables == "gødskning" ~ "Gødskning el. sprøjteskader",
                                  Variables == "positive strukturer" ~  "Positive strukturer",
                                  Variables == "negative strukturer" ~ "Negative strukturer"),
         Variables = case_when(Variables == "Vegetationsstruktur" ~ "Vegetationsstruktur",
                               Variables == "bar jord" ~ "Vegetationsstruktur",
                               Variables == "lave urter" ~ "Vegetationsstruktur",
                               Variables == "middel urter" ~ "Vegetationsstruktur",
                               Variables == "høje urter" ~ "Vegetationsstruktur",
                               Variables == "dværgbuske" ~ "Vegetationsstruktur",
                               Variables == "vedplanter" ~  "Vegetationsstruktur",
                               Variables == "invasive planter" ~ "Vegetationsstruktur",
                               Variables == "hydrologi" ~ "Hydrologi",
                               Variables == "afvanding" ~ "Hydrologi",
                               Variables == "vandløb" ~ "Hydrologi",
                               Variables == "kystsikring" ~ "Hydrologi",
                               Variables == "Landbrugspåvirkninger" ~ "Landbrugspåvirkninger",
                               Variables == "afgræsning" ~ "Landbrugspåvirkninger",
                               Variables == "gødskning" ~ "Landbrugspåvirkninger",
                               Variables == "Naturtypekarak. strukturer" ~  "Naturtypekarak. strukturer",
                               Variables == "positive strukturer" ~  "Naturtypekarak. strukturer",
                               Variables == "negative strukturer" ~ "Naturtypekarak. strukturer"))


Weights_Sheet2 <- Subweights_Sheet2 %>%
  dplyr::filter(is.na(Subvariables)) %>%
  dplyr::select(-Subvariables)

Subweights_Sheet2 <-  Subweights_Sheet2 %>%
  dplyr::filter(!is.na(Subvariables)) %>%
  rename(Subweights = Weight) %>%
  full_join(Weights_Sheet2) %>%
  relocate(Subweights, .after = Weight) %>%
  relocate(Habitat, .before = everything()) %>%
  mutate(Habitat = as.character(Habitat)) %>%
  full_join(Final_Sheet2)

Final_Sheet2 <- Subweights_Sheet2

rm(Habitat1, Habitat2, Habitat3, Habitat4, Scores, StruktureIndeksScores, Subweights_Sheet1, Subweights_Sheet2, Values, Values1, Values2, Variables, Weights_Sheet1, Weights_Sheet2)

## Sheet3

StruktureIndeksScores <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                    sheet = Sheets[3]) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  clean_names()

Variables <- StruktureIndeksScores %>%
  dplyr::filter(is.na(x3)) %>%
  pull(score) %>%
  str_remove_all("\\r\\n")

Variables = c(rep(Variables[1:12], each = 5),rep(Variables[13:14], each = 3))


Scores <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x3)) %>%
  pull(score) %>%
  str_remove_all("\\r\\n")

Scores <- Scores[-1]

Values <- StruktureIndeksScores %>%
  dplyr::filter(!is.na(x3)) %>%
  dplyr::select(x3:x8)

colnames(Values) <- Values[1,]

Values <- Values[-1,] %>% mutate_all(as.numeric) %>%
  pivot_longer(everything(), names_to = "Habitat", values_to = "Values") %>%
  mutate(Habitat = parse_number(Habitat)) %>%
  arrange(Habitat)

N_Habitats <- length(unique(Values$Habitat))

Final_Sheet3 <- data.frame(Variables = rep(Variables, N_Habitats),
                           Values = Values$Values,
                           Scores = rep(Scores, N_Habitats),
                           Habitat = Values$Habitat) %>%
  rename(Subvariables = Variables) %>%
  mutate(Habitat = as.character(Habitat))


Subweights_Sheet3 <- read_excel("Habitattypernes scoringer og vægtninger_lysåben og skov.xlsx",
                                skip = 1, sheet = Sheets[3]) %>%
  dplyr::select("Habitattype\r\n...10":"5130") %>%
  drop_na() %>% pivot_longer(-`Habitattype\r\n...10`, names_to = "Habitat", values_to = "Weight") %>%
  rename(Variables = `Habitattype\r\n...10`) %>%
  mutate(Variables = str_remove_all(Variables, "\\r\\n")) %>%
  mutate(Subvariables = case_when(Variables == "Bar jord" ~ "Uden vegetationsdække",
                                  Variables == "lave urter" ~ "Græs/urteveg. under 15 cm",
                                  Variables == "middel urter" ~ "Græs/urtevegetation 15-50 cm",
                                  Variables == "høje urter" ~ "Græs/urtevegetation over 50 cm",
                                  Variables == "dværgbuske" ~ "Dværgbuske",
                                  Variables == "vedplanter" ~  "Vedplanter (kronedække)",
                                  Variables == "invasive planter" ~ "Forekomst af invasive arter",
                                  Variables == "afvanding" ~ "Afvanding og vandindvinding",
                                  Variables == "vandløb" ~ "Vandløb",
                                  Variables == "kystsikring" ~ "Kystsikring",
                                  Variables == "afgræsning" ~ "Græsning/høslæt",
                                  Variables == "gødskning" ~ "Gødskning el. sprøjteskader",
                                  Variables == "positive strukturer" ~  "Positive strukturer",
                                  Variables == "negative strukturer" ~ "Negative strukturer"),
         Variables = case_when(Variables == "Vegetationsstruktur" ~ "Vegetationsstruktur",
                               Variables == "Bar jord" ~ "Vegetationsstruktur",
                               Variables == "lave urter" ~ "Vegetationsstruktur",
                               Variables == "middel urter" ~ "Vegetationsstruktur",
                               Variables == "høje urter" ~ "Vegetationsstruktur",
                               Variables == "dværgbuske" ~ "Vegetationsstruktur",
                               Variables == "vedplanter" ~  "Vegetationsstruktur",
                               Variables == "invasive planter" ~ "Vegetationsstruktur",
                               Variables == "Hydrologi" ~ "Hydrologi",
                               Variables == "afvanding" ~ "Hydrologi",
                               Variables == "vandløb" ~ "Hydrologi",
                               Variables == "kystsikring" ~ "Hydrologi",
                               Variables == "landbrugspåvirkninger" ~ "Landbrugspåvirkninger",
                               Variables == "afgræsning" ~ "Landbrugspåvirkninger",
                               Variables == "gødskning" ~ "Landbrugspåvirkninger",
                               Variables == "Naturtypekarak. strukturer" ~  "Naturtypekarak. strukturer",
                               Variables == "positive strukturer" ~  "Naturtypekarak. strukturer",
                               Variables == "negative strukturer" ~ "Naturtypekarak. strukturer"))


Weights_Sheet3 <- Subweights_Sheet3 %>%
  dplyr::filter(is.na(Subvariables)) %>%
  dplyr::select(-Subvariables)

Subweights_Sheet3 <-  Subweights_Sheet3 %>%
  dplyr::filter(!is.na(Subvariables)) %>%
  rename(Subweights = Weight) %>%
  full_join(Weights_Sheet3) %>%
  relocate(Subweights, .after = Weight) %>%
  relocate(Habitat, .before = everything()) %>%
  mutate(Habitat = as.character(Habitat)) %>%
  full_join(Final_Sheet3)

Final_Sheet3 <- Subweights_Sheet3
