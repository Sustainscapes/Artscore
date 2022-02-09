#' Get an Artscore from a list of species and a specific habitat type
#'
#' @param ScientificName a vector with the list of found species in a location the
#' full list can be found in the \code{\link{Species_List}} dataset in the
#' Scientific_name column.
#' @param Common_name Only use if you didn't provide the Scientific name,
#' a vector with the list of found species in a location the
#' full list can be found in the \code{\link{Species_List}} dataset in the
#' Danish_name column.
#' @param Habitat_name a character with the habitat found in the location,
#' a full list can be found in the \code{\link{Habitat_List}} dataset in the
#' habitat_name column.
#' @param Habitat_code Only use if you didn't provide the Habitat_name,
#' a character with the habitat found in the location,  a full list can be
#' found in the \code{\link{Habitat_List}} dataset in the  habitat_name
#' column.
#'
#' @return a data frame with the intermediate calculations and the Artscore
#'
#' @details
#'
#' The dataframe returns the following calculated values
#'
#' m = Adjusted average score for the test field.
#'
#' a(b) = Number (a) contribution species (b)
#' (number with species score above 0). Coded as a_b in the data
#' frame.
#'
#' s = Artsum calculated as follows:
#'
#' \eqn{s = m \times a(b)}
#'
#' m(a) = Average adjusted mean score, coded as m_a in the data
#' frame
#'
#' A(s) = Artcore index, coded as A_s in the data frame calculated
#' as follows
#'
#' \eqn{A(s) = \frac{1}{(1+e^{m(a))} \times e^{1.60(1-m)}}}
#'
#' a(t) = number of plant species in the sample field
#' (without mosses etc.), coded as a_t in the data frame
#'
#' @importFrom dplyr filter
#' @export


Artscore <- function(ScientificName = NULL, Common_name = NULL, Habitat_name = NULL, Habitat_code = NULL){
  ## Filter by species
  Scientific_name <- Danish_name <- habitat_name <- habitatnaturtype <- NULL
  if(!is.null(ScientificName) & length(ScientificName) >=1){
    message("Using scientific names for filtering")
    Temp <- ScoreByHabitat %>%
      dplyr::filter(Scientific_name %in% ScientificName)
  } else if(!is.null(Common_name) & length(Common_name) >= 1){
    message("Using common names for filtering")
    Temp <- ScoreByHabitat %>%
      dplyr::filter(Danish_name %in% Common_name)
  } else if(is.null(ScientificName) & is.null(Common_name)){
    stop("Provide a species list")
  }
  ## Check for species validity
  if(nrow(Temp) == 0){
    stop("None of your species matches the list, check Species_List dataset")
  }

  ## filter by Habitat

  if(!is.null(Habitat_name) & length(Habitat_name) == 1){
    message(paste("Using habitat name for filtering, habitat =", Habitat_name))
    Temp <- Temp %>%
      dplyr::filter(habitat_name == Habitat_name)
  } else if(!is.null(Habitat_code) & length(Habitat_code) == 1){
    message(paste("Using habitat code for filtering, habitat =", Habitat_code))
    Temp <- Temp %>%
      dplyr::filter(habitatnaturtype == Habitat_code)
  } else if(is.null(Habitat_name) & is.null(Habitat_code)){
    stop("Provide a habitat")
  }

  ## Check for species validity
  if(nrow(Temp) == 0){
    stop("None of your habitat matches the list, check Habitat_List dataset")
  }

  ## Start calculations

  m <- mean(Temp$Artscore)
  a_b <- sum(Temp$Bilagsart)
  s <- m*a_b
  m_a <- unique(Temp$gennemsnitlig_middelscore)
  A_s <- 1/((1+exp(m_a))*exp(1.6*(1-m)))
  a_t <- nrow(Temp)



  Artscore_result <- data.frame(m = m,
                                a_b = a_b,
                                s = s,
                                m_a = m_a,
                                A_s = A_s,
                                a_t = a_t)

  return(Artscore_result)
}
