#' Get an Struktureindeks value from a specific habitat type
#'
#' @param Habitat_name a character with the habitat found in the location,
#' a full list can be found in the \code{\link{Habitat_List}} dataset in the
#' habitat_name column.
#' @param Habitat_code Only use if you didn't provide the Habitat_name,
#' a character with the habitat found in the location,  a full list can be
#' found in the \code{\link{Habitat_List}} dataset in the  habitat_name
#' column.
#'
#' @return an Struktureindeks value
#'
#' @examples
#' \dontrun{
#' data("Habitat_List")
#'
#' set.seed(2022)
#'
#' Strukturindeks(Habitat_name = Habitat_List$habitat_name[10])
#' }
#'
#'
#'
#' @importFrom dplyr filter
#' @export

Strukturindeks <- function(Habitat_name = NULL, Habitat_code = NULL){
  ## Filter Habitat

  Subvariables <- habitatnaturtype <- habitat_name <- NULL

  if(!is.null(Habitat_name) & length(Habitat_name) == 1){
    message(paste("Using habitat name for filtering, habitat =", Habitat_name))
    Temp <- ScoreByHabitat[[2]] %>%
      dplyr::filter(habitat_name == Habitat_name)
  } else if(!is.null(Habitat_code) & length(Habitat_code) == 1){
    message(paste("Using habitat code for filtering, habitat =", Habitat_code))
    Temp <- ScoreByHabitat[[2]] %>%
      dplyr::filter(habitatnaturtype == Habitat_code)
  } else if(is.null(Habitat_name) & is.null(Habitat_code)){
    stop("Provide a habitat")
  }

  Subs <- Temp$Subvariables %>% unique()

  Results <- list()

  for(i in 1:length(Subs)){
    ForScores <- Temp %>%
      dplyr::filter(Subvariables == Subs[i])

    Options <- paste(paste(1:length(ForScores$Scores), ForScores$Scores), collapse = "\n ")

    var = readline(paste("Choose an alternative number for", Subs[i], "\n ", Options))
    # convert the inputted value to integer
    var = as.integer(var)

    Results[[i]] <- ForScores[ForScores$Scores == ForScores$Scores[var],]
  }
  Results <- do.call("rbind", Results)

  Scores <- sum(Results$Weight * Results$Subweights * Results$Values)
  Weights <- sum(Results$Weight * Results$Subweights)
  Index <- Scores/(Weights*100)
  return(Index)
}
