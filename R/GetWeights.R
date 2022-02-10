#' Get the weights and subweights for a particular habitat
#'
#' @param Habitat_name a character with the habitat found in the location,
#' a full list can be found in the \code{\link{Habitat_List}} dataset in the
#' habitat_name column.
#'
#' @param Habitat_code Only use if you didn't provide the Habitat_name,
#' a character with the habitat found in the location,  a full list can be
#' found in the \code{\link{Habitat_List}} dataset in the  habitat_name
#' column.
#'
#' @return a data frame with the weights and subweights of each habitat
#' @examples
#'
#' data("Habitat_List")
#'
#' GetWeights(Habitat_name = Habitat_List$habitat_name[10])
#'
#' @importFrom dplyr filter
#' @export

GetWeights <- function(Habitat_name = NULL, Habitat_code = NULL){
  Subvariables <- habitatnaturtype <- habitat_name <- NULL

  if(!is.null(Habitat_name) & length(Habitat_name) == 1){
    message(paste("Using habitat name for filtering, habitat =", Habitat_name))
    Temp <- ScoreByHabitat[[2]] %>%
      dplyr::filter(habitat_name == Habitat_name)
  } else if(!is.null(Habitat_code) & length(Habitat_code) > 0){
    message(paste("Using habitat code for filtering, habitat =", Habitat_code))
    Temp <- ScoreByHabitat[[2]] %>%
      dplyr::filter(habitatnaturtype == Habitat_code)
  } else if(is.null(Habitat_name) & is.null(Habitat_code)){
    stop("Provide a habitat")
  }
  return(Temp)
}
