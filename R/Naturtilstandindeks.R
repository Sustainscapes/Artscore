#' Natural condition index
#'
#' @param Artscore a numeric value, can be calculated using the,
#' \code{\link{Artscore}} function.
#'
#' @param Strukturindeks a numeric value, can be calculated using the,
#' \code{\link{Strukturindeks}} function.
#'
#' @return a numeric value with the Natual condition index
#' @examples
#'
#' data("Habitat_List")
#' data("Species_List")
#'
#' set.seed(2022)
#'
#' Ascore <- Artscore(Common_name = sample(Species_List$Danish_name, 10),
#' Habitat_code = Habitat_List$Code[10])
#' Naturtilstandindeks(Artscore = Ascore$Artsindex,
#' Strukturindeks = 0.6)
#'
#' @export

Naturtilstandindeks <- function(Artscore, Strukturindeks){
  Index <- ifelse(Artscore > Strukturindeks, (Artscore*0.4 + Strukturindeks*0.6), (Artscore*0.6 + Strukturindeks*0.4))
  return(Index)
}
