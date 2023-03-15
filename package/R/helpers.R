
#' Title
#'
#' @param annee An integer. The year from which we want to compute the decade.
#'
#' @return An integer. The start year of the decade of the input year.
#' @export
#'
#' @examples
#' decennie_a_partir_annee(2022)
#'
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}
