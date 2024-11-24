decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' @title Calcul d'une statistique agrégée
#' 
#' @description
#' Cette fonction calcule une statistique agrégée (moyenne, écart-type ou variance) pour un vecteur donné.
#'
#' @param x Un vecteur numérique pour lequel la statistique doit être calculée.
#' @param statistique Le nom de la statistique à calculer. Peut être `"moyenne"`, `"ecart-type"` (ou `"sd"`), ou `"variance"`. Par défaut, `"moyenne"`.
#' @param ... Arguments supplémentaires passés à la fonction de calcul sous-jacente (voir [mean()], [sd()], [var()]).
#'
#' @return La valeur de la statistique agrégée calculée.
#'
#' @details
#' - Pour `"moyenne"`, la fonction utilise [mean()].
#' - Pour `"ecart-type"` ou `"sd"`, la fonction utilise [sd()].
#' - Pour `"variance"`, la fonction utilise [var()].
#'
#' @examples
#' stat_desc_variable(rnorm(10))
#' stat_desc_variable(rnorm(10), "ecart-type")
#' stat_desc_variable(rnorm(10), "variance")
#'
#' @export

stat_desc_variable <- function(x, statistique = "moyenne", ...) {
  
  stopifnot(is.numeric(x), 
            is.character(statistique), 
            statistique %in% c("moyenne", "ecart-type", "sd", "variance"))
  
  if (statistique == "moyenne") {
    resultat <- mean(x, na.rm = TRUE, ...)
  } else if (statistique == "ecart-type" || statistique == "sd") {
    resultat <- sd(x, na.rm = TRUE, ...)
  } else if (statistique == "variance") {
    resultat <- var(x, na.rm = TRUE, ...)
  }
  
  return(resultat)
}
