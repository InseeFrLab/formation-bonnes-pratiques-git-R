decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' Calcul automatique de stats agrégées
#'
#' @param x Un vecteur
#' @param stat La statistique d'intérêt. Peut-être "moyenne", "écart-type"
#'  ou "variance". Par défaut "moyenne"
#' @param ...  Arguments additionnels à passer aux fonctions de stats agrégées
#'
#' @return Un vecteur avec la statistique d'intérêt
#' @export
#'
#' @examples
#' fonction_de_stat_agregee(rnorm(10))
#' fonction_de_stat_agregee(rnorm(10), "ecart-type")
#' fonction_de_stat_agregee(rnorm(10), "variance")
stats_agregees <- function(x, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    resultat <- mean(x, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    resultat <- sd(x, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    resultat <- var(x, na.rm = TRUE, ...)
  }
  return(resultat)
}

