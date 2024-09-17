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

#' Créer un graphique de la part des hommes
#'
#' @param df un data.frame
#' @return un graphique ggplot
figure_part_homme_age <- function(df){
  p <- df %>%
    group_by(aged, sexe) %>%
    summarise(SH_sexe = n()) %>%
    group_by(aged) %>%
    mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
    filter(sexe == "Homme") %>%
    ggplot() +
    geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
    geom_point(
      aes(x = aged, y = SH_sexe),
      stat = "identity", color = "red") +
    coord_cartesian(c(0, 100))
  return(p)
}
