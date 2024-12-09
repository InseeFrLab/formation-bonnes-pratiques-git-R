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
#' calculer_stat_agregee(rnorm(10))
#' calculer_stat_agregee(rnorm(10), "ecart-type")
#' calculer_stat_agregee(rnorm(10), "variance")
#'
#' @export

calculer_stat_agregee <- function(x, statistique = "moyenne", ...) {
  
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


# Fonction pour générer une pyramide des âges
generer_pyramide_ages <- function(df) {
  print("Pyramide des âges de la population française")
  
  pyramide_ages <- df |>
    group_by(AGED) |>
    summarise(n = sum(IPONDI))
  
  ggplot(df) +
    geom_histogram(
      aes(x = 5 * floor(as.numeric(AGED) / 5), weight = IPONDI),
      stat = "count"
    )
}

# Fonction pour calculer les modalités de transport par statut familial
calculer_transport_par_statut <- function(df) {
  print("Modalités de transports par statut familial")
  
  transport_par_statut_couple <- df |>
    group_by(COUPLE, TRANS) |>
    summarise(x = sum(IPONDI)) |>
    group_by(COUPLE) |>
    mutate(y = 100 * x / sum(x))
  
  return(transport_par_statut_couple)
}

# Fonction pour calculer la part des hommes dans chaque cohorte
calculer_part_hommes <- function(df) {
  print("Part des hommes dans chaque cohorte")
  
  part_hommes_chaque_cohorte <- df |>
    select(AGED, SEXE, IPONDI) |>
    group_by(AGED, SEXE) |>
    summarise(SH_sexe = sum(IPONDI)) |>
    group_by(AGED) |>
    mutate(SH_sexe = SH_sexe / sum(SH_sexe)) |>
    filter(SEXE == "Homme")
  
  p <- ggplot(part_hommes_chaque_cohorte) +
    geom_bar(aes(x = AGED, y = SH_sexe), stat = "identity") +
    geom_point(aes(x = AGED, y = SH_sexe), stat = "identity", color = "red") +
    coord_cartesian(c(0, 100))
  
  return(p)
}

# Fonction pour générer une carte des seniors par département
generer_carte_seniors <- function(departements, part_seniors) {
  print("Carte de la part des seniors")
  
  departements_60_plus_sf <- departements |>
    inner_join(
      part_seniors,
      by = c("INSEE_DEP" = "DEPT")
    )
  
  ggplot(departements_60_plus_sf) +
    geom_sf(aes(fill = pourcentage_60_plus)) + 
    scale_fill_fermenter(n.breaks = 5, palette = "PuBuGn", direction = 1) + 
    theme_void() + 
    labs(
      title = "Part des personnes de plus de 60 ans par département",
      caption = "Source: Insee, Fichiers détails du recensement de la population",
      fill = "Part (%)"
    )
}