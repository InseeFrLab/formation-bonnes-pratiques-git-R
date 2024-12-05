library(dplyr)
library(ggplot2)
library(forcats)

read_yaml_secret <- function(path, key) {
  return(yaml::read_yaml(path)[[key]])
}
read_from_parquet <- function(path) {
  df <- arrow::read_parquet(
    path,
    col_select  = c(
      "region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
      "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
      "trans", "ur"
    )
  )
  return(df)
}

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



retraitement_donnees <- function(df){
  df <- df |>
    mutate(aged = as.numeric(aged))
  df$sexe <- df$sexe |>
    as.character() |>
    fct_recode(Homme = "1", Femme = "2")
  df <- df |>
    mutate(
      surf = factor(surf, ordered = TRUE),
      cs1 = factor(cs1)
    )
  return(df)
}


produce_table_age <- function(df){
  stats_age <- df |>
    group_by(decennie = decennie_a_partir_annee(aged)) |>
    summarise(n())
  
  table_age <- gt::gt(stats_age) |>
    gt::tab_header(
      title = "Distribution des âges dans notre population"
    ) |>
    gt::fmt_number(
      columns = `n()`,
      sep_mark = " ",
      decimals = 0
    ) |>
    gt::cols_label(
      decennie = "Tranche d'âge",
      `n()` = "Population"
    )
  return(table_age)
}