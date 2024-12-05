library(dplyr)
library(ggplot2)
library(forcats)


api_token <- Sys.getenv("JETON_API")


# FUNCTIONS -----------------------------------------------

decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
}

fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")



# IMPORT ET STRUCTURATION DONNEES -------------


df <- readr::read_csv(
  "RPindividus_24.csv",
  col_select = c(
    "REGION", "AGED", "ANAI", "CATL", "COUPLE",
    "SEXE", "SURF", "TP", "TRANS", "IPONDI"
  )
)


df <- df |>
  mutate(SEXE = as.character(SEXE)) |>
  mutate(SEXE = fct_recode(SEXE, Homme = "1", Femme = "2"))



# STATISTIQUES AGREGEES ---------------------------------------

fonction_de_stat_agregee(df |> filter(SEXE == "Homme") |> pull(AGED))
fonction_de_stat_agregee(df |> filter(SEXE == "Femme") |> pull(AGED))


# PYRAMIDE AGES =============================

pyramide_ages <- df |>
  group_by(AGED) |>
  summarise(n = sum(IPONDI))

ggplot(df) +
  geom_histogram(
    aes(x = 5 * floor(as.numeric(AGED) / 5), weight = IPONDI),
    stat = "count"
  )


# STATS MODALITES DE TRANSPORT ===============

transport_par_statut_couple <- df |>
  group_by(COUPLE, TRANS) |>
  summarise(x = sum(IPONDI)) |>
  group_by(COUPLE) |>
  mutate(y = 100 * x / sum(x))
transport_par_statut_couple

# PART HOMMES DANS CHAQUE COHORTE ============================


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


ggsave("p.png", p)


# MODELISATION --------------------------------

data_modelisation <- df |>
  filter(SURF != "Z") |>
  mutate(SURF = factor(SURF, ordered = TRUE)) |>
  filter(between(AGED, 40, 60)) |>
  sample_n(1000)


MASS::polr(SURF ~ factor(COUPLE) + factor(TP), data_modelisation)
