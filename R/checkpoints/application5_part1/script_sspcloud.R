library(arrow)
library(dplyr)
library(ggplot2)
library(forcats)


api_token <- Sys.getenv("JETON_API")

bucket_formation <- "projet-formation"
path_within_bucket <- "/bonnes-pratiques/data"

source("R/functions.R", encoding = "UTF-8")
  
  
# IMPORT ET STRUCTURATION DONNEES -------------


columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS", "IPONDI", "NUMMI"
)

bucket <- s3_bucket(bucket_formation, endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
bucket_path <- bucket$path(paste0(path_within_bucket, "/RPindividus"))

df <- open_dataset(
  bucket_path,
  hive_style = TRUE
) |>
  filter(REGION == 24) |>
  select(any_of(columns_subset)) |>
  collect()


df <- df |>
  mutate(SEXE = as.character(SEXE)) |>
  mutate(SEXE = fct_recode(SEXE, Homme = "1", Femme = "2"))



# STATISTIQUES AGREGEES ---------------------------------------


calculer_stat_agregee(df |> filter(SEXE == "Homme") |> pull(AGED))
calculer_stat_agregee(df |> filter(SEXE == "Femme") |> pull(AGED))


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


ggsave("output/p.png", p)


# PART DES SENIORS DANS LA POPULATION ---------------------------

# France geojson
departements <- aws.s3::s3read_using(
  FUN = sf::st_read, 
  object = "france.geojson", 
  bucket = paste0(bucket_formation, "/", path_within_bucket),
  opts = list("region" = "")
)


# PART DES SENIORS FRANCE ENTIERE =====================================

part_seniors <- open_dataset(
  bucket_path,
  hive_style = TRUE
) |>
  mutate(plus_60 = AGED > 60) |>
  group_by(DEPT, plus_60) |>
  summarise(
    population_totale = sum(IPONDI)
  ) |>
  group_by(DEPT) |>
  mutate(
    population_60_ans = population_totale,
    pourcentage_60_plus = population_totale/sum(population_totale),
    population_totale = sum(population_totale)
  ) |>
  filter(plus_60 == TRUE) |>
  select(DEPT, population_totale, population_60_ans, pourcentage_60_plus) |>
  collect()


# CARTE =====================================

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



# MODELISATION --------------------------------

data_modelisation <- df |>
  filter(SURF != "Z") |>
  mutate(SURF = factor(SURF, ordered = TRUE)) |>
  filter(between(AGED, 40, 60)) |>
  sample_n(1000)


MASS::polr(SURF ~ factor(COUPLE) + factor(TP), data_modelisation)
