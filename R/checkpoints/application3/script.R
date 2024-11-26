library(arrow)
library(dplyr)
library(ggplot2)
library(forcats)


api_token <- Sys.getenv("JETON_API")

source("R/functions.R", encoding = "UTF-8")


# IMPORT ET STRUCTURATION DONNEES -------------


columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS", "IPONDI"
)

df <- open_dataset(
  "data/RPindividus_partitionne.parquet",
  hive_style = TRUE
) %>%
  filter(REGION == 24) %>%
  select(any_of(columns_subset)) %>%
  collect()


df <- df %>%
  mutate(SEXE = as.character(SEXE)) %>%
  mutate(SEXE = fct_recode(SEXE, Homme = "1", Femme = "2"))



# STATISTIQUES AGREGEES ---------------------------------------


stat_desc_variable(df %>% filter(SEXE == "Homme") %>% pull(AGED))
stat_desc_variable(df %>% filter(SEXE == "Femme") %>% pull(AGED))


# PYRAMIDE AGES =============================

pyramide_ages <- df %>%
  group_by(AGED) %>%
  summarise(n = sum(IPONDI))

ggplot(df) +
  geom_histogram(
    aes(x = 5 * floor(as.numeric(AGED) / 5), weight = IPONDI),
    stat = "count"
  )


# STATS MODALITES DE TRANSPORT ===============


transport_par_statut_couple <- df %>%
  group_by(COUPLE, TRANS) %>%
  summarise(x = sum(IPONDI)) %>%
  group_by(COUPLE) %>%
  mutate(y = 100 * x / sum(x))

transport_par_statut_couple


# PART HOMMES DANS CHAQUE COHORTE ============================


part_hommes_chaque_cohorte <- df %>%
  select(AGED, SEXE, IPONDI) %>%
  group_by(AGED, SEXE) %>%
  summarise(SH_sexe = sum(IPONDI)) %>%
  group_by(AGED) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(SEXE == "Homme")


p <- ggplot(part_hommes_chaque_cohorte) +
  geom_bar(aes(x = AGED, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = AGED, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


ggsave("output/p.png", p)


# MODELISATION --------------------------------

data_modelisation <- df %>%
  filter(SURF != "Z") %>%
  mutate(SURF = factor(SURF, ordered = TRUE)) %>%
  filter(between(AGED, 40, 60)) %>%
  sample_n(1000)


MASS::polr(SURF ~ factor(COUPLE) + factor(TP), data_modelisation)
