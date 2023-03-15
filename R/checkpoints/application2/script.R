# Chaine de production sur le fichier recensement diffusé par l'Insee

# GESTION ENVIRONNEMENT ----------------------

library(dplyr)
library(ggplot2)
library(forcats)

api_token <- yaml::read_yaml("secrets.yaml")$JETON_API

source("R/functions.R", encoding = "UTF-8")

# IMPORT DONNEES -----------------------------

df <- readr::read_csv2(
  "individu_reg.csv",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2",
                 "cs3", "couple", "na38", "naf08", "pnai12", "sexe",
                 "surf", "tp", "trans", "ur")
)

# RETRAITEMENT --------------------------------

df <- df %>%
  mutate(aged = as.numeric(aged))

df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")


# STATISTIQUES DESCRIPTIVES --------------------

summarise(group_by(df, aged), n())

stats_agregees(df %>% filter(sexe == "Homme") %>% pull(aged))
stats_agregees(df %>% filter(sexe == "Femme") %>% pull(aged))

## stats trans par statut =====================

df3 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))


# GRAPHIQUES -----------------------------------

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(aged / 5)), stat = "count")

# part d'homme dans chaque cohort
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

ggsave("p.png", p)


# MODELISATION -------------------------------

df3 <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")

df3 <- df3 %>%
  mutate(
    surf = factor(df3$surf, ordered = TRUE),
    cs1 = factor(cs1)
  )

MASS::polr(surf ~ cs1 + factor(ur), df3)
