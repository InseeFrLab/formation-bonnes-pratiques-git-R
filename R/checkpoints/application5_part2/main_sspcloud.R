library(arrow)
library(dplyr)
library(ggplot2)
library(forcats)


api_token <- Sys.getenv("JETON_API")
bucket_formation <- "projet-formation"
path_within_bucket <- "/bonnes-pratiques/data"


source("R/functions.R", encoding = "UTF-8")
source("R/functions_import.R", encoding = "UTF-8")
source("R/functions_stats_desc.R", encoding = "UTF-8")
source("R/functions_models.R", encoding = "UTF-8")


# ENVIRONNEMENT DE STOCKAGE -------------------

bucket <- s3_bucket(bucket_formation, endpoint_override = Sys.getenv("AWS_S3_ENDPOINT"))
bucket_path <- bucket$path(paste0(path_within_bucket, "/RPindividus"))

  
# IMPORT ET STRUCTURATION DONNEES -------------

columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS", "IPONDI"
)


# Données recensement
df <- import_recensement_subset(
  bucket_path, REGION = 24, cols = columns_subset
)


# Shapefile départements
departements <- import_shapefile_departement(
  path = paste0(bucket_formation, "/", path_within_bucket)
)

print("Calcul de la part de seniors dans chaque département")
part_seniors <- compute_part_seniors_by_dep(bucket_path)

  
# STATISTIQUES AGREGEES ---------------------------------------


stat_desc_variable(df %>% filter(SEXE == "Homme") %>% pull(AGED))
stat_desc_variable(df %>% filter(SEXE == "Femme") %>% pull(AGED))


# Pyramide des âges
generer_pyramide_ages(df)

# Modalités de transport par statut familial
resultats_transport <- calculer_transport_par_statut(df)
print(resultats_transport)

# Part des hommes dans chaque cohorte
plot_part_hommes <- calculer_part_hommes(df)
print(plot_part_hommes)

# Carte des seniors
generer_carte_seniors(departements, part_seniors)


# MODELISATION --------------------------------

print("Modélisation")
modelisation_recensement(df)

