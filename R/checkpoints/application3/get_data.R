library(dplyr)

options(timeout = max(300, getOption("timeout")))

# FUNCTIONS ---------------------------------------------

filename_table_individu <- "data/RPindividus.parquet"
system(
  "mc cp s3/projet-formation/bonnes-pratiques/data/RPindividus.parquet data/RPindividus.parquet"
)

# ALTERNATIVE EN CSV POUR LES BENCHMARKS -------------------------

rp <- arrow::open_dataset(filename_table_individu)

# Echantillon

rp24 <- rp |>
  filter(REGION == "24")

rp24 |> 
  collect() |> 
  as_tibble() |> 
  readr::write_csv("data/RPindividus_24.csv")

arrow::write_parquet(rp24, "data/RPindividus_24.parquet")


# DOCUMENTATION -------------------------------------------

# Pour info, voici le script qui a créé ce fichier
# download_if_not_exists <- function(url, filename) {
#   if (!file.exists(filename)) {
#     download.file(url, filename)
#     message(paste("Downloaded:", filename))
#   } else {
#     message(paste("File already exists:", filename))
#   }
# }
# 
# dir.create("data")
# 
# url_table_individu <- "https://static.data.gouv.fr/resources/recensement-de-la-population-fichiers-detail-individus-localises-au-canton-ou-ville-2020-1/20231023-122841/fd-indcvi-2020.parquet"
# filename_table_individu <- "data/RPindividus.parquet"
# system("mc cp data/RPindividus.parquet s3/projet-formation/bonnes-pratiques/data/RPindividus.parquet")


# Pour info, le CSV du RP complet a été créé de cette manière:
# readr::write_csv(
#   rp |> collect() |> as_tibble(),
#   "data/RPindividus.csv"
# )
