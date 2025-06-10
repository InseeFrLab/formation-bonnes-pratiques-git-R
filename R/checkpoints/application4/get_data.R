
system("mc cp s3/projet-formation/diffusion/bonnes-pratiques/data/france.geojson data/france.geojson")

# Pour réimporter le parquet du RP si besoin

# system("mc cp s3/projet-formation/diffusion/bonnes-pratiques/data/RPindividus.parquet data/RPindividus.parquet")
# arrow::open_dataset("data/RPindividus.parquet") |>
#   dplyr::group_by(REGION, DEPT) |>
#   arrow::write_dataset("data/RPindividus_partitionne.parquet")
