
system("mc cp s3/public/ssplab-formation/france.geojson data/france.geojson")

# Pour réimporter le parquet du RP si besoin

# system("mc cp s3/public/ssplab-formation/RPindividus.parquet data/RPindividus.parquet")
# arrow::open_dataset("data/RPindividus.parquet") %>% 
#   group_by(REGION, DEPT) %>% 
#   arrow::write_dataset("data/RPindividus_partitionne.parquet")
