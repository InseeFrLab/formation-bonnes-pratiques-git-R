
df_parquet <- 
  aws.s3::s3read_using(
    FUN = arrow::read_parquet,
    object = "/diffusion/RP/2016/individu_reg/individu_reg.parquet",
    bucket = "donnees-insee",
    opts = list("region" = "")
  )

df_subset <- data.table::setDT(df_parquet) %>%
  dplyr::slice_sample(n=100000)

aws.s3::s3write_using(
  df_subset,
  FUN = data.table::fwrite,
  sep = ";",
  object = "diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv",
  bucket = "projet-formation",
  opts = list("region" = "")
  )
