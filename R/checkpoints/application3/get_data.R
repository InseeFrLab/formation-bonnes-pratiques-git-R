
df <- 
  aws.s3::s3read_using(
    FUN = readr::read_csv2,
    object = "diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv",
    bucket = "projet-formation",
    opts = list("region" = "")
  )

arrow::write_parquet(df, "individu_reg.parquet")
