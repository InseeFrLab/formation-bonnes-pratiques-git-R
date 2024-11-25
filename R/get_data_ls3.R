df <- 
  aws.s3::s3read_using(
    FUN = readr::read_csv2,
    object = "ssplab-formation/rp_2016_individu_sample.csv",
    bucket = "public/",
    opts = list("region" = "")
  )

readr::write_csv2(df, "individu_reg.csv")
