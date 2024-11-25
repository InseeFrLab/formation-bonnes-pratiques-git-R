import_recensement_subset <- function(bucket_path, REGION, cols){
  
  df <- open_dataset(
    bucket_path,
    hive_style = TRUE
  ) %>%
    filter(REGION == 24) %>%
    select(any_of(cols)) %>%
    collect()
  
  
  df <- df %>%
    mutate(SEXE = as.character(SEXE)) %>%
    mutate(SEXE = fct_recode(SEXE, Homme = "1", Femme = "2"))
  
  return(df)  
}


import_shapefile_departement <- function(path){
  
  departements <- aws.s3::s3read_using(
    FUN = sf::st_read, 
    object = "france.geojson", 
    bucket = path,
    opts = list("region" = "")
  )

  return(departements)
}


compute_part_seniors_by_dep <- function(bucket_path){
  
  part_seniors <- open_dataset(
    bucket_path,
    hive_style = TRUE
  ) %>%
    mutate(plus_60 = AGED > 60) %>%
    group_by(DEPT, plus_60) %>%
    summarise(
      population_totale = sum(IPONDI)
    ) %>%
    group_by(DEPT) %>%
    mutate(
      population_60_ans = population_totale,
      pourcentage_60_plus = population_totale/sum(population_totale),
      population_totale = sum(population_totale)
    ) %>%
    filter(plus_60 == TRUE) %>%
    select(DEPT, population_totale, population_60_ans, pourcentage_60_plus) %>%
    collect()
  
  return(part_seniors)
  
}
