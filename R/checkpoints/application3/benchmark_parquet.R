library(dplyr)
library(ggplot2)
library(bench)


req_csv <- function() {
  res <- readr::read_csv("data/RPindividus_24.csv") |> 
    filter(DEPT == "36") |>
    group_by(AGED, DEPT) |>
    summarise(n_indiv = sum(IPONDI))

  return(res)
}

req_read_parquet <- function() {
  res <- arrow::read_parquet("data/RPindividus_24.parquet") |> 
    filter(DEPT == "36") |>
    group_by(AGED, DEPT) |>
    summarise(n_indiv = sum(IPONDI))
  
  return(res)
}

req_open_dataset <- function() {
  res <- arrow::open_dataset("data/RPindividus_24.parquet") |> 
    filter(DEPT == "36") |>
    group_by(AGED, DEPT) |>
    summarise(n_indiv = sum(IPONDI)) |> 
    collect()
  
  return(res)
}

req_open_dataset_full <- function() {
  res <- arrow::open_dataset("data/RPindividus.parquet") |> 
    filter(DEPT == "36") |>
    group_by(AGED, DEPT) |>
    summarise(n_indiv = sum(IPONDI)) |> 
    collect()
  
  return(res)
}

req_open_dataset_part <- function() {
  res <- arrow::open_dataset("data/RPindividus_partitionne.parquet", hive_style = TRUE) |> 
    filter(DEPT == "36") |>
    group_by(AGED, DEPT) |>
    summarise(n_indiv = sum(IPONDI)) |> 
    collect()
  
  return(res)
}

# Requête sur l'échantillon
benchmark1 <- bench::mark(
  req_csv = req_csv(),
  req_read_parquet = req_read_parquet(),
  req_open_dataset = req_open_dataset(),
  iterations = 1,
  check = FALSE
)

# Export du parquet partitionné
arrow::open_dataset("data/RPindividus.parquet") |> 
  group_by(REGION, DEPT) |> 
  arrow::write_dataset("data/RPindividus_partitionne.parquet")

# Requête sur la table complète
benchmark2 <- bench::mark(
  req_open_dataset_full = req_open_dataset_full(),
  req_open_dataset_part = req_open_dataset_part(),
  iterations = 1,
  check = FALSE
)
