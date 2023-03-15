# _targets.R file
library(targets)
source("R/functions.R")
tar_option_set(packages = c("arrow","dplyr", "ggplot2", "forcats", "gt"))

source("R/functions.R", encoding = "UTF-8")

list(
  tar_target(file_token, "secrets.yaml", format = "file"),
  tar_target(file_data, "individu_reg.parquet", format = "file"),
  tar_target(token, read_yaml_secret(file_token)),
  tar_target(data, read_from_parquet(file_data), format = "parquet"),
  tar_target(clean_data, retraitement_donnees(data)),
  tar_target(table_age, produce_table_age(clean_data)),
  tar_target(graph_part_hommes, figure_part_homme_age(clean_data))
)