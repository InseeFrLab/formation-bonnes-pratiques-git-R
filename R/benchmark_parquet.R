library(fs)
library(profvis)
library(DBI)
library(arrow)
library(dplyr)
library(glue)
library(gt)
library(gtExtras)

source("R/benchmark_functions.R")

# PARAMETERS --------------------------------------------


columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS"
)

filename_sample_csv <- "data/RPindividus_24.csv"
filename_sample_parquet <- gsub("csv", "parquet", filename_sample_csv)
filename_full_parquet <- gsub("_24", "", filename_sample_parquet)
filename_full_csv <- gsub("parquet", "csv", filename_full_parquet)


# WRITE PARQUET PARTIONED IF NECESSARY
unlink("./data/RPindividus")
rp <- open_dataset(filename_full_parquet) %>%
  group_by(REGION) %>%
  write_dataset("./data/RPindividus")


# DISK FILESIZE ------------------------------------------------

disk_usage_sample_csv <- file_size(filename_sample_csv)
disk_usage_sample_parquet <- file_size(filename_sample_parquet)
disk_usage_full_parquet <- file_size(filename_full_parquet)
disk_usage_full_csv <- file_size(filename_full_csv)

disk_usage <- list(
  sample_csv = disk_usage_sample_csv,
  sample_parquet = disk_usage_sample_parquet,
  full_parquet = disk_usage_full_parquet,
  full_csv = disk_usage_full_csv
)

# IMPORT TIME --------------------------------------------------

# CSV (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ====================
diff_time_csv <- import_time_csv(filename_sample_csv, readr::read_csv)
diff_time_csv_subset <- import_time_csv(
  filename_sample_csv, readr::read_csv, col_names = columns_subset
)

# PARQUET (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ===============
diff_time_parquet <- import_time_parquet(filename_sample_parquet)  
diff_time_parquet_subset <- import_time_parquet(
  filename_sample_parquet,
  col_names = columns_subset 
)

# PARQUET (FULL): WITH AND WITHOUT COLUMN CONDITIONING ===============
diff_time_parquet_full <- import_time_parquet(filename_full_parquet)
diff_time_parquet_full_sample <- import_time_parquet(
  filename_full_parquet,
  col_names = columns_subset
)

# CSV (FULL): WITH AND WITHOUT COLUMN CONDITIONING ===============
# /!\ ne faire tourner qu'une fois, c'est loooooooooooong 
diff_time_csv_full <- import_time_csv(filename_full_csv, readr::read_csv)

# PARTITIONNED PARQUET: WITH AND WITHOUT COLUMN CONDITIONING ===============
diff_time_parquet_partioned <- import_time_parquet_partioned(filename_full_parquet)
diff_time_parquet_partioned_sample <- import_time_parquet(
  filename_full_parquet,
  col_names = columns_subset
)



# PROFILING: VISUALISING TIME SPENT ON DIFFERENT OPERATIONS ----------------------

# CSV (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ====================
profvis(readr::read_csv(filename_sample_csv), interval = .005)
profvis(
  readr::read_csv(filename_sample_csv, col_names = columns_subset), interval = .005
)
# ce que ça nous dit: globalement le temps en lecture change pas (logique: csv)


# PARQUET (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ===============
profvis(
  open_dataset(filename_sample_parquet) %>% collect(),
  interval = .005
)
profvis(
  open_dataset(filename_sample_parquet) %>%
    select(any_of(columns_subset)) %>%
    collect(),
  interval = .005)
# ça va vite mais on voit déjà une diff: si on passait sur le gros fichier ? ;) 


# PARQUET (FULL): WITH AND WITHOUT COLUMN CONDITIONING ===============
profvis(
  open_dataset(filename_full_parquet) %>% collect(),
  interval = .005
)
profvis(
  open_dataset(filename_full_parquet) %>%
    select(any_of(columns_subset)) %>%
    collect(),
  interval = .005
)

# DIMENSIONS ---------------------------------------------------------

complete <- open_dataset(filename_full_parquet) %>% collect()
sample <- open_dataset(filename_sample_parquet) %>%
  select(any_of(columns_subset)) %>% collect()

dims_complete <- complete %>% dim()
dims_sample <- sample %>% dim()


# ON MET TOUT ENSEMBLE ------------------------------------------------


timings <- list(
  csv_sample = diff_time_csv,
  csv_sample_subset = diff_time_csv_subset,
  parquet_sample = diff_time_parquet,
  parquet_sample_subset = diff_time_parquet_subset,
  parquet_full = diff_time_parquet_full,
  parquet_full_subset = diff_time_parquet_full_sample,
  parquet_partitioned_full = diff_time_parquet_partioned,
  parquet_partitioned_full_subset = diff_time_parquet_partioned_sample,
  csv_full = diff_time_csv_full
)

dimensions <- list(
  complete = dims_complete,
  sample = dims_sample
)


results_df <- create_results_df(disk_usage, timings, dimensions)

create_report_table(results_df)








