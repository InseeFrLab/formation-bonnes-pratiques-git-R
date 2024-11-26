library(fs)
library(readr)
library(arrow)
library(dplyr)
library(glue)
library(gt)
library(gtExtras)



file_size <- function(file_path) {
  size_in_bytes <- file.info(file_path)$size
  return(
    fs::fs_bytes(size_in_bytes)
  )
}


import_time_csv <- function(path, import_fun = .f, ...) {
  
  start_time <- Sys.time()
  csv_data <- import_fun(path, ...)
  end_time <- Sys.time()
  diff_time <- end_time - start_time
  
  return(diff_time)
  
}

import_time_parquet <- function(path, col_names = NULL) {
  
  start_time <- Sys.time()
  parquet_data <- open_dataset(path)
  
  if (!is.null(col_names)){
    parquet_data <- parquet_data %>%
      select(any_of(col_names))
  }
  
  parquet_data <- parquet_data %>% collect()
  
  end_time <- Sys.time()
  diff_time <- end_time - start_time
  
  return(diff_time)
  
}


import_time_parquet_partioned <- function(
    path = "data/RPindividus.parquet",
    col_names = NULL
){
  
  start <- Sys.time()
  
  parquet_data <- open_dataset(
    path,
    hive_style = TRUE,
  ) %>%
    filter(region == "24")
  
  if (!is.null(col_names)){
    parquet_data <- parquet_data %>%
      select(any_of(col_names))
  }
  
  parquet_data <- parquet_data %>% collect()
  diff_time <- Sys.time() - start
  
  return(diff_time)
  
}


create_results_df <- function(disk_usage, timings, dimensions) {
  
  results_df <- data.frame(
    format = c(
      rep(c("CSV", "Parquet", "Parquet", "Parquet partitionné"), each = 2),
      "CSV"
    ),
    cols = c(
      rep(c("Toutes", "Sous-ensemble"), times = 4),
      "Toutes"
    ),
    cols_number = c(
      rep(c(dimensions$complete[2], dimensions$sample[2]), 4),
      dimensions$complete[2]
    ),
    rows_number = c(
      rep(c(dimensions$complete[1], dimensions$sample[1]), 3),
      rep(dimensions$sample[1], 2),
      dimensions$complete[1]
    ),
    disk = c(
      rep(c(
        disk_usage$sample_csv, 
        disk_usage$sample_parquet, 
        disk_usage$full_parquet
      ), each = 2),
      rep(disk_usage$full_parquet, 2),
      disk_usage$full_csv
    ),
    import = as.numeric(
      c(
        timings$csv_sample,
        timings$csv_sample_subset,
        timings$parquet_sample,
        timings$parquet_sample_subset,
        timings$parquet_full,
        timings$parquet_full_subset,
        timings$parquet_partitioned_full,
        timings$parquet_partitioned_full_subset,
        timings$csv_full
      ),
      units = "secs"),
    sample = c(
      rep(TRUE, 4),
      rep(FALSE, 2),
      rep(TRUE, 2),
      FALSE
    )
  )
  
  results_df <- results_df %>%
    mutate(
      partitioned = case_when(
        (sample) & (grepl("partitionné", format)) ~ "✅️",
        (sample) & (format != "CSV" ) ~ "❌️",
        TRUE ~ ""
      )
    ) %>%
    mutate(
      format = gsub(" partitionné", "", format)
    )
  
  results_df <- results_df %>%
    mutate(disk = fs::fs_bytes(disk)) %>%
    mutate(disk_bar = as.numeric(disk)) %>%
    mutate(
      import_bar = import,
      cols_bar = cols_number,
      rows_number_bar = rows_number
    ) %>%
    select(order(colnames(.))) %>%
    mutate(cols = glue("_{cols}_ (**{cols_number}** colonnes)")) %>%
    mutate(emo = if_else(format == "Parquet", "🐎", "🐢")) %>%
    select(
      emo, format, partitioned, cols, sample,
      starts_with("rows_"),
      starts_with("cols_"),
      everything()
    ) %>%
    arrange(desc(sample), desc(cols), format)
  
  return(results_df)
  
  
}



create_report_table <- function(df){
  
  tab <- gt(results_df) %>%
    cols_hide(
      columns = c("sample", "cols_bar", "cols_number", "rows_number", "rows_number_bar")
    ) %>%
    fmt_markdown(columns = c('sample', 'cols')) %>%
    fmt_number(columns = "import", decimals = 2) %>%
    gtExtras::gt_plt_bar(
      column = disk_bar,
      color = "#ff562c"
    ) %>%
    gtExtras::gt_plt_bar(
      column = cols_bar,
      color = "#ff562c"
    ) %>%
    gtExtras::gt_plt_bar(
      column = rows_number_bar,
      color = "#ff562c"
    ) %>%
    gtExtras::gt_plt_bar(
      column = import_bar,
      color = "#ff562c"
    ) %>%
    tab_spanner(
      label = md("**Configuration**"),
      columns = c("emo", "format", "partitioned", "cols", "sample", starts_with("cols_"), starts_with("rows_"))
    ) %>%
    tab_spanner(label = md("**Taille sur disque**<br>_(MiB ou GiB)_"), columns = starts_with("disk")) %>%
    tab_spanner(label = md("**Vitesse à l'import**<br>_(secondes)_"), columns = starts_with("import")) %>%
    cols_label(
      emo = "",
      format = "*Format du fichier*",
      partitioned = "*Partitionné?*",
      cols = "*Colonnes*",
      sample = "*Echantillon de données ?*",
      disk = "",
      import = "",
      ends_with("_bar") ~ "",
      .fn = md
    ) %>%
    tab_row_group(
      label = md(
        glue("**Seulement le Centre Val de Loire ({nrows} observations)**", nrows = format(dims_sample[1], big.mark=" "))
      ),
      rows = (sample == TRUE),
      id = "sample"
    ) %>%
    tab_row_group(
      label = md(
        glue("**Ensemble des données ({nrows} observations)**", nrows = format(dims_complete[1], big.mark=" "))
      ),
      rows = (sample == FALSE),
      id = "full"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#4758AB"),
        cell_text(color = "white")
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top"),
          color = "#4758AB",
          weight = px(2)
        )
      ),
      locations = list(
        cells_body(
          rows = (format == "CSV" & cols != " _Toutes_ (**88** colonnes)")
        )
      )
    ) %>%
    tab_footnote(
      footnote = md("Poids de l'ensemble des données, y compris régions différentes"),
      locations = cells_body(
        rows = (format == "Parquet" & partitioned == "✅️"),
        columns = "disk"
      )
    )
  
  return(tab)
  
}
