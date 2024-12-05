
#' Build a sub-sample of a Parquet Dataframe.
#'
#' @param n_samples A number. Size of the sample to keep.
#' @param bucket_input A string. Name of the input bucket on MinIO.
#' @param path_input A string. Name of the path where to read in the bucket on MinIO.
#' @param bucket_output A string. Name of the output bucket on MinIO.
#' @param path_output A string. Name of the path where to write in the bucket on MinIO.
#'
#' @return A string. Indicates if the operation was successful.
#'
#' @examples
#' \dontrun{
#' templateProjetMutualise:::build_sample(n_samples=100000,
#'                                        bucket_input="donnees-insee",
#'                                        path_input="diffusion/RP/2016/individu_reg/individu_reg.parquet",
#'                                        bucket_output="projet-formation",
#'                                        path_output="diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv"
#'                                        )
#'}
#'
build_sample <- function(n_samples=100000, bucket_input, path_input,
                         bucket_output, path_output) {
  df_parquet <-
    aws.s3::s3read_using(
      FUN = arrow::read_parquet,
      object = path_input,
      bucket = bucket_input,
      opts = list("region" = "")
    )

  df_subset <- data.table::setDT(df_parquet) |>
    dplyr::slice_sample(n=n_samples)

  aws.s3::s3write_using(
    df_subset,
    FUN = data.table::fwrite,
    sep = ";",
    object = path_output,
    bucket = bucket_output,
    opts = list("region" = "")
  )
}

#' Import a Dataframe from a given path.
#'
#' @param path A string. Path of the input data.
#'
#' @return A Dataframe.
#' @export
#'
#' @examples
#' url <- "http://minio.lab.sspcloud.fr/projet-formation/diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv"
#' df_sample_rp <- get_data(path=url)
#'
get_data <- function(path) {
  df <- readr::read_csv2(path)
  return(df)
}
