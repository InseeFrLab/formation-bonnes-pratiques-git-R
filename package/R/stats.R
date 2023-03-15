
#' Calcul de statistiques agrégées.
#'
#' @param x A numeric vector.
#' @param statistic A string. The statistic to compute.
#' @param ... Other parameters to pass to the statistic function.
#'
#' @return A number.
#' @export
#'
#' @examples
#' url <- "http://minio.lab.sspcloud.fr/projet-formation/diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv"
#' df_sample_rp <- get_data(path = url)
#' ages_femme <- as.numeric(df_sample_rp$aged[df_sample_rp$sexe == 2])
#' stats_agregees(x = ages_femme, statistic = "moyenne")
#'
stats_agregees <- function(x, statistic = "moyenne",
                           ...) {
  match.arg(statistic,
            c("moyenne",
              "variance",
              "ecart_type")
  )

  switch(statistic,
         moyenne = mean(x, ...),
         variance = var(x, ...),
         ecart_type = sd(x, ...)
  )

}
