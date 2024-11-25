modelisation_recensement <- function(df){
  
  data_modelisation <- df %>%
    filter(SURF != "Z") %>%
    mutate(SURF = factor(SURF, ordered = TRUE)) %>%
    filter(between(AGED, 40, 60)) %>%
    sample_n(1000)
  
  
  model <- MASS::polr(SURF ~ factor(COUPLE) + factor(TP), data_modelisation)
  
  return(model)
  
}

