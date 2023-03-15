library(devtools)

# Initialise le package R
# create_package("~/work/templateProjetMutualise")
# use_mit_license()

# Chargement des fonctions exportées du package
load_all()

# Documentation des fonctions
document()
?get_data
?stats_agregees

# Tests CRAN (facultatif)
# check()

# Installation du package
install()
library(templateProjetMutualise)

# Récupération des données
url <- "http://minio.lab.sspcloud.fr/projet-formation/diffusion/bonnes-pratiques-r/rp_2016_individu_sample.csv"
df_sample_rp <- get_data(path = url)

# Calcul de statistiques agrégées
ages_femme <- as.numeric(df_sample_rp$aged[df_sample_rp$sexe == 2])
stats_agregees(x = ages_femme, statistic = "moyenne")
stats_agregees(x = ages_femme, statistic = "ecart_type")
