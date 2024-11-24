rm(list = ls())
setwd("/home/onyxia/formation-bonnes-pratiques-R")

if (!require('ggplot2')) install.packages('ggplot2')
if (!require('stringr')) install.packages('stringr')
if (!require('dplyr')) install.packages('dplyr')
if (!require('tidyverse')) install.packages('tidyverse')


library(tidyverse)

# j'importe les données avec read_csv2 parce que c'est un csv avec des ; et que read_csv attend comme separateur des , 
df <- readr::read_csv(
  "RPindividus_24.csv", col_select =c(
    "REGION", "AGED", "ANAI", "CATL", "COUPLE",
    "SEXE", "SURF", "TP", "TRANS", "IPONDI"
  )
)


df %>% group_by(AGED) %>% summarise(n = sum(IPONDI))

decennie_a_partir_annee    = function(ANNEE){ return(ANNEE - ANNEE %%
                                                       10) }

ggplot(df) + geom_histogram(aes(x = 5*floor(as.numeric(AGED)/5), weight = IPONDI), stat = "count")

# correction (qu'il faudra retirer)
# ggplot(
#   df %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% filter(sexe==1)
# ) + geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))

# stats trans par statut
df3 = df %>% group_by(COUPLE, TRANS) %>% summarise(x = sum(IPONDI)) %>% group_by(COUPLE) %>% mutate(y = 100*x/sum(x))

library(forcats)
df <- df %>% mutate(SEXE = as.character(SEXE)) %>%
  mutate(SEXE = fct_recode(SEXE, Homme = "1", Femme = "2"))

p <- # part d'homme dans chaque cohort
  df %>% 
  group_by(AGED, SEXE) %>% 
  summarise(SH_sexe = sum(IPONDI)) %>% 
  group_by(AGED) %>% 
  mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% 
  filter(SEXE=="Homme") %>%
  ggplot() + geom_bar(aes(x = AGED, y = SH_sexe), stat="identity") + geom_point(aes(x = AGED, y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))


ggsave("p.png", p)


#fonction de stat agregee
fonction_de_stat_agregee<-function(a,b="moyenne",...){
  if (b=="moyenne"){
    x=mean(a, na.rm = T,...)
  } else if (b=="ecart-type" | b == "sd"){
    x = sd(a, na.rm = T, ...)
  } else if (b=="variance"){
    x = var(a, na.rm = T, ...)
  }
  return(x)
}

fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "ecart-type")
fonction_de_stat_agregee(rnorm(10), "variance")

fonction_de_stat_agregee(df %>% filter(SEXE == "Homme") %>% pull(AGED))
fonction_de_stat_agregee(df %>% filter(SEXE == "Femme") %>% pull(AGED))

api_token <- "trotskitueleski$1917"

# modelisation
# library(MASS)
df3=df%>%filter(SURF!="Z")%>%sample_n(1000)
df3 <- df3 %>% mutate(SURF = factor(SURF, ordered=T))
df3 %>% 
  filter(COUPLE == 2 & between(AGED, 40, 60))
polr(SURF ~ factor(COUPLE) + factor(TP), df3)
