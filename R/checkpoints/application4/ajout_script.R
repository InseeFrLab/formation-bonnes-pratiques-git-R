# PART DES SENIORS DANS LA POPULATION ---------------------------

# France geojson
departements <- sf::st_read("data/france.geojson")

# PART DES SENIORS FRANCE ENTIERE =====================================

part_seniors <- open_dataset(
  "RPindividus_partitionne.parquet",
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


# CARTE =====================================

departements_60_plus_sf <- departements %>%
  inner_join(
    part_seniors,
    by = c("INSEE_DEP" = "DEPT")
  )


ggplot(departements_60_plus_sf) +
  geom_sf(aes(fill = pourcentage_60_plus)) + 
  scale_fill_fermenter(n.breaks = 5, palette = "PuBuGn", direction = 1) + 
  theme_void() + 
  labs(
    title = "Part des personnes de plus de 60 ans par département",
    caption = "Source: Insee, Fichiers détails du recensement de la population",
    fill = "Part (%)"
  )
