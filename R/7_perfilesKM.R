# Primero, aseguramos que los clusters estén en tu dataset principal
# Aquí df sería el dataset original limpio (el de NBA Draft Combine que estás usando)

# Añadir el cluster de kmeans al dataset original
df$cluster <- pca_scores$cluster

# Ahora agrupamos y sacamos estadísticas por cluster
library(dplyr)

# Calcular medias por cluster de algunas variables relevantes
perfil_clusters <- df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),  # número de jugadores
    altura_media = mean(HGT, na.rm = TRUE),
    peso_media = mean(WGT, na.rm = TRUE),
    bmi_medio = mean(BMI, na.rm = TRUE),
    grasa_corporal_media = mean(BF, na.rm = TRUE),
    maxvert_medio = mean(MAXVERT, na.rm = TRUE),
    stndvert_medio = mean(STNDVERT, na.rm = TRUE),
    sprint_medio = mean(SPRINT, na.rm = TRUE),
    lane_medio = mean(LANE, na.rm = TRUE),
    bench_medio = mean(BENCH, na.rm = TRUE)
  )

print(perfil_clusters)

table(df$cluster, df$Posicion_Principal)


# Aseguramos que tenemos la variable Posicion_Principal en df
# (df debería ser tu dataset original limpio)

# Cruzar clusters con posiciones principales
tabla_posicion_cluster <- df %>%
  group_by(cluster, Posicion_Principal) %>%
  summarise(n = n()) %>%
  arrange(cluster, desc(n))

print(tabla_posicion_cluster)

# También puedes verlo como tabla de contingencia
table(df$cluster, df$Posicion_Principal)



# Filtrar jugadores que son bases y están en el Cluster 2
bases_cluster2 <- df %>%
  filter(Posicion_Principal == "PG", cluster == 2)

# Ver el número de bases en Cluster 2
nrow(bases_cluster2)

# Mostrar los nombres de los jugadores (o cualquier otra variable que quieras ver)
# Suponiendo que tienes una columna llamada PLAYER
bases_cluster3 %>% select(PLAYER, HGT, WGT, BMI, MAXVERT, STNDVERT, REACH_MAXVERT, SPRINT, LANE, BENCH)

# Bases del Cluster 3
bases_cluster3 <- df %>%
  filter(Posicion_Principal == "PG", cluster == 3)

# Bases del Cluster 2 ya los tienes como 'bases_cluster2'

# Comparar medias de variables relevantes
comparacion_bases <- data.frame(
  Variable = c("HGT", "WGT", "BMI", "MAXVERT", "STNDVERT", "SPRINT", "LANE", "BENCH"),
  Media_Cluster3 = c(
    mean(bases_cluster3$HGT, na.rm = TRUE),
    mean(bases_cluster3$WGT, na.rm = TRUE),
    mean(bases_cluster3$BMI, na.rm = TRUE),
    mean(bases_cluster3$MAXVERT, na.rm = TRUE),
    mean(bases_cluster3$STNDVERT, na.rm = TRUE),
    mean(bases_cluster3$SPRINT, na.rm = TRUE),
    mean(bases_cluster3$LANE, na.rm = TRUE),
    mean(bases_cluster3$BENCH, na.rm = TRUE)
  ),
  Media_Cluster2 = c(
    mean(bases_cluster2$HGT, na.rm = TRUE),
    mean(bases_cluster2$WGT, na.rm = TRUE),
    mean(bases_cluster2$BMI, na.rm = TRUE),
    mean(bases_cluster2$MAXVERT, na.rm = TRUE),
    mean(bases_cluster2$STNDVERT, na.rm = TRUE),
    mean(bases_cluster2$SPRINT, na.rm = TRUE),
    mean(bases_cluster2$LANE, na.rm = TRUE),
    mean(bases_cluster2$BENCH, na.rm = TRUE)
  )
)

print(comparacion_bases)

# Filtrar pívots en el Cluster 1
pivots_cluster1 <- df %>%
  filter(Posicion_Principal == "C", cluster == 1)

# Filtrar pívots en el Cluster 2
pivots_cluster2 <- df %>%
  filter(Posicion_Principal == "C", cluster == 2)

# Ver cuántos hay en cada grupo
nrow(pivots_cluster1)
nrow(pivots_cluster2)

# Mostrar los nombres y algunas variables (opcional)
pivots_cluster2 %>% select(PLAYER, HGT, WGT, BMI, MAXVERT, STNDVERT, REACH_MAXVERT, SPRINT, LANE, BENCH)

# Comparar medias de variables relevantes
comparacion_pivots <- data.frame(
  Variable = c("HGT", "WGT", "BMI", "MAXVERT", "STNDVERT", "SPRINT", "LANE", "BENCH"),
  Media_Cluster1 = c(
    mean(pivots_cluster1$HGT, na.rm = TRUE),
    mean(pivots_cluster1$WGT, na.rm = TRUE),
    mean(pivots_cluster1$BMI, na.rm = TRUE),
    mean(pivots_cluster1$MAXVERT, na.rm = TRUE),
    mean(pivots_cluster1$STNDVERT, na.rm = TRUE),
    mean(pivots_cluster1$SPRINT, na.rm = TRUE),
    mean(pivots_cluster1$LANE, na.rm = TRUE),
    mean(pivots_cluster1$BENCH, na.rm = TRUE)
  ),
  Media_Cluster2 = c(
    mean(pivots_cluster2$HGT, na.rm = TRUE),
    mean(pivots_cluster2$WGT, na.rm = TRUE),
    mean(pivots_cluster2$BMI, na.rm = TRUE),
    mean(pivots_cluster2$MAXVERT, na.rm = TRUE),
    mean(pivots_cluster2$STNDVERT, na.rm = TRUE),
    mean(pivots_cluster2$SPRINT, na.rm = TRUE),
    mean(pivots_cluster2$LANE, na.rm = TRUE),
    mean(pivots_cluster2$BENCH, na.rm = TRUE)
  )
)

print(comparacion_pivots)


