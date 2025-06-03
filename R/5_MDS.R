# Librerías necesarias
library(dplyr)
library(ggplot2)

# --- Paso 1: Usar los scores de PCA (4 componentes) ---

# Si no lo tienes aún, creamos los datos
pca_scores <- as.data.frame(pca_result$x[, 1:4])

# --- Paso 2: Calcular matriz de distancias ---

distancia <- dist(pca_scores)  # Calcula distancia euclidiana entre observaciones

# --- Paso 3: Aplicar MDS ---

mds_result <- cmdscale(distancia, k = 2)  # Queremos 2 dimensiones para visualizar

# --- Paso 4: Convertir en dataframe para plotear ---

mds_df <- as.data.frame(mds_result)
colnames(mds_df) <- c("MDS1", "MDS2")
mds_df$cluster <- as.factor(km_result$cluster)  # Añadimos cluster para colorear si quieres

# --- Paso 5: Visualizar ---

ggplot(mds_df, aes(x = MDS1, y = MDS2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Representación mediante MDS de los jugadores",
    subtitle = "Basado en las distancias entre scores de PCA",
    x = "Dimensión 1 (MDS1)",
    y = "Dimensión 2 (MDS2)",
    color = "Cluster"
  )
ggsave("figures/mds_clusters.png",width = 6, height = 4, dpi = 300)