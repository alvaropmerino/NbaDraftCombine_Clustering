# Librerías necesarias
library(factoextra)  # Para clustering y visualización
library(dplyr)

# --- Paso 1: Seleccionar componentes principales (PC1-PC4) ---
# Usamos solo los componentes (sin la columna 'cluster' añadida)
pca_scores <- as.data.frame(pca_result$x[, 1:4])

# --- Paso 2: Determinar el número óptimo de clusters ---

# Método del codo (WSS - within cluster sum of squares)
fviz_nbclust(pca_scores, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Puedes ajustar el número según lo que veas
  labs(subtitle = "Método del Codo (Elbow method) para determinar K")

# Método de la Silueta
fviz_nbclust(pca_scores, kmeans, method = "silhouette") +
  labs(subtitle = "Método de la Silueta para determinar K")

# --- Paso 3: Aplicar K-means clustering ---

set.seed(123)  # Para hacer resultados reproducibles
km_result <- kmeans(pca_scores, centers = 3, nstart = 25)  # Ajusta 'centers' después según resultado

# --- Paso 4: Visualizar los clusters ---

# Ahora SÓLO usamos las 4 PCs (sin problema de datos no numéricos)
fviz_cluster(km_result, data = pca_scores, ellipse.type = "norm",
             geom = "point", stand = FALSE,
             main = "Clustering de jugadores basado en PCA") +
  theme_minimal()

# --- Paso 5: (Opcional) Ver detalles de clustering ---

# Ver resumen de clustering
print(km_result)

# Añadir los clusters al dataset de scores (para analizar perfiles luego)
pca_scores$cluster <- as.factor(km_result$cluster)

# Ver número de jugadores en cada cluster
table(pca_scores$cluster)



#----------- GRAFICO CON CLUSTERS BIEN SEÑALIZADOS Y sin escalado----------------

library(ggplot2)

# Usamos los scores del PCA
pca_scores <- as.data.frame(pca_result$x[, 1:2])  # Solo PC1 y PC2 para visualización

# Añadimos los clusters desde el resultado de K-means
pca_scores$cluster <- as.factor(km_result$cluster)

# Visualizar con nombres de clusters correctos
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster, shape = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  #stat_ellipse(type = "norm", linetype = 1) +
  labs(
    title = "Distribución de jugadores según los dos primeros componentes principales",
    subtitle = "Colores representan los clusters del análisis de K-means",
    x = paste0("Componente Principal 1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
    y = paste0("Componente Principal 2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +  # Rojo, Verde, Azul
  theme(legend.title = element_blank())
ggsave("figures/clusters_pca.png",width = 6, height = 4, dpi = 300)
