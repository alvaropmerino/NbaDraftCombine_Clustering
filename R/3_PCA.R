# Librerías necesarias
library(ggplot2)
library(factoextra)  # Para visualizaciones de PCA
library(dplyr)

# Paso 1: Selección de variables numéricas
# Asegúrate de que df_num contiene solo variables numéricas relevantes para el PCA
# Ejemplo: si ya limpiaste el dataset como hablamos
df_pca <- df %>% 
  select(HGT, WGT, BMI, BF, WNGSPN_HGT, MAXVERT, STNDVERT, REACH_MAXVERT, BENCH, SPRINT, LANE)

# Paso 2: Estandarización (media 0, sd 1)
df_scaled <- scale(df_pca)

# Paso 3: Aplicar PCA
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)



# Paso 4: Varianza explicada (Scree plot)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
ggsave("figures/scree_plot.png", width = 6, height = 4, dpi = 300)

# Paso 5: Biplot (2 primeros componentes) hacemos tambien dispersion entre dif componentes 
fviz_pca_biplot(pca_result,
                repel = TRUE,
                col.var = "steelblue",
                col.ind = "gray30")
ggsave("figures/plot_zoom.png", width = 6, height = 4, dpi = 300)


# Paso 6: Revisar resultados
# Varianza explicada por componente
summary(pca_result)

#------- ## AUTOVALORES

# Obtener varianza explicada de cada componente
varianzas <- (pca_result$sdev)^2  # sdev = desviaciones estándar → (sdev)^2 = varianza = autovalor

# Mostrar los autovalores
autovalores <- round(varianzas, 3)
names(autovalores) <- paste0("PC", 1:length(autovalores))
print(autovalores)

# Si quieres visualizarlos como una tabla bonita
autovalores_df <- data.frame(
  Componente = paste0("PC", 1:length(autovalores)),
  Autovalor = autovalores
)
print(autovalores_df)

# Cargas de las variables (loading matrix)
loadings <- pca_result$rotation
round(loadings[, 1:4], 3)  #SOLO los 4 primeros componentes
#Pa ver la ecuacion
for (i in 1:4) {
  cat(paste0("PC", i, " = "))
  cat(paste0(round(loadings[, i], 3), "·", rownames(loadings), collapse = " + "))
  cat("\n\n")
}

# Scores (jugadores proyectados)
pca_result$x  # Matriz de individuos (puedes guardarla para clustering luego)

library(ggplot2)
library(dplyr)
library(patchwork)  # Para combinar gráficos

# Extraer los scores del PCA
pca_scores <- as.data.frame(pca_result$x)

# Añadir la variable de posición al dataframe de los componentes
pca_scores$Posicion_Principal <- df$Posicion_Principal

# Gráfico PC1 vs PC2
p1 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Posicion_Principal)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(title = "PC1 vs PC2", x = "Componente Principal 1", y = "Componente Principal 2") +
  theme_minimal()

# Gráfico PC1 vs PC3
p2 <- ggplot(pca_scores, aes(x = PC1, y = PC3, color = Posicion_Principal)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(title = "PC1 vs PC3", x = "Componente Principal 1", y = "Componente Principal 3") +
  theme_minimal()

# Gráfico PC2 vs PC3
p3 <- ggplot(pca_scores, aes(x = PC2, y = PC3, color = Posicion_Principal)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(title = "PC2 vs PC3", x = "Componente Principal 2", y = "Componente Principal 3") +
  theme_minimal()

# Mostrar los tres gráficos juntos en una sola visualización
p1

# Guardar los gráficos por separado si lo necesitas
ggsave("figures/PC1_PC2.png", p1, width = 6, height = 4, dpi = 300)
ggsave("figures/PC1_PC3.png", p2, width = 6, height = 4, dpi = 300)
ggsave("figures/PC2_PC3.png", p3, width = 6, height = 4, dpi = 300)


# Exportar si lo deseas
scores_pca <- as.data.frame(pca_result$x)
write.csv(scores_pca, "scores_PCA.csv", row.names = FALSE)
