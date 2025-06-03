# Librerías necesarias
library(ggplot2)
library(corrplot)

# Crear carpeta si no existe
if (!dir.exists("figures")) dir.create("figures")

# ---------- HISTOGRAMAS Y BOXPLOTS INDIVIDUALES ----------

# BMI
ggplot(df, aes(x = BMI)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribución del IMC (BMI)", x = "BMI", y = "Frecuencia")
ggsave("figures/BMI_histograma.png", width = 6, height = 4, dpi = 300)

ggplot(df, aes(y = BMI)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Boxplot del IMC (BMI)")
ggsave("figures/BMI_boxplot.png", width = 6, height = 4, dpi = 300)

# BENCH
ggplot(df, aes(y = BENCH)) +
  geom_boxplot(fill = "purple") +
  theme_minimal() +
  labs(title = "Boxplot de Bench Press", y = "Repeticiones")
ggsave("figures/BENCH_boxplot.png", width = 6, height = 4, dpi = 300)

# BF

ggplot(df, aes(y = BF)) +
  geom_boxplot(fill = "blue") +
  theme_minimal() +
  labs(title = "Boxplot del porcentaje de grasa (BF)")
ggsave("figures/BF_boxplot.png", width = 6, height = 4, dpi = 300)

#SPRINT

ggplot(df, aes(y = SPRINT)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Boxplot del tiempo de sprint (SPRINT)")
ggsave("figures/SPRINT_boxplot.png", width = 6, height = 4, dpi = 300)

# ---------- COMPARACIONES POR POSICIÓN PRINCIPAL ----------

# BENCH por posición
ggplot(df, aes(x = Posicion_Principal, y = BENCH)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Bench Press por Posición", x = "Posición", y = "Repeticiones")
ggsave("figures/BENCH_por_posicion.png", width = 7, height = 4.5, dpi = 300)

# STNDVERT por posición
ggplot(df, aes(x = Posicion_Principal, y = STNDVERT)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Salto Vertical en Parado por Posición", x = "Posición", y = "Altura de salto")
ggsave("figures/STNDVERT_por_posicion.png", width = 7, height = 4.5, dpi = 300)

# BMI por posición
ggplot(df, aes(x = Posicion_Principal, y = BMI)) +
  geom_boxplot(fill = "salmon") +
  theme_minimal() +
  labs(title = "Índice de Masa Corporal por Posición", x = "Posición", y = "BMI")
ggsave("figures/BMI_por_posicion.png", width = 7, height = 4.5, dpi = 300)

# Altura (HGT)
ggplot(df, aes(x = HGT)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribución de Altura (HGT)", x = "Altura (pulgadas)", y = "Frecuencia")
ggsave("figures/HGT_histograma.png", width = 6, height = 4, dpi = 300)

ggplot(df, aes(x = Posicion_Principal, y = HGT)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Altura por Posición", x = "Posición", y = "Altura (pulgadas)")
ggsave("figures/HGT_por_posicion.png", width = 7, height = 4.5, dpi = 300)

# Peso (WGT)
ggplot(df, aes(x = WGT)) +
  geom_histogram(bins = 30, fill = "darkred", color = "white") +
  theme_minimal() +
  labs(title = "Distribución de Peso (WGT)", x = "Peso (libras)", y = "Frecuencia")
ggsave("figures/WGT_histograma.png", width = 6, height = 4, dpi = 300)

ggplot(df, aes(x = Posicion_Principal, y = WGT)) +
  geom_boxplot(fill = "mistyrose") +
  theme_minimal() +
  labs(title = "Peso por Posición", x = "Posición", y = "Peso (libras)")
ggsave("figures/WGT_por_posicion.png", width = 7, height = 4.5, dpi = 300)

# ----------- BAR (ratio envergadura/altura) -----------
ggplot(df, aes(x = WNGSPN_HGT)) +
  geom_histogram(bins = 30, fill = "mediumseagreen", color = "white") +
  theme_minimal() +
  labs(title = "Distribución del Ratio Wingspan/Height (WNGSPN_HGT)", x = "Ratio", y = "Frecuencia")
ggsave("figures/BAR_histograma.png", width = 6, height = 4, dpi = 300)

# ----------- MAXVERT -----------
ggplot(df, aes(x = MAXVERT)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribución del salto vertical con carrera (MAXVERT)", x = "MAXVERT (pulgadas)", y = "Frecuencia")
ggsave("figures/MAXVERT_histograma.png", width = 6, height = 4, dpi = 300)

ggplot(df, aes(y = MAXVERT)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Boxplot del salto vertical con carrera (MAXVERT)", y = "MAXVERT (pulgadas)")
ggsave("figures/MAXVERT_boxplot.png", width = 6, height = 4, dpi = 300)

# ----------- LANE -----------
ggplot(df, aes(x = LANE)) +
  geom_histogram(bins = 30, fill = "orchid", color = "white") +
  theme_minimal() +
  labs(title = "Distribución del tiempo en circuito de agilidad (LANE)", x = "LANE (segundos)", y = "Frecuencia")
ggsave("figures/LANE_histograma.png", width = 6, height = 4, dpi = 300)

ggplot(df, aes(y = LANE)) +
  geom_boxplot(fill = "purple") +
  theme_minimal() +
  labs(title = "Boxplot del tiempo en circuito de agilidad (LANE)", y = "LANE (segundos)")
ggsave("figures/LANE_boxplot.png", width = 6, height = 4, dpi = 300)

# ----------- COMBINACIÓN REACH + MAXVERT (scatter) -----------
ggplot(df, aes(x = REACH_MAXVERT, y = MAXVERT, color = Posicion_Principal)) +
  geom_point(alpha = 0.8, size = 2) +
  theme_minimal() +
  scale_color_manual(values = c("PG" = "#1f78b4", "SG" = "#e31a1c", "SF" = "#33a02c", 
                                "PF" = "yellow", "C" = "#ff7f00")) +
  labs(title = "Relación entre Reach MAXVERT y Salto con carrera",
       x = "Reach MAXVERT (pulgadas)",
       y = "MAXVERT (pulgadas)")
ggsave("figures/REACH_MAXVERT_vs_MAXVERT.png", width = 7.5, height = 4.5, dpi = 300)

# ----------- Boxplot de BF por posición -----------
ggplot(df, aes(x = Posicion_Principal, y = BF)) +
  geom_boxplot(fill = "tomato") +
  theme_minimal() +
  labs(title = "Porcentaje de grasa corporal por posición", x = "Posición", y = "BF")
ggsave("figures/BF_por_posicion.png", width = 6.5, height = 4.5, dpi = 300)


# ---------- MATRIZ DE CORRELACIÓN ----------

library(corrplot)

# Selección de variables numéricas ya limpias
cor_matrix <- cor(df_num)  # Asegúrate de que df_num contiene solo numéricas

# Guardar gráfico
png("figures/Correlacion_variables.png", width = 1000, height = 800)
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 1, addCoef.col = "black")
dev.off()


# ------------ DETECCION DE ATIPICOS ------------

# Seleccionar extremos para algunas variables clave
library(dplyr)

# Top jugadores con mayor BF
bf_extremos <- df %>% 
  arrange(desc(BF)) %>% 
  select(PLAYER, BF) %>% 
  head(5)

# Top jugadores con menor BENCH
bench_bajos <- df %>% 
  arrange(BENCH) %>% 
  select(PLAYER, BENCH) %>% 
  filter(BENCH <= 2)

# Top jugadores con mayor MAXVERT
maxvert_altos <- df %>% 
  arrange(desc(MAXVERT)) %>% 
  select(PLAYER, MAXVERT) %>% 
  head(5)

# Unir todos (ejemplo visual para revisión)
extremos_combinados <- full_join(
  full_join(bf_extremos, bench_bajos, by = "PLAYER"), 
  maxvert_altos, by = "PLAYER"
)

print(extremos_combinados)
