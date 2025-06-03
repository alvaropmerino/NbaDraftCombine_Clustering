library(tidyverse)
library(ggplot2)

NBA_draft_combine <- read.csv("C:/Users/alvar/OneDrive/Documentos/Estadistica y Empresa/cuarto/TFG/NBA_draft_combine.csv", stringsAsFactors = FALSE)

#Importamos el dataset
datos <- NBA_draft_combine

#Cambiamos las variables a su tipo correspondiente 

datos$YEAR <- as.character(datos$YEAR)
datos$WGT <- as.double(datos$WGT )

# Observamos el porcentaje de datos faltantes en cada variable

missing_summary <- sapply(datos, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
cat(missing_summary)

#Eliminamos variables con muchos datos faltantes 

datos <- datos[, !names(datos) %in% c("HANDL", "HANDW", "SHUTTLE", "PAN")]

#Eliminamos observaciones con Datos faltantes

data_limpio <- na.omit(datos)

#Creamos una nueva variable que nos indique la posicion principal del jugador

# Extraer solo la primera posición antes de la "-"
data_limpio <-  data_limpio %>%
  mutate(Posicion_Principal = sub("-.*", "", POS))

# Frecuencias POS

ggplot(data_limpio, aes(x = POS)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Posiciones",
       x = "Posición",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Frecuencias Posicion_Principal

ggplot(data_limpio, aes(x = Posicion_Principal)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribución de Posiciones",
       x = "Posición",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Frecuencias de años

ggplot(data_limpio, aes(x = YEAR)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Jugadores por año",
       x = "Año",
       y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Cambiamos nombre a df para facililitar todo

df <- data_limpio

colnames(df) <- c("YEAR", "PLAYER", "POS", "HGT", "WGT", "BMI", "BF", "WNGSPN",
                     "STNDRCH", "STNDVERT", "MAXVERT", "LANE", "SPRINT", "BENCH",
                     "WNGSPN_HGT", "REACH_VERT", "REACH_MAXVERT", "Posicion_Principal")

#dataset con los numericos
df_num <- df[,4:17]



