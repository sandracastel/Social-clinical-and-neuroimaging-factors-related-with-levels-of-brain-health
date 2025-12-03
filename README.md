# 1. Instalar paquetes (solo la primera vez que lo uses)
install.packages("haven")       # para leer .dta
install.packages("FactoMineR")  # para MCA
install.packages("factoextra")  # para gráficas (opcional)
install.packages("dplyr")       # para manipular datos
install.packages("readr")       # para guardar CSV
# 2. Cargar paquetes
library(haven)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(readr)

# 3. Cargar la base .dta
# Cambia la ruta si es necesario:
data <- read_excel("C:/Escritorio/Proyecto_Doctorado/Organización de variables/MCA_indice_socioeconomico_violencia.xlsx")

# 4. Seleccionar las variables del MCA
vars_mca <- c("uls_2", "uls_3", "msoc_forb", "msoc_humi", "msoc_prov")

mca_data <- data %>%
  select(all_of(vars_mca))

# 5. Asegurar que todas son factores (categóricas)
mca_data <- mca_data %>%
  mutate(across(everything(), ~ as_factor(.)))  # as_factor de haven respeta etiquetas

# (Si prefieres convertir directo a factor base de R:)
# mca_data <- mca_data %>%
#   mutate(across(everything(), ~ factor(.)))

# 6. Ejecutar el MCA (sin gráficas por ahora)
res_mca <- MCA(mca_data, graph = FALSE)

# 7. Extraer la Dimensión 1 para cada individuo (índice principal)
dim1_scores <- res_mca$ind$coord[, 1]

# Añadir el índice a la base original
data$mca_iso_dim1 <- dim1_scores

# (Opcional) Estandarizar la Dimensión 1 (media 0, DE 1)
data$mca_iso_dim1_z <- as.numeric(scale(data$mca_iso_dim1))

# 8. Guardar la base con el índice en CSV
write_csv(data, "MCA_indice_socioeconomico_violencia_con_MCA.csv")

# 9. (Opcional) Ver un resumen rápido del MCA
summary(res_mca)

# 10. (Opcional) Graficar el biplot de categorías e individuos
fviz_mca_biplot(res_mca, repel = TRUE)

# # ============================================
# 11. Guardar el índice en un objeto
# ============================================
index <- data$mca_iso_dim1   # Dimensión 1 del MCA (índice de aislamiento social)

# ============================================
# 12. Estadísticos descriptivos básicos
# ============================================
summary(index)

# ============================================
# 13. Estadísticos ampliados (Nature Aging)
# ============================================
mean_val   <- mean(index, na.rm = TRUE)
sd_val     <- sd(index, na.rm = TRUE)
median_val <- median(index, na.rm = TRUE)
iqr_val    <- IQR(index, na.rm = TRUE)
range_val  <- range(index, na.rm = TRUE)

percentiles <- quantile(index,
                        probs = c(0.01, 0.05, 0.10, 0.90, 0.95, 0.99),
                        na.rm = TRUE)

# ============================================
# 14. Crear tabla con todos los estadísticos (Tabla S5)
# ============================================
tabla_S5 <- data.frame(
  Statistic = c("Mean", "SD", "Median", "Min", "Max",
                "Q1 (25%)", "Q3 (75%)", "IQR",
                "P1", "P5", "P10", "P90", "P95", "P99"),
  
  Value = c(
    mean_val,
    sd_val,
    median_val,
    range_val[1],     # Min
    range_val[2],     # Max
    quantile(index, 0.25, na.rm = TRUE),  # Q1
    quantile(index, 0.75, na.rm = TRUE),  # Q3
    iqr_val,
    percentiles[1],   # P1
    percentiles[2],   # P5
    percentiles[3],   # P10
    percentiles[4],   # P90
    percentiles[5],   # P95
    percentiles[6]    # P99
  )
)

# Mostrar la tabla en consola
tabla_S5

# ============================================
# 15. Exportar la Tabla S5 a CSV
# ============================================
write_csv(tabla_S5, "tabla_S5_descriptivos_indice_MCA.csv")

# ============================================
# 16. Gráfico de densidad del índice
# ============================================
library(ggplot2)

ggplot(data, aes(x = mca_iso_dim1)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribution of the MCA-derived social isolation index (Dim 1)",
    x = "MCA Dimension 1 score",
    y = "Density"
  )
