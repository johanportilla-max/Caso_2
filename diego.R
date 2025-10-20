#diego b
library(tidyverse)
library(ggplot2)

library(knitr)
lending_base <- read.csv("LC_loans_granting_model_dataset.csv", sep = ",", header = TRUE)
glimpse(lending_base)

# Selección de variables utilizadas en el modelo
lending_muestra <- lending_base %>%
  select(estado_pago, ingreso, relacion_deuda_ingreso, monto_prestamo,
         puntaje_fico, experiencia_lc, años_empleo)

# Convertir a factor la variable dependiente si no lo está
lending_muestra$estado_pago <- as.factor(lending_muestra$estado_pago)

# ============================================================
#   TABLAS DESCRIPTIVAS
# ============================================================

# Resumen estadístico de variables numéricas
tabla_resumen <- lending_muestra %>%
  select(-estado_pago) %>%
  describe() %>%
  select(mean, sd, median, min, max, skew, kurtosis)

kable(tabla_resumen, digits = 2, caption = "Resumen descriptivo de variables cuantitativas")

# Frecuencias de la variable dependiente
tabla_estado <- table(lending_muestra$estado_pago)
kable(as.data.frame(tabla_estado), caption = "Distribución de la variable estado_pago")

# ============================================================
#   GRÁFICOS DESCRIPTIVOS
# ============================================================

# Histograma de cada variable numérica
lending_muestra %>%
  select(-estado_pago) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Valor, fill = Variable)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "black") +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribución de las variables numéricas")

# Boxplots por estado de pago
lending_muestra %>%
  pivot_longer(cols = c(ingreso, relacion_deuda_ingreso, monto_prestamo,
                        puntaje_fico, experiencia_lc, años_empleo),
               names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = estado_pago, y = Valor, fill = estado_pago)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Comparación de variables por estado de pago",
       x = "Estado de pago", y = "Valor")


ggplot(lending_muestra, aes(x = estado_pago, fill = estado_pago)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Frecuencia de cada categoría de estado_pago",
       x = "Estado de pago", y = "Número de observaciones")