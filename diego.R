#diegonsi
library(readr)
library(dplyr)
library(tidyr)

library(stringr)
library(ggplot2)


lending_base <- read_csv("LC_loans_granting_model_dataset.csv")

# 2) Seleccionar y renombrar variables
lending_base <- lending_base %>%
  select(revenue, dti_n, loan_amnt, fico_n, experience_c, emp_length, Default) %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    experiencia_lc = experience_c,
    anos_empleo_raw = emp_length,
    estado_pago_raw = Default
  )

# Convertir 'emp_length' a número (maneja casos como "10+ years", "less than 1 year")
lending_base <- lending_base %>%
  mutate(
    anos_empleo = case_when(
      str_detect(tolower(as.character(anos_empleo_raw)), "less") ~ 0,
      TRUE ~ as.numeric(parse_number(as.character(anos_empleo_raw)))
    )
  )

#  Re-codificar la variable objetivo (Default) con etiquetas claras
lending_base <- lending_base %>%
  mutate(
    estado_pago = case_when(
      estado_pago_raw %in% c(0, "0") ~ "Paga",
      estado_pago_raw %in% c(1, "1") ~ "No_paga",
      TRUE ~ NA_character_
    ),
    estado_pago = factor(estado_pago, levels = c("Paga", "No_paga"))
  )

#  Eliminar filas con valores NA
lending_base <- lending_base %>%
  drop_na(ingreso, relacion_deuda_ingreso, monto_prestamo,
          puntaje_fico, experiencia_lc, anos_empleo, estado_pago)

cat("Filas después de limpiar NA:", nrow(lending_base), "\n")

#sumariaza
desc_var <- lending_base %>%
  summarise(
    n = n(),
    ingreso_mean = mean(ingreso),
    ingreso_sd   = sd(ingreso),
    ingreso_med  = median(ingreso),
    ingreso_min  = min(ingreso),
    ingreso_max  = max(ingreso),
    
    dti_mean = mean(relacion_deuda_ingreso),
    dti_sd   = sd(relacion_deuda_ingreso),
    
    monto_mean = mean(monto_prestamo),
    monto_sd   = sd(monto_prestamo),
    
    fico_mean = mean(puntaje_fico),
    fico_sd   = sd(puntaje_fico),
    
    exp_mean = mean(experiencia_lc),
    exp_sd   = sd(experiencia_lc),
    
    anos_mean = mean(anos_empleo),
    anos_sd   = sd(anos_empleo)
  )

desc_var_round <- desc_var %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

print("Estadísticos generales:")
print(desc_var_round)

# Estadísticas por grupo (Paga / No_paga)
desc_por_estado <- lending_base %>%
  group_by(estado_pago) %>%
  summarise(
    n = n(),
    ingreso_mean = mean(ingreso),
    ingreso_med  = median(ingreso),
    dti_mean = mean(relacion_deuda_ingreso),
    monto_mean = mean(monto_prestamo),
    fico_mean = mean(puntaje_fico),
    anos_mean = mean(anos_empleo)
  )

desc_por_estado_round <- desc_por_estado %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

print("Estadísticos por estado de pago:")
print(desc_por_estado_round)

# Tablas de frecuencia
tabla_estado <- table(lending_base$estado_pago)
prop_estado  <- prop.table(tabla_estado) * 100
print("Tabla de frecuencia - estado_pago:")
print(tabla_estado)
print("Porcentajes:")
print(round(prop_estado, 2))



# Histograma del ingreso
library(scales)
# Histograma estético del ingreso (USD) sin valores extremos
library(scales)

ggplot(lending_base %>% filter(ingreso <= 300000), aes(x = ingreso)) +
  geom_histogram(
    bins = 40,
    fill = "#26A69A",     # verde-azulado
    color = "white",
    alpha = 0.9
  ) +
  scale_x_continuous(
    labels = dollar_format(prefix = "$", big.mark = ",", decimals = 0),
    breaks = seq(0, 300000, 50000)
  ) +
  labs(
    title = "Distribución del ingreso (sin valores extremos)",
    subtitle = "La mayoría de los clientes tienen ingresos menores a $200,000 USD",
    x = "Ingreso del solicitante (USD)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#004D40", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90")
  )

# Boxplot del puntaje FICO por estado de pago
p_fico_box <- ggplot(lending_base, aes(x = estado_pago, y = puntaje_fico, fill = estado_pago)) +
  geom_boxplot() +
  labs(title = "Puntaje FICO por estado de pago", x = "Estado de pago", y = "Puntaje FICO") +
  theme_minimal()
print(p_fico_box)

# Dispersión DTI vs Monto del préstamo
p_dti_vs_monto <- ggplot(lending_base, aes(x = relacion_deuda_ingreso, y = monto_prestamo, color = estado_pago)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relación deuda/ingreso vs Monto del préstamo", x = "Relación deuda/ingreso", y = "Monto del préstamo") +
  theme_minimal()
print(p_dti_vs_monto)

# Matriz de correlación
num_vars <- lending_base %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico, experiencia_lc, anos_empleo)
cor_mat <- cor(num_vars, use = "complete.obs")

print("Matriz de correlación (redondeada):")
print(round(cor_mat, 2))

# Bins de FICO vs estado de pago
lending_base <- lending_base %>%
  mutate(fico_bin = cut(puntaje_fico, breaks = seq(300, 900, by = 50), right = FALSE))

tabla_fico_estado <- table(lending_base$fico_bin, lending_base$estado_pago)
print("Tabla cruzada de FICO_bin x estado_pago (primeras filas):")
print(head(tabla_fico_estado, 20))

write_csv(desc_var_round, "desc_var.csv")
write_csv(desc_por_estado_round, "desc_por_estado.csv")



#################################################
# Análisis descriptivo — gráficos usando las variables del modelo y el tema original
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(forcats)

# --- Tema idéntico al del modelo ---
tema <- theme_minimal() +
  theme(
    text = element_text(family = "Segoe UI", color = "#2d3748"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#323130"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#605e5c", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#605e5c", hjust = 0),
    panel.grid.major = element_line(color = "#f3f2f1"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title = element_text(face = "bold", color = "#323130"),
    axis.text = element_text(color = "#605e5c"),
    legend.position = "none"
  )

# --- Leer y preparar datos (igual que tu script) ---
lending_base <- read_csv("LC_loans_granting_model_dataset.csv")

# seleccionar y renombrar
lending_base <- lending_base %>%
  select(revenue, dti_n, loan_amnt, fico_n, experience_c, emp_length, Default) %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    experiencia_lc = experience_c,
    anos_empleo_raw = emp_length,
    estado_pago_raw = Default
  )

# convertir emp_length a número (mismo enfoque)
lending_base <- lending_base %>%
  mutate(
    anos_empleo = case_when(
      str_detect(anos_empleo_raw, "10") ~ 10,
      str_detect(anos_empleo_raw, "9")  ~ 9,
      str_detect(anos_empleo_raw, "8")  ~ 8,
      str_detect(anos_empleo_raw, "7")  ~ 7,
      str_detect(anos_empleo_raw, "6")  ~ 6,
      str_detect(anos_empleo_raw, "5")  ~ 5,
      str_detect(anos_empleo_raw, "4")  ~ 4,
      str_detect(anos_empleo_raw, "3")  ~ 3,
      str_detect(anos_empleo_raw, "2")  ~ 2,
      str_detect(anos_empleo_raw, "1")  ~ 1,
      str_detect(tolower(as.character(anos_empleo_raw)), "less") ~ 0,
      TRUE ~ NA_real_
    )
  )

# recodificar estado_pago igual que tu modelo
lending_base <- lending_base %>%
  mutate(
    estado_pago = as.factor(estado_pago_raw),
    estado_pago = forcats::fct_recode(estado_pago,
                                      "Paga" = "0",
                                      "No_paga" = "1")
  )

# eliminar filas con NA en las variables que usamos
lending_base <- lending_base %>%
  drop_na(ingreso, relacion_deuda_ingreso, monto_prestamo,
          puntaje_fico, experiencia_lc, anos_empleo, estado_pago)

cat("Filas después de limpiar NA:", nrow(lending_base), "\n")

# --- Gráficos (con el tema 'tema' y variables del modelo) ---

# 1) Histograma del ingreso (sin valores extremos)
ggplot(lending_base %>% filter(ingreso <= 300000), aes(x = ingreso)) +
  geom_histogram(bins = 40, fill = "#26A69A", color = "white", alpha = 0.9) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",", decimals = 0),
                     breaks = seq(0, 300000, 50000)) +
  labs(title = "Distribución del ingreso (sin valores extremos)",
       subtitle = "Mostrando clientes con ingreso <= $300,000",
       x = "Ingreso (USD)", y = "Frecuencia") +
  tema

# 2) Boxplot: Puntaje FICO por estado de pago
ggplot(lending_base, aes(x = estado_pago, y = puntaje_fico, fill = estado_pago)) +
  geom_boxplot() +
  labs(title = "Puntaje FICO por estado de pago", x = "Estado de pago", y = "Puntaje FICO") +
  tema +
  theme(legend.position = "right")  # mostrar leyenda si la quieres

# 3) Scatter: DTI vs Monto del préstamo (coloreado por estado)
ggplot(lending_base, aes(x = relacion_deuda_ingreso, y = monto_prestamo, color = estado_pago)) +
  geom_point(alpha = 0.4) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Relación deuda/ingreso vs Monto del préstamo",
       x = "Relación deuda/ingreso (DTI)", y = "Monto del préstamo (USD)") +
  tema +
  theme(legend.position = "right")

# 4) Boxplot: Monto del préstamo por estado de pago (útil para comparar montos)
ggplot(lending_base, aes(x = estado_pago, y = monto_prestamo, fill = estado_pago)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Monto del préstamo por estado de pago", x = "Estado de pago", y = "Monto (USD)") +
  tema +
  theme(legend.position = "none")

# 5) Violín / densidad de años de empleo por estado de pago (ver distribución)
ggplot(lending_base, aes(x = estado_pago, y = anos_empleo, fill = estado_pago)) +
  geom_violin(trim = TRUE, alpha = 0.8) +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  labs(title = "Años de empleo por estado de pago", x = "Estado de pago", y = "Años de empleo") +
  tema +
  theme(legend.position = "none")

# 6) Scatter simple: Ingreso vs Monto del préstamo (para ver relación)
ggplot(lending_base, aes(x = ingreso, y = monto_prestamo, color = estado_pago)) +
  geom_point(alpha = 0.35) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Ingreso vs Monto del préstamo", x = "Ingreso (USD)", y = "Monto (USD)") +
  tema +
  theme(legend.position = "right")

# 7) Matriz de correlación (imprime en consola)
num_vars <- lending_base %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico, experiencia_lc, anos_empleo)
cor_mat <- cor(num_vars, use = "complete.obs")
cat("Matriz de correlación (redondeada):\n")
print(round(cor_mat, 2))

