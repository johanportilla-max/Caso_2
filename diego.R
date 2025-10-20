#diegonsi
library(readr)
library(dplyr)
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
p_ingreso <- ggplot(lending_base, aes(x = ingreso)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histograma del ingreso", x = "Ingreso", y = "Frecuencia") +
  theme_minimal()
print(p_ingreso)

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

