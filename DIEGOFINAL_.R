##diego final
##tabla resumen inicial
library(dplyr)
library(tidyr)
library(readr)
library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)
library(ggthemes)
library(lubridate)
lending_raw <- read_csv("LC_loans_granting_model_dataset.csv", guess_max = 20000)

lending_baseD<- lending_raw %>%
  select(
    revenue, dti_n, loan_amnt, fico_n, emp_length, Default, 
    purpose   ) %>%
  rename(
    ingreso = revenue, relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt, puntaje_fico = fico_n,
    años_empleo = emp_length, estado_pago = Default,
    proposito = purpose
  ) %>%
  mutate(
    años_empleo = case_when(
      str_detect(años_empleo, "10") ~ 10, str_detect(años_empleo, "9") ~ 9,
      str_detect(años_empleo, "8") ~ 8, str_detect(años_empleo, "7") ~ 7,
      str_detect(años_empleo, "6") ~ 6, str_detect(años_empleo, "5") ~ 5,
      str_detect(años_empleo, "4") ~ 4, str_detect(años_empleo, "3") ~ 3,
      str_detect(años_empleo, "2") ~ 2, str_detect(años_empleo, "1") ~ 1,
      str_detect(años_empleo, "less") ~ 0, TRUE ~ NA_real_),
    años_empleo = as.numeric(años_empleo)
  ) %>%  mutate(
    proposito = as.factor(proposito), 
    proposito_agrupado = fct_collapse(proposito,
                                      Consolidacion = c("debt_consolidation", "credit_card"), 
                                      Casa_Vehiculo = c("home_improvement", "major_purchase", "car", "house"),
                                      Negocio_Estudio = c("small_business", "educational"))) %>%
  mutate(proposito_agrupado = fct_other(proposito_agrupado, 
                                        keep = c("Consolidacion", "Casa_Vehiculo", "Negocio_Estudio"),
                                        other_level = "Otros")) %>%  mutate(
                                          estado_pago = fct_recode(as.factor(estado_pago), "Paga" = "0", "No_paga" = "1")
                                        ) %>%select(-proposito)


# Filtro de Outliers (Límites: ingreso <= 250k, DTI <= 50)
lending_baseD <- lending_baseD  %>%
  filter(ingreso <= 250000) %>% 
  filter(relacion_deuda_ingreso <= 50) 

# Eliminar NAs
lending_base_final <- na.omit(lending_baseD)

##resumen
resumen_general <- lending_base_final %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico, años_empleo) %>%
  summarise_all(list(
    n = ~sum(!is.na(.)),
    media = ~mean(., na.rm = TRUE),
    mediana = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    minimo = ~min(., na.rm = TRUE),
    maximo = ~max(., na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(n|media|mediana|sd|minimo|maximo)$"
  )

print(resumen_general)
###############################################
## tabla estado de pago

tabla_por_estado <- lending_base_final %>%
  group_by(estado_pago) %>%
  summarise(
    n = n(),
    tasa_default = mean(if_else(estado_pago == "No_paga", 1, 0)),
    ingreso_med = median(ingreso, na.rm = TRUE),
    dti_med = median(relacion_deuda_ingreso, na.rm = TRUE),
    monto_med = median(monto_prestamo, na.rm = TRUE),
    fico_med = median(puntaje_fico, na.rm = TRUE)
  )

print(tabla_por_estado)

# Tablas de frecuencia
tabla_estado <- table(lending_base_final$estado_pago)
prop_estado  <- prop.table(tabla_estado) * 100
print("Tabla de frecuencia - estado_pago:")
print(tabla_estado)
print("Porcentajes:")
print(round(prop_estado, 2))


# Boxplot del puntajefico por estado de pago
p_fico_box <- ggplot(lending_base, aes(x = estado_pago, y = puntaje_fico, fill = estado_pago)) +
  geom_boxplot() +
  labs(title = "Puntaje FICO por estado de pago", x = "Estado de pago", y = "Puntaje FICO") +
  theme_minimal()
print(p_fico_box)

# 6)  Ingreso vs Monto del préstamo 
ggplot(lending_base, aes(x = ingreso, y = monto_prestamo, color = estado_pago)) +
  geom_point(alpha = 0.35) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Ingreso vs Monto del préstamo", x = "Ingreso (USD)", y = "Monto (USD)") +
  tema +
  theme(legend.position = "right")

# Matriz de correlación
num_vars <- lending_base %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico, experiencia_lc, anos_empleo)
cor_mat <- cor(num_vars, use = "complete.obs")

print("Matriz de correlación (redondeada):")
print(round(cor_mat, 2))


###############################
#histograma
library(ggplot2)
library(scales)

p1 <- ggplot(lending_base_final, aes(x = ingreso)) +
  geom_histogram(aes(y = ..density..), binwidth = 5000, fill = "grey70", color = "black") +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = median(lending_base_final$ingreso, na.rm = TRUE), color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(title = "Histograma y densidad de ingreso (mediana en rojo)", x = "Ingreso", y = "Densidad")

print(p1)

#####distribucion fico 
col_paga <- "#2b8cbe"
col_nopaga <- "#de2d26"

# Histograma Ingreso (estético)
p_ingreso <- ggplot(lending_base_final, aes(x = ingreso)) +
  geom_histogram(binwidth = 5000, fill = "#4c72b0", color = "white") +
  geom_vline(xintercept = median(lending_base_final$ingreso, na.rm = TRUE),
             color = "#f15a24", linetype = "dashed", size = 0.9) +
  annotate("text", x = median(lending_base_final$ingreso, na.rm = TRUE) + 8000, y = Inf,
           label = paste0("Mediana = ", scales::dollar(median(lending_base_final$ingreso, na.rm = TRUE))),
           vjust = 2, color = "#f15a24") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(title = "Distribución de ingreso", subtitle = "Histograma (binwidth = 5k)", x = "Ingreso", y = "Cuenta") +
  theme_minimal(base_size = 13)
print(p_ingreso)

#Histograma FICO (estético)
p_fico <- ggplot(lending_base_final, aes(x = puntaje_fico)) +
  geom_histogram(binwidth = 5, fill = "#3182bd", color = "white") +
  geom_vline(xintercept = median(lending_base_final$puntaje_fico, na.rm = TRUE),
             color = "#de2d26", linetype = "dashed", size = 0.9) +
  annotate("text", x = median(lending_base_final$puntaje_fico, na.rm = TRUE) + 8, y = Inf,
           label = paste0("Mediana = ", median(lending_base_final$puntaje_fico, na.rm = TRUE)),
           vjust = 2, color = "#de2d26") +
  scale_x_continuous(breaks = seq(600, 850, 25), limits = c(600, 850)) +
  labs(title = "Distribución FICO", subtitle = "Histograma (binwidth = 5). Línea punteada = mediana",
       x = "Puntaje FICO", y = "Cuenta") +
  theme_minimal(base_size = 13)
print(p_fico)


# Densidad FICO por estado 
p_fico_dens <- ggplot(lending_base_final, aes(x = puntaje_fico, fill = estado_pago, color = estado_pago)) +
  geom_density(alpha = 0.35, size = 0.3) +
  scale_fill_manual(values = c("Paga" = col_paga, "No_paga" = col_nopaga)) +
  scale_color_manual(values = c("Paga" = col_paga, "No_paga" = col_nopaga)) +
  labs(title = "Densidad FICO por estado de pago", subtitle = "Paga vs No_paga", x = "Puntaje FICO", y = "Densidad") +
  theme_minimal(base_size = 13)
print(p_fico_dens)
ggsave("fico_densidad.png", p_fico_dens, width = 10, height = 4.5, dpi = 300)



# Boxplot FICO por estado (usando muestra) feo 
set.seed(123)
sample_plot <- lending_base_final %>% sample_n(min(50000, nrow(.)))
p_fico_box <- ggplot(sample_plot, aes(x = estado_pago, y = puntaje_fico, fill = estado_pago)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.15, size = 0.6) +
  scale_fill_manual(values = c("Paga" = col_paga, "No_paga" = col_nopaga)) +
  labs(title = "Puntaje FICO por estado de pago", subtitle = "Boxplot + puntos (muestra)", x = "", y = "Puntaje FICO") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
print(p_fico_box)
ggsave("fico_boxplot.png", p_fico_box, width = 8, height = 4.5, dpi = 300)

##Tabla bins FICO con tasa de default (útil para reporte)
tabla_bins <- lending_base_final %>%
  mutate(fico_bin = cut(puntaje_fico, breaks = seq(600, 850, by = 25), right = FALSE)) %>%
  group_by(fico_bin) %>%
  summarise(n = n(), no_paga = sum(estado_pago == "No_paga"), tasa_no_paga = no_paga / n) %>%
  arrange(fico_bin)

print(tabla_bins)

#violin
ggplot(lending_base_final, aes(x = estado_pago, y = ingreso, fill = estado_pago)) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  scale_fill_manual(values = c("Paga" = "#1b9e77", "No_paga" = "#d95f02")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Distribución de ingreso por estado de pago",
    subtitle = "Gráfico de violín (densidad y dispersión)",
    x = "Estado de pago",
    y = "Ingreso (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


##mapa calor correlacion
library(ggcorrplot)

num_vars <- lending_base_final %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico)

cor_mat <- cor(num_vars, use = "complete.obs")

ggcorrplot(cor_mat,
           lab = TRUE, 
           lab_size = 3,
           colors = c("#de425b", "white", "#2ca25f"),
           title = "Matriz de correlaciones entre variables numéricas",
           outline.col = "white")

##mapa calor densidad de prestamos x ingreso
ggplot(lending_base_final, aes(x = ingreso, y = monto_prestamo)) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(
    title = "Densidad de préstamos según ingreso y monto",
    subtitle = "Más oscuro = más concentrado",
    x = "Ingreso",
    y = "Monto del préstamo"
  ) +
  theme_minimal(base_size = 13)

##para que prestamo fico vs densidad
ggplot(lending_base_final, aes(x = puntaje_fico, fill = estado_pago)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ proposito_agrupado, ncol = 2) +
  scale_fill_manual(values = c("Paga" = "#4daf4a", "No_paga" = "#e41a1c")) +
  labs(
    title = "Distribución del puntaje FICO por propósito y estado de pago",
    x = "Puntaje FICO",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12)

#FICO vs DTI
ggplot(lending_base_final, aes(x = puntaje_fico, y = relacion_deuda_ingreso, color = estado_pago)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Paga" = "#1b9e77", "No_paga" = "#d95f02")) +
  labs(
    title = "Relación entre puntaje FICO y relación deuda/ingreso",
    subtitle = "Con línea de tendencia lineal",
    x = "Puntaje FICO",
    y = "Relación deuda/ingreso (%)"
  ) +
  theme_minimal(base_size = 13)









