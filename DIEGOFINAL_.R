## diego final - con tema y tablas kable

library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)
library(ggthemes)
library(lubridate)
library(kableExtra)
library(knitr)
library(ggcorrplot)
library(scales)

# ==========================================
# Tema personalizado
# ==========================================
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

# ==========================================
# BASE DE DATOS PRINCIPAL (actualizada)
# ==========================================

lending_raw <- read_csv("LC_loans_granting_model_dataset.csv", guess_max = 20000)

lending_base <- lending_raw %>%
  select(revenue, dti_n, loan_amnt, fico_n, Default, purpose, issue_d) %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    estado_pago = Default,
    proposito = purpose
  ) %>%
  mutate(
    fecha_emision = parse_date_time(issue_d, orders = "b-Y", locale = "en_US"),
    Año = year(fecha_emision),
    proposito = as.factor(proposito),
    proposito_agrupado = fct_collapse(
      proposito,
      Consolidacion = c("debt_consolidation", "credit_card"),
      Casa_Vehiculo = c("home_improvement", "major_purchase", "car", "house"),
      Negocio_Estudio = c("small_business", "educational")
    ),
    proposito_agrupado = fct_other(
      proposito_agrupado,
      keep = c("Consolidacion", "Casa_Vehiculo", "Negocio_Estudio"),
      other_level = "Otros"
    ),
    estado_pago = fct_recode(as.factor(estado_pago), "Paga" = "0", "No_paga" = "1")
  ) %>%
  select(-proposito)

# Filtro de outliers y eliminación de NAs
lending_base <- lending_base %>%
  filter(ingreso <= 250000, relacion_deuda_ingreso <= 50) %>%
  drop_na()

# ==========================================
# RESUMEN GENERAL
# ==========================================

resumen_general <- lending_base %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico) %>%
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

# Tabla resumen con estilo solicitado
tabla_resumen = resumen_general %>%
  kable("html", caption = "Resumen de variables principales",
        col.names = c("Variable", "N", "Media", "Mediana", "Desviación estándar", "Mínimo", "Máximo"),
        align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black") %>%
  row_spec(0, background = "skyblue", color = "black") %>%
  footnote(general = "Datos de préstamos Lending Club",
           number = c("Montos en dólares", "Variables numéricas seleccionadas"),
           alphabet = c("Valores procesados y filtrados."),
           symbol = c("Tabla resumen de variables."))

tabla_resumen

# ==========================================
# TABLA ESTADO DE PAGO
# ==========================================

tabla_por_estado <- lending_base %>%
  group_by(estado_pago) %>%
  summarise(
    n = n(),
    tasa_default = mean(if_else(estado_pago == "No_paga", 1, 0)),
    ingreso_med = median(ingreso, na.rm = TRUE),
    dti_med = median(relacion_deuda_ingreso, na.rm = TRUE),
    monto_med = median(monto_prestamo, na.rm = TRUE),
    fico_med = median(puntaje_fico, na.rm = TRUE)
  )

# Tabla por estado con estilo kable
tabla_por_estado_kable <- tabla_por_estado %>%
  kable("html", caption = "Resumen por estado de pago",
        col.names = c("Estado pago", "N", "Tasa default", "Ingreso mediano", "DTI mediano", "Monto mediano", "FICO mediano"),
        digits = 2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black") %>%
  row_spec(0, background = "skyblue", color = "black") %>%
  footnote(general = "Resumen agrupado por estado de pago",
           number = c("Tasa default = proporción de No_paga"))

print(tabla_por_estado_kable)

# Frecuencias estado_pago en formato tabla kable
tabla_estado <- as.data.frame(table(lending_base$estado_pago))
colnames(tabla_estado) <- c("estado_pago", "n")
tabla_estado$porcentaje <- round(prop.table(tabla_estado$n) * 100, 2)

tabla_estado_kable <- tabla_estado %>%
  kable("html", caption = "Frecuencia estado de pago",
        col.names = c("Estado pago", "N", "Porcentaje (%)"), align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black") %>%
  row_spec(0, background = "skyblue", color = "black")

print(tabla_estado_kable)

# ==========================================
# GRÁFICOS (aplicando tema personalizado)
# ==========================================

# Boxplot FICO por estado
p_fico_box <- ggplot(lending_base, aes(x = estado_pago, y = puntaje_fico, fill = estado_pago)) +
  geom_boxplot() +
  labs(title = "Puntaje FICO por estado de pago", x = "Estado de pago", y = "Puntaje FICO") +
  tema
print(p_fico_box)

# Ingreso vs Monto préstamo

# 1) Resumen por deciles de ingreso
# Resumen por deciles de ingreso
summ_dec <- lending_base %>%
  filter(!is.na(ingreso), !is.na(monto_prestamo), !is.na(estado_pago)) %>%
  mutate(decil = ntile(ingreso, 10)) %>%
  group_by(decil) %>%
  summarise(
    ingreso_med = median(ingreso),
    monto_med = median(monto_prestamo),
    tasa_no_paga = mean(estado_pago == "No_paga"),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(decil)

# Tabla resumida con estilo kable
tabla_summ_dec <- summ_dec %>%
  mutate(
    ingreso_med = scales::dollar(ingreso_med),
    monto_med = scales::dollar(monto_med),
    tasa_no_paga = scales::percent(tasa_no_paga, accuracy = 0.1)
  ) %>%
  rename(
    Decil = decil,
    `Ingreso mediano` = ingreso_med,
    `Monto mediano` = monto_med,
    `Tasa No_paga` = tasa_no_paga,
    N = n
  )

tabla_summ_dec %>%
  kable("html", caption = "Resumen por decil de ingreso",
        align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black") %>%
  row_spec(0, background = "skyblue", color = "black") %>%
  footnote(general = "Resumen agregado por decil",
           number = c("Ingreso y monto en USD", "Tasa No_paga en %"))

# Gráfico 1: Monto mediano por decil (línea + puntos) con tema
p_monto <- ggplot(summ_dec, aes(x = ingreso_med, y = monto_med)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Monto mediano del préstamo por decil de ingreso",
    subtitle = "Cada punto representa la mediana del decil de ingreso",
    x = "Ingreso (mediana del decil)",
    y = "Monto mediano (USD)"
  ) +
  tema +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_monto)

# Gráfico 2: Tasa de default por decil (barras) con tema
summ_dec <- summ_dec %>% mutate(label_ing = scales::dollar(ingreso_med))

p_tasa <- ggplot(summ_dec, aes(x = factor(decil), y = tasa_no_paga)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(tasa_no_paga, accuracy = 0.1)),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(summ_dec$tasa_no_paga, na.rm = TRUE) * 1.15)) +
  scale_x_discrete(labels = summ_dec$label_ing) +
  labs(
    title = "Tasa de default (No_paga) por decil de ingreso",
    subtitle = "Eje X muestra el ingreso mediano del decil",
    x = "Ingreso mediano por decil",
    y = "Tasa No_paga"
  ) +
  tema +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")

print(p_tasa)

# Matriz de correlación
num_vars <- lending_base %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico)

cor_mat <- cor(num_vars, use = "complete.obs")
cor_mat_round <- round(cor_mat, 2)

print("Matriz de correlación (redondeada):")
print(cor_mat_round)

# 2) Tabla estilo kable (HTML) similar a tus tablas anteriores
library(tibble)
cor_df <- as.data.frame(cor_mat_round) %>%
  rownames_to_column(var = "Variable")

cor_df %>%
  kable("html", caption = "Matriz de correlación (Pearson)",
        digits = 2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black") %>%
  row_spec(0, background = "skyblue", color = "black") %>%
  footnote(general = "Correlaciones entre variables numéricas (complete.obs)",
           number = c("Valores redondeados a 2 decimales"))


# Histograma Ingreso
p1 <- ggplot(lending_base, aes(x = ingreso)) +
  geom_histogram(aes(y = ..density..), binwidth = 5000, fill = "grey70", color = "black") +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = median(lending_base$ingreso, na.rm = TRUE), color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$") ) +
  labs(title = "Histograma y densidad de ingreso (mediana en rojo)", x = "Ingreso", y = "Densidad") +
  tema
print(p1)

# Histograma FICO
p_fico <- ggplot(lending_base, aes(x = puntaje_fico)) +
  geom_histogram(binwidth = 5, fill = "#3182bd", color = "white") +
  geom_vline(xintercept = median(lending_base$puntaje_fico, na.rm = TRUE),
             color = "#de2d26", linetype = "dashed", size = 0.9) +
  scale_x_continuous(breaks = seq(600, 850, 25), limits = c(600, 850)) +
  labs(title = "Distribución FICO", subtitle = "Histograma (binwidth = 5). Línea punteada = mediana",
       x = "Puntaje FICO", y = "Cuenta") +
  tema
print(p_fico)

# Densidad FICO por estado
col_paga <- "#2b8cbe"
col_nopaga <- "#de2d26"

p_fico_dens <- ggplot(lending_base, aes(x = puntaje_fico, fill = estado_pago, color = estado_pago)) +
  geom_density(alpha = 0.35, size = 0.3) +
  scale_fill_manual(values = c("Paga" = col_paga, "No_paga" = col_nopaga)) +
  scale_color_manual(values = c("Paga" = col_paga, "No_paga" = col_nopaga)) +
  labs(title = "Densidad FICO por estado de pago", subtitle = "Paga vs No_paga", x = "Puntaje FICO", y = "Densidad") +
  tema
print(p_fico_dens)

# Violin plot ingreso
p_violin_ingreso <- ggplot(lending_base, aes(x = estado_pago, y = ingreso, fill = estado_pago)) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  scale_fill_manual(values = c("Paga" = "#1b9e77", "No_paga" = "#d95f02")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Distribución de ingreso por estado de pago",
    subtitle = "Gráfico de violín (densidad y dispersión)",
    x = "Estado de pago",
    y = "Ingreso (USD)"
  ) +
  tema
print(p_violin_ingreso)

# Mapa de calor de correlaciones
ggcorrplot(cor_mat,
           lab = TRUE,
           lab_size = 3,
           colors = c("#de425b", "white", "#2ca25f"),
           title = "Matriz de correlaciones entre variables numéricas",
           outline.col = "white") + tema

# Densidad préstamos ingreso vs monto
p_bin2d <- ggplot(lending_base, aes(x = ingreso, y = monto_prestamo)) +
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
  tema
print(p_bin2d)

# Distribución FICO por propósito
p_fico_proposito <- ggplot(lending_base, aes(x = puntaje_fico, fill = estado_pago)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ proposito_agrupado, ncol = 2) +
  scale_fill_manual(values = c("Paga" = "#4daf4a", "No_paga" = "#e41a1c")) +
  labs(
    title = "Distribución del puntaje FICO por propósito y estado de pago",
    x = "Puntaje FICO",
    y = "Densidad"
  ) +
  tema
print(p_fico_proposito)

# FICO vs DTI
set.seed(123)
sample_plot <- lending_base %>% sample_n(min(40000, nrow(.)))

ggplot(sample_plot, aes(x = puntaje_fico, y = relacion_deuda_ingreso, color = estado_pago)) +
  geom_point(alpha = 0.12, size = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Relación entre puntaje FICO y relación deuda/ingreso",
       
       x = "Puntaje FICO", y = "Relación deuda/ingreso (%)") +
  tema +
  theme(legend.position = "right")+geom_smooth(method = "loess", se = FALSE, size = 1, aes(color = estado_pago))

# Distribución del monto del préstamo
p_monto_hist <- ggplot(lending_base, aes(x = monto_prestamo)) +
  geom_histogram(binwidth = 1000, fill = "#74add1", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = median(monto_prestamo, na.rm = TRUE)), 
             color = "red", linetype = "dashed") +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Distribución del monto del préstamo",
    subtitle = "Línea punteada indica la mediana",
    x = "Monto del préstamo (USD)", y = "Frecuencia"
  ) +
  tema
print(p_monto_hist)


# Distribución de la relación deuda/ingreso (DTI)
p_dti <- ggplot(lending_base, aes(x = relacion_deuda_ingreso)) +
  geom_histogram(binwidth = 1, fill = "#fdae61", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = median(relacion_deuda_ingreso, na.rm = TRUE)),
             color = "red", linetype = "dashed") +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Distribución de la relación deuda/ingreso",
    subtitle = "Distribución concentrada entre 10% y 30%",
    x = "Relación deuda/ingreso (%)", y = "Frecuencia"
  ) +
  tema
print(p_dti)

caracterizacion_clases <- lending_base %>%
  group_by(estado_pago) %>%
  summarise(
    ingreso_prom = mean(ingreso, na.rm = TRUE),
    dti_prom = mean(relacion_deuda_ingreso, na.rm = TRUE),
    monto_prom = mean(monto_prestamo, na.rm = TRUE),
    fico_prom = mean(puntaje_fico, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

caracterizacion_clases %>%
  mutate(
    ingreso_prom = dollar(ingreso_prom),
    monto_prom = dollar(monto_prom),
    dti_prom = percent(dti_prom/100, accuracy = 0.1)
  ) %>%
  kable("html", caption = "Caracterización promedio por clase (Paga vs No_paga)",
        col.names = c("Estado", "Ingreso prom.", "DTI prom.", "Monto prom.", "FICO prom.", "N"),
        align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black")

p_comparativo <- lending_base %>%
  pivot_longer(cols = c(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico),
               names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = estado_pago, y = valor, fill = estado_pago)) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("Paga" = "#1b9e77", "No_paga" = "#d95f02")) +
  labs(
    title = "Comparación de variables numéricas según estado de pago",
    subtitle = "Boxplots por clase (Paga vs No_paga)",
    x = "Estado de pago", y = "Valor"
  ) +
  tema
print(p_comparativo)


tasa_por_proposito <- lending_base %>%
  group_by(proposito_agrupado) %>%
  summarise(
    tasa_no_paga = mean(estado_pago == "No_paga"),
    ingreso_mediano = median(ingreso),
    fico_mediano = median(puntaje_fico),
    .groups = "drop"
  )

tasa_por_proposito %>%
  mutate(
    tasa_no_paga = percent(tasa_no_paga, accuracy = 0.1),
    ingreso_mediano = dollar(ingreso_mediano)
  ) %>%
  kable("html", caption = "Tasa de default e ingreso mediano por propósito",
        col.names = c("Propósito", "Tasa No_paga", "Ingreso mediano", "FICO mediano"),
        align = "c") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "skyblue", color = "black")
