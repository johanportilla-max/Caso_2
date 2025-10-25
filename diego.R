# Análisis descriptivo completo — Lending Club (versión en español)
# Código en estilo y lenguaje similar al que enviaste. 
# Carga de librerías ----------------------------------------------------
library(readr)
library(tidyverse)
library(lubridate)
library(scales)
library(kableExtra)
library(knitr)
library(ggcorrplot)
library(janitor)

options(scipen = 999)

# 1) Leer y preparar la base (igual que tu pipeline inicial) ------------
lending_raw <- read_csv("LC_loans_granting_model_dataset.csv", guess_max = 20000)

lending_base <- lending_raw %>%
  select(revenue, dti_n, loan_amnt, fico_n, emp_length, Default, purpose, issue_d) %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    años_empleo = emp_length,
    estado_pago = Default,
    proposito = purpose,
    issue_d = issue_d
  ) %>%
  # convertir fecha de emisión y extraer año
  mutate(
    fecha_emision = parse_date_time(issue_d, orders = "b-Y", locale = "en_US"),
    Año = year(fecha_emision)
  ) %>%
  # normalizar años de empleo (emp_length) a numérico similar a tu código
  mutate(
    años_empleo = case_when(
      str_detect(años_empleo, "10") ~ 10,
      str_detect(años_empleo, "9") ~ 9,
      str_detect(años_empleo, "8") ~ 8,
      str_detect(años_empleo, "7") ~ 7,
      str_detect(años_empleo, "6") ~ 6,
      str_detect(años_empleo, "5") ~ 5,
      str_detect(años_empleo, "4") ~ 4,
      str_detect(años_empleo, "3") ~ 3,
      str_detect(años_empleo, "2") ~ 2,
      str_detect(años_empleo, "1") ~ 1,
      str_detect(años_empleo, "less") ~ 0,
      TRUE ~ NA_real_
    ),
    años_empleo = as.numeric(años_empleo)
  ) %>%
  # proposito agrupado
  mutate(
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
  select(-proposito, -issue_d)

# Filtro de outliers y NA (igual que pediste)
lending_base <- lending_base %>%
  filter(ingreso <= 250000, relacion_deuda_ingreso <= 50) %>%
  drop_na()

# Tema estético que pediste ------------------------------------------------
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

# Crear carpeta para guardar figuras (opcional)
dir.create("figuras", showWarnings = FALSE)

# 2) Distribución de las variables principales ---------------------------
# Histograma Ingreso
p_ingreso <- ggplot(lending_base, aes(x = ingreso)) +
  geom_histogram(binwidth = 5000, fill = "#4c72b0", color = "white") +
  geom_vline(xintercept = median(lending_base$ingreso, na.rm = TRUE),
             color = "#f15a24", linetype = "dashed", size = 0.9) +
  annotate("text", x = median(lending_base$ingreso, na.rm = TRUE) + 8000, y = Inf,
           label = paste0("Mediana = ", scales::dollar(median(lending_base$ingreso, na.rm = TRUE))),
           vjust = 2, color = "#f15a24") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(title = "Distribución de ingreso", subtitle = "Histograma (binwidth = 5k)",
       x = "Ingreso (USD)", y = "Cuenta") +
  tema
print(p_ingreso)
# Histograma DTI
p_dti <- ggplot(lending_base, aes(x = relacion_deuda_ingreso)) +
  geom_histogram(binwidth = 2.5, fill = "#7fbf7f", color = "white") +
  geom_vline(xintercept = median(lending_base$relacion_deuda_ingreso, na.rm = TRUE),
             color = "#de2d26", linetype = "dashed") +
  labs(title = "Distribución DTI", x = "Relación deuda/ingreso (%)", y = "Cuenta") +
  tema
print(p_dti)
# Histograma Monto préstamo
p_monto <- ggplot(lending_base, aes(x = monto_prestamo)) +
  geom_histogram(binwidth = 1000, fill = "#9fb0d3", color = "white") +
  labs(title = "Distribución monto del préstamo", x = "Monto (USD)", y = "Cuenta") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  tema
print(p_monto)
# Histograma FICO
p_fico <- ggplot(lending_base, aes(x = puntaje_fico)) +
  geom_histogram(binwidth = 5, fill = "#3182bd", color = "white") +
  geom_vline(xintercept = median(lending_base$puntaje_fico, na.rm = TRUE),
             color = "#de2d26", linetype = "dashed") +
  labs(title = "Distribución FICO", x = "Puntaje FICO", y = "Cuenta") +
  tema
print(p_fico)
# Histograma años de empleo
p_emp <- ggplot(lending_base, aes(x = años_empleo)) +
  geom_histogram(binwidth = 1, fill = "#a6bddb", color = "white", center = 0.5) +
  labs(title = "Distribución años de empleo", x = "Años de empleo", y = "Cuenta") +
  tema
print(p_emp)
# 3) Resumen numérico general (tabla mejorada con kableExtra) ------------
resumen_general <- lending_base %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico, años_empleo) %>%
  summarise_all(list(
    N = ~n(),
    Media = ~mean(., na.rm = TRUE),
    Mediana = ~median(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  )) %>%
  pivot_longer(everything(), names_to = c("variable", "estadistico"),
               names_sep = "_", values_to = "valor") %>%
  pivot_wider(names_from = estadistico, values_from = valor)
print(resumen_general)
# Formatear y mostrar con kable
resumen_general %>%
  mutate(
    Media = if_else(variable == "ingreso", scales::dollar(Media), as.character(round(as.numeric(Media), 2))),
    Mediana = if_else(variable == "ingreso", scales::dollar(Mediana), as.character(round(as.numeric(Mediana), 2))),
    Min = if_else(variable == "ingreso", scales::dollar(Min), as.character(round(as.numeric(Min), 2))),
    Max = if_else(variable == "ingreso", scales::dollar(Max), as.character(round(as.numeric(Max), 2)))
  ) %>%
  kable(format = "html", caption = "Resumen general de variables numéricas", align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE, background = "#e9f2fb") %>%
  footnote(general = "Tabla de resumen: N, media, mediana, sd, min y max.")

# 4) Caracterización de las clases (Paga vs No_paga) -------------------
tabla_por_estado <- lending_base %>%
  group_by(estado_pago) %>%
  summarise(
    n = n(),
    tasa_default = mean(estado_pago == "No_paga"),
    ingreso_med = median(ingreso, na.rm = TRUE),
    dti_med = median(relacion_deuda_ingreso, na.rm = TRUE),
    monto_med = median(monto_prestamo, na.rm = TRUE),
    fico_med = median(puntaje_fico, na.rm = TRUE),
    anos_emp_med = median(años_empleo, na.rm = TRUE)
  )

# Mostrar con kable
tabla_por_estado %>%
  mutate(ingreso_med = scales::dollar(ingreso_med)) %>%
  kable(format = "html", caption = "Caracterización por clase: Paga vs No_paga", align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE, background = "#fcefe6")

# Tabla de frecuencias y proporciones
freq_estado <- lending_base %>% count(estado_pago) %>% mutate(porc = round(n / sum(n) * 100, 2))

freq_estado %>%
  kable(format = "html", caption = "Frecuencias por estado de pago", align = 'c') %>%
  kable_styling(full_width = FALSE)

# Boxplot FICO por estado (aplicando tema)
set.seed(123)
muestra_plot <- lending_base %>% sample_n(min(50000, nrow(.)))
p_fico_box <- ggplot(muestra_plot, aes(x = estado_pago, y = puntaje_fico, fill = estado_pago)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.15, size = 0.6) +
  labs(title = "Puntaje FICO por estado de pago", x = "", y = "Puntaje FICO") +
  tema
ggsave("figuras/fico_boxplot_estado.png", p_fico_box, width = 8, height = 4.5, dpi = 300)

# Densidad FICO por estado
p_fico_dens <- ggplot(lending_base, aes(x = puntaje_fico, fill = estado_pago, color = estado_pago)) +
  geom_density(alpha = 0.35, size = 0.3) +
  labs(title = "Densidad FICO por estado de pago", x = "Puntaje FICO", y = "Densidad") +
  tema
ggsave("figuras/fico_densidad_estado.png", p_fico_dens, width = 10, height = 4.5, dpi = 300)

# 5) Comportamiento según propósito del préstamo -----------------------
# Tabla agregada por propósito
tabla_proposito <- lending_base %>%
  group_by(proposito_agrupado) %>%
  summarise(
    n = n(),
    tasa_no_paga = mean(estado_pago == "No_paga"),
    ingreso_med = median(ingreso, na.rm = TRUE),
    monto_med = median(monto_prestamo, na.rm = TRUE),
    fico_med = median(puntaje_fico, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

# Mostrar con kable (formato más limpio)
tabla_proposito %>%
  mutate(ingreso_med = scales::dollar(ingreso_med), monto_med = scales::dollar(monto_med),
         tasa_no_paga = scales::percent(tasa_no_paga)) %>%
  kable(format = "html", caption = "Resumen por propósito agrupado", align = 'c') %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","hover")) %>%
  column_spec(1, bold = TRUE)

# Cruzada propósito vs estado de pago (tabla de contingencia y tasas)
tabla_cruzada <- lending_base %>%
  tabyl(proposito_agrupado, estado_pago) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2)

# Mostrar tabla cruzada
tabla_cruzada %>%
  kable(format = "html", caption = "% por estado de pago por propósito agrupado (filas=propósito)") %>%
  kable_styling(full_width = FALSE)

# Visualizaciones por propósito: densidad FICO facetada y violín ingreso por estado
p_fico_prop <- ggplot(lending_base, aes(x = puntaje_fico, fill = estado_pago)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ proposito_agrupado, ncol = 2) +
  labs(title = "Distribución FICO por propósito y estado de pago", x = "Puntaje FICO") +
  tema
ggsave("figuras/fico_por_proposito.png", p_fico_prop, width = 12, height = 6, dpi = 300)

p_violin_ingreso <- ggplot(lending_base, aes(x = estado_pago, y = ingreso, fill = estado_pago)) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Distribución de ingreso por estado de pago", x = "Estado de pago", y = "Ingreso (USD)") +
  tema
ggsave("figuras/violin_ingreso_estado.png", p_violin_ingreso, width = 9, height = 5, dpi = 300)

# 6) Relaciones entre variables -----------------------------------------
# Correlación y mapa de calor
num_vars <- lending_base %>% select(ingreso, relacion_deuda_ingreso, monto_prestamo, puntaje_fico, años_empleo)
cor_mat <- cor(num_vars, use = "complete.obs")

p_corr <- ggcorrplot(cor_mat, lab = TRUE, lab_size = 3, title = "Matriz de correlaciones entre variables numéricas")
# aplicar tema básico al plot de correlación (ggcorrplot devuelve ggplot)
p_corr <- p_corr + tema
ggsave("figuras/matriz_correlacion.png", p_corr, width = 7, height = 6, dpi = 300)

# Densidad 2d préstamos vs ingreso (mapa de densidad)
p_bin2d <- ggplot(lending_base, aes(x = ingreso, y = monto_prestamo)) +
  geom_bin2d(bins = 60) +
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(title = "Densidad de préstamos según ingreso y monto", x = "Ingreso", y = "Monto del préstamo") +
  tema
ggsave("figuras/densidad_prestamo_ingreso.png", p_bin2d, width = 10, height = 5, dpi = 300)

# FICO vs DTI con línea de tendencia
p_fico_dti <- ggplot(lending_base, aes(x = puntaje_fico, y = relacion_deuda_ingreso, color = estado_pago)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(title = "Relación entre puntaje FICO y relación deuda/ingreso") +
  tema
ggsave("figuras/fico_vs_dti.png", p_fico_dti, width = 9, height = 5, dpi = 300)

# 7) Bins de FICO con tasa de default (útil para reportes)
tabla_bins <- lending_base %>%
  mutate(fico_bin = cut(puntaje_fico, breaks = seq(600, 850, by = 25), right = FALSE)) %>%
  group_by(fico_bin) %>%
  summarise(n = n(), no_paga = sum(estado_pago == "No_paga"), tasa_no_paga = no_paga / n) %>%
  arrange(fico_bin)

tabla_bins %>%
  mutate(tasa_no_paga = scales::percent(tasa_no_paga, accuracy = 0.1)) %>%
  kable(format = "html", caption = "Tasa de default por bin de FICO") %>%
  kable_styling(full_width = FALSE)

# Guardar una versión reducida de la base para reportes (opcional)
write_csv(lending_base, "lending_base_filtrada.csv")

# Mensaje final (impreso en consola)
cat("Análisis descriptivo completo generado. Plots guardados en la carpeta 'figuras' y tablas mostradas en formato HTML para informes (kable/kableExtra).\n")

# Fin del script
