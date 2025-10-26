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
library(gridExtra)
library(grid)
library(ggplotify)

# Base de Datos

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

ggplot(lending_raw, aes(x = revenue)) +
  geom_histogram(binwidth = 25000, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 250000, linetype = "dashed", color = "red", linewidth = 1) +
  xlim(0, 500000) +
  labs(title = "Distribución del Ingreso (Filtro 250K)", x = "Ingreso (Revenue)")

ggplot(lending_raw, aes(x = dti_n)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 1) +
  xlim(0, 100) +
  labs(title = "Distribución de DTI (Filtro 50)", x = "Relación Deuda/Ingreso (DTI)")

# filtro
lending_base <- lending_base %>%
  filter(ingreso <= 250000, relacion_deuda_ingreso <= 50) %>%
  drop_na()

# MUestra aleatoria proporcional 
set.seed(28)
paga <- lending_base %>% filter(estado_pago == "Paga") %>% sample_n(5000)
nopaga <- lending_base %>% filter(estado_pago == "No_paga") %>% sample_n(5000)
lending_muestra <- bind_rows(paga, nopaga) %>% arrange(sample(1:n()))

# Particion
set.seed(28)
index_muestra <- sample(1:nrow(lending_muestra), nrow(lending_muestra))
index_entrena <- sample(index_muestra, floor(0.75 * length(index_muestra)))
index_test <- setdiff(index_muestra, index_entrena)

train <- lending_muestra[index_entrena, ]
test <- lending_muestra[index_test, ]

# KNN (CLASS)
vars_input <- c("ingreso", "relacion_deuda_ingreso", "monto_prestamo", "puntaje_fico")
train_input <- train[, vars_input]
test_input  <- test[, vars_input]
train_output <- train$estado_pago
test_output  <- test$estado_pago

k_vals <- 1:100
resultado <- data.frame(k = k_vals, precision = 0)

# centrar y escalar 

scaler <- preProcess(train_input, method = c("center", "scale"))
train_input_scaled <- predict(scaler, train_input)
test_input_scaled  <- predict(scaler, test_input)


for (n in k_vals) {
  pred_temp <- knn(train = train_input_scaled, test = test_input_scaled, cl = train_output, k = n)
  resultado$precision[n] <- mean(pred_temp == test_output)
}

ggplot(resultado, aes(k, precision)) +
  geom_line(color = "steelblue") +
  labs(title = "Precisión según k (KNN)", x = "k", y = "Precisión")

k_optimo <- resultado$k[which.max(resultado$precision)]
k_optimo


pred_knn <- knn(train = train_input, test = test_input, cl = train_output, k = k_optimo)

mean(pred_knn == test_output)
table(pred_knn, test_output)

confusionMatrix(pred_knn, test_output, positive = "No_paga")

# KNN CARET

ctrl_knn <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

modelo_knn <- train(
  estado_pago ~ ingreso + relacion_deuda_ingreso + monto_prestamo + puntaje_fico + proposito_agrupado,
  data = train,
  method = "knn",
  trControl = ctrl_knn,
  preProcess = c("center", "scale"),
  tuneLength = 150,
  metric = "ROC"
)

plot(modelo_knn)
modelo_knn

pred_clase_knn <- predict(modelo_knn, newdata = test)
pred_prob_knn  <- predict(modelo_knn, newdata = test, type = "prob")

confusionMatrix(pred_clase_knn, test$estado_pago, positive = "No_paga")

roc_knn <- roc(response = test$estado_pago, predictor = pred_prob_knn$No_paga, levels = c("Paga", "No_paga"))
auc(roc_knn)
plot(roc_knn)
auc_knn <- round(auc(roc_knn), 4)
auc_knn
# LOGIT 

train_logit <- train %>% mutate(default_num = ifelse(estado_pago == "No_paga", 1, 0))
test_logit  <- test %>% mutate(default_num = ifelse(estado_pago == "No_paga", 1, 0))

fit_logit <- glm(default_num ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
                   puntaje_fico + proposito_agrupado,
                 data = train_logit, family = binomial())

summary(fit_logit)

p_hat <- predict(fit_logit, newdata = test_logit, type = "response")

roc_logit <- roc(response = test_logit$estado_pago, predictor = p_hat, levels = c("Paga", "No_paga"))
thr <- coords(roc_logit, x = "best", best.method = "youden", ret = "threshold")
umbral <- as.numeric(thr)

pred_clase_logit <- factor(ifelse(p_hat >= umbral, "No_paga", "Paga"),
                           levels = c("Paga", "No_paga"))

confusionMatrix(pred_clase_logit, test_logit$estado_pago, positive = "No_paga")

auc_val <- auc(roc_logit)
plot(roc_logit, main = sprintf("ROC Logit | AUC = %.3f | Umbral = %.3f", auc_val, umbral))


########
clases <- lending_base %>%
  group_by(estado_pago) %>%
  summarise(
    N_observaciones = n(),
    Porcentaje = round(100 * n() / nrow(lending_base), 2),
    .groups = 'drop'
  )
tabla_clases=clases %>%
  kbl(
    caption = "Tabla 1. Distribución de clases antes del balanceo",
    align = c("l", "r", "r"),
    col.names = c("Estado de pago", "N observaciones", "Porcentaje (%)")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3.5cm") %>%
  column_spec(2:3, width = "3cm") %>%
  footnote(
    general = "Fuente: Elaboración propia con base en el dataset Lending Club (2007–2018).",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )
tabla_clases


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
### class

Grafico_precision <- ggplot(resultado, aes(x = k, y = precision)) +
  geom_line(color = "#1a5276", linewidth = 1.1) +
  geom_point(aes(color = precision), size = 2.5, alpha = 0.8) +
  scale_color_gradient(
    low = "#aed6f1",
    high = "#1a5276",
    name = "Precisión",
    labels = percent_format(accuracy = 1)
  ) +
  geom_vline(xintercept = k_optimo, color = "#c0392b", linetype = "dashed", linewidth = 1) +
  annotate("text", x = k_optimo, y = max(resultado$precision),
           label = paste("k óptimo =", k_optimo),
           color = "#c0392b", hjust = -0.1, vjust = -1, size = 3, fontface = "bold") +
  labs(
    title = "Figura 2. Precisión del modelo KNN según número de vecinos (k)",
    subtitle = "El valor óptimo de k maximiza la precisión de clasificación en el conjunto de prueba",
    x = "Número de vecinos (k)",
    y = "Precisión",
    caption = "Fuente: Elaboración propia con base en el dataset Lending Club (2007–2018)"
  ) +
  tema +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#34495e"),
    plot.caption = element_text(size = 9, color = "#7f8c8d", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11, color = "#2c3e50"),
    legend.position = "bottom"
  )


print(Grafico_precision)


### confu

cm <- confusionMatrix(pred_knn, test_output, positive = "No_paga")

matriz_conf <- as.data.frame(cm$table)

matriz_tabla <- matrix(
  c(matriz_conf$Freq[1], matriz_conf$Freq[2],
    matriz_conf$Freq[3], matriz_conf$Freq[4]),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    "Predicción" = c("Paga", "No_paga"),
    "Referencia" = c("Paga", "No_paga")
  )
)

matriz_tabla %>%
  kbl(
    caption = "Tabla 3. Matriz de Confusión del Modelo KNN (class)",
    align = c("c", "c", "c"),
    col.names = c("Paga", "No_paga")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, width = "3cm", bold = TRUE) %>%
  add_header_above(c(" " = 1, "Referencia" = 2), bold = TRUE, background = "#2b6cb0", color = "white") %>%
  footnote(
    general = "La matriz muestra las predicciones frente a los valores reales para la clase positiva 'No_paga'.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )

## Metricas y descripcion

metricas_knn <- data.frame(
  Indicador = c(
    "Accuracy (Exactitud)",
    "95% CI",
    "No Information Rate", 
    "P-Value [Acc > NIR]",
    "Kappa",
    "Mcnemar's Test P-Value",
    "Sensitivity",
    "Specificity",
    "Pos Pred Value",
    "Neg Pred Value", 
    "Prevalence",
    "Detection Rate",
    "Detection Prevalence",
    "Balanced Accuracy"
  ),
  Descripción = c(
    "Proporción total de predicciones correctas",
    "Intervalo de confianza del 95% para exactitud",
    "Tasa al predecir siempre clase mayoritaria",
    "Valor p vs tasa no información",
    "Acuerdo ajustado por azar",
    "Test de McNemar para diferencias entre clases",
    "Capacidad detectar 'No_paga' (VP / VP+FN)",
    "Capacidad detectar 'Paga' (VN / VN+FP)", 
    "Precisión para clase 'No_paga'",
    "Precisión para clase 'Paga'",
    "Proporción real de 'No_paga'",
    "Tasa de detección de 'No_paga'",
    "Proporción predicha de 'No_paga'",
    "Promedio sensibilidad y especificidad"
  ),
  Valor = c(
    "0.5564", "(0.5367, 0.5760)", "0.5104", "2.25e-06", "0.1135", "0.03061",
    "0.5768", "0.5368", "0.5443", "0.5694", "0.4896", "0.2824", "0.5188", "0.5568"
  )
)

# Crear tabla formateada
tabla_metricas <- metricas_knn %>%
  kbl(
    caption = "Métricas de Evaluación - Modelo KNN",
    align = c("l", "l", "c"),
    col.names = c("Métrica", "Descripción", "Valor")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  column_spec(2, width = "6cm") %>%
  column_spec(3, width = "2cm")
tabla_metricas

## caret 

ggplot(modelo_knn$results, aes(x = k, y = ROC)) +
  geom_line(color = "#1a5276", linewidth = 1.1) +
  geom_point(aes(color = ROC), size = 2.5, alpha = 0.8) +
  scale_color_gradient(
    low = "#aed6f1",
    high = "#1a5276",
    name = "AUC (ROC)"
  ) +
  geom_vline(xintercept = modelo_knn$bestTune$k, color = "#c0392b", linetype = "dashed") +
  annotate("label", x = modelo_knn$bestTune$k, y = max(modelo_knn$results$ROC)-0.01,
           label = paste("k óptimo =", modelo_knn$bestTune$k),
           color = "white", fill = "#c0392b", fontface = "bold") +
  labs(
    title = "AUC del modelo KNN según número de vecinos (k)",
    subtitle = "El valor óptimo de k maximiza el área bajo la curva ROC en validación cruzada (5-fold)",
    x = "Número de vecinos (k)",
    y = "Área bajo la curva (ROC)",
    caption = "Fuente: Elaboración propia con base en el dataset Lending Club (2007–2018)"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#34495e"),
    plot.caption = element_text(size = 9, color = "#7f8c8d", hjust = 0.5),
    legend.position = "bottom"
  )


cm_caret <- confusionMatrix(pred_clase_knn, test$estado_pago, positive = "No_paga")

matriz_conf <- matrix(
  c(cm_caret$table[1], cm_caret$table[2],
    cm_caret$table[3], cm_caret$table[4]),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    "Predicción" = c("Paga", "No_paga"),
    "Referencia" = c("Paga", "No_paga")
  )
)

matriz_conf %>%
  kbl(
    caption = "Tabla 1. Matriz de Confusión del Modelo KNN (caret)",
    align = c("c", "c", "c"),
    col.names = c("Paga", "No_paga")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  add_header_above(c(" " = 1, "Referencia" = 2),
                   bold = TRUE, background = "#2b6cb0", color = "white") %>%
  footnote(
    general = "La matriz muestra las predicciones frente a los valores reales para la clase positiva 'No_paga'.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )


metricas_tabla %>%
  kbl(
    caption = "Tabla 2. Indicadores de Desempeño del Modelo KNN (caret)",
    align = c("l", "l", "c"),
    col.names = c("Métrica", "Descripción", "Valor")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, width = "3cm", bold = TRUE) %>%
  column_spec(2, width = "7cm") %>%
  column_spec(3, width = "2cm") %>%
  footnote(
    general = "Los indicadores se calcularon considerando la clase positiva 'No_paga'.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )
## 


cm_caret <- confusionMatrix(pred_clase_knn, test$estado_pago, positive = "No_paga")

acc <- round(cm_caret$overall["Accuracy"], 4)
acc_ci <- paste0("(", round(cm_caret$overall[["AccuracyLower"]], 4), ", ",
                 round(cm_caret$overall[["AccuracyUpper"]], 4), ")")

no_info_rate <- round(as.numeric(cm_caret$overall[[3]]), 4)  # Tercera posición de la lista "overall"

p_value_acc <- "<2.2e-16"
kappa <- round(cm_caret$overall["Kappa"], 4)
mcnemar <- formatC(as.numeric(cm_caret$overall["McnemarPValue"]), format = "e", digits = 2)

metricas_tabla <- data.frame(
  Métrica = c(
    "Accuracy (Exactitud)",
    "95% CI (Intervalo de Confianza)",
    "No Information Rate",
    "P-Value [Acc > NIR]",
    "Kappa",
    "Mcnemar's Test P-Value",
    "Sensitivity",
    "Specificity",
    "Pos Pred Value",
    "Neg Pred Value",
    "Prevalence",
    "Detection Rate",
    "Detection Prevalence",
    "Balanced Accuracy"
  ),
  Valor = c(
    acc,
    acc_ci,
    no_info_rate,  # <-- ya no será NA
    p_value_acc,
    kappa,
    mcnemar,
    round(cm_caret$byClass["Sensitivity"], 4),
    round(cm_caret$byClass["Specificity"], 4),
    round(cm_caret$byClass["Pos Pred Value"], 4),
    round(cm_caret$byClass["Neg Pred Value"], 4),
    round(cm_caret$byClass["Prevalence"], 4),
    round(cm_caret$byClass["Detection Rate"], 4),
    round(cm_caret$byClass["Detection Prevalence"], 4),
    round(cm_caret$byClass["Balanced Accuracy"], 4)
  ),
  Interpretación = c(
    "El modelo clasifica correctamente el 59.7% de los préstamos, mostrando una mejora moderada frente al azar.",
    "El verdadero desempeño del modelo se ubica entre 57.7% y 61.6%, con un nivel de confianza del 95%.",
    "Un modelo trivial (que siempre predice la clase más frecuente) tendría un acierto del 51%.",
    "El valor p < 0.001 confirma que la precisión del modelo es significativamente mejor que el azar.",
    "El valor de Kappa (0.19) indica un acuerdo leve entre predicciones y observaciones reales.",
    "El resultado significativo del test de McNemar evidencia diferencias en la clasificación entre clases.",
    "El modelo identifica correctamente el 65.9% de los casos de incumplimiento ('No_paga').",
    "Detecta correctamente el 53.7% de los casos de pago ('Paga').",
    "Cuando predice 'No_paga', acierta en el 57.7% de los casos.",
    "Cuando predice 'Paga', acierta en el 62.2% de los casos.",
    "El 48.9% de los datos corresponde a la clase 'No_paga'.",
    "El modelo detecta correctamente el 32.3% de los casos reales de incumplimiento.",
    "Predice la clase 'No_paga' en el 55.9% de los casos totales.",
    "Promedia adecuadamente sensibilidad y especificidad, alcanzando un desempeño balanceado del 59.8%."
  )
)

metricas_tabla %>%
  kbl(
    caption = "Métricas de Evaluación - Modelo KNN (caret)",
    align = c("l", "c", "l"),
    col.names = c("Métrica", "Valor", "Interpretación")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3.5cm") %>%
  column_spec(2, width = "2.5cm") %>%
  column_spec(3, width = "10cm") %>%
  footnote(
    general = "Elaboración propia con base en el dataset Lending Club (2007–2018).",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )


##


roc_data <- data.frame(
  specificity = roc_knn$specificities,
  sensitivity = roc_knn$sensitivities
) %>%
  arrange(specificity)  # Ordenar por especificidad

curva_roc_identica <- ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  # Área bajo la curva (sombreado azul claro)
  geom_ribbon(aes(ymin = 0, ymax = sensitivity), 
              fill = "#d1e0f0", alpha = 0.8) +
  # Línea de referencia (diagonal)
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
               color = "grey60", linetype = "dashed", size = 0.7) +
  # Línea de la curva ROC
  geom_line(color = "#1a5276", size = 1.3) +
  # Invertir el eje X
  scale_x_reverse(
    name = "Especificidad",
    limits = c(1, 0),
    breaks = seq(1, 0, by = -0.2),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(
    name = "Sensibilidad",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.02, 0.02)
  ) +
  labs(
    title = "Curva ROC del Modelo KNN (caret)",
    caption = "Fuente: Elaboración propia con base en resultados del modelo."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.3, y = 0.25, 
           label = paste("AUC =", auc_knn),
           color = "#1a5276", fontface = "bold", size = 4.5)

print(curva_roc_identica)


coefs_logit <- summary(fit_logit)$coefficients %>%
as.data.frame() %>%
rownames_to_column("Variable") %>%
mutate(
OR = round(exp(Estimate), 3),
Significancia = ifelse(Pr(>|z|) < 0.05, "Significativo", "No significativo")
) %>%
rename(
Estimación = Estimate,
Error estándar = Std. Error,
Valor z = z value,
p-valor = Pr(>|z|)
)




coefs_logit %>%
kbl(
caption = "Coeficientes y Odds Ratios del Modelo Logit",
align = c("l", "c", "c", "c", "c", "c"),
col.names = c("Variable", "Estimación", "Error estándar", "Valor z", "p-valor", "OR")
) %>%
kable_styling(
bootstrap_options = c("striped", "hover", "condensed"),
full_width = FALSE,
font_size = 12,
position = "center"
) %>%
row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
column_spec(1, bold = TRUE, width = "4cm") %>%
column_spec(6, bold = TRUE, width = "2cm") %>%
footnote(
general = "El OR > 1 indica un incremento en la probabilidad de 'No_paga'.",
general_title = "Nota:",
footnote_as_chunk = TRUE
)
