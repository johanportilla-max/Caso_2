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
           color = "#c0392b", hjust = -0.1, vjust = -1, size = 3.8, fontface = "bold") +
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

matriz <- as.data.frame(cm$table)
colnames(matriz) <- c("Predicción", "Real", "Frecuencia")

tabla_conf <- matriz %>%
  kbl(
    caption = "Matriz de Confusión del Modelo KNN (class)",
    col.names = c("Predicción", "Real", "Frecuencia"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    position = "center"
  ) %>%
  row_spec(0, background = "#1a5276", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  column_spec(2:3, width = "3cm")

metricas_knn <- data.frame(
  Métrica = c(
    "Precisión (Accuracy)",
    "Kappa",
    "Sensibilidad (Recall)",
    "Especificidad", 
    "Precisión (Precision)",
    "Valor Predictivo Negativo",
    "Precisión Balanceada"
  ),
  Valor = c(
    round(cm$overall["Accuracy"], 4),
    round(cm$overall["Kappa"], 4),
    round(cm$byClass["Sensitivity"], 4),
    round(cm$byClass["Specificity"], 4),
    round(cm$byClass["Pos Pred Value"], 4),
    round(cm$byClass["Neg Pred Value"], 4),
    round(cm$byClass["Balanced Accuracy"], 4)
  ),
  Descripción = c(
    "Proporción total de predicciones correctas",
    "Acuerdo entre real y predicho, ajustado por azar",
    "Capacidad de identificar correctamente 'No_paga'",
    "Capacidad de identificar correctamente 'Paga'",
    "Probabilidad de que 'No_paga' predicho sea correcto",
    "Probabilidad de que 'Paga' predicho sea correcto",
    "Promedio entre sensibilidad y especificidad"
  )
)

tabla_metricas <- metricas_knn %>%
  kbl(
    caption = "Métricas de Desempeño del Modelo KNN",
    align = "c",
    col.names = c("Métrica", "Valor", "Descripción")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  row_spec(0, background = "#1a5276", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  column_spec(2, width = "2cm") %>%
  column_spec(3, width = "6cm") %>%
  footnote(
    general = "Clase positiva: 'No_paga'",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )

tabla_conf
tabla_metricas