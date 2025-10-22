library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)
library(ggthemes)
library(lubridate)

lending_raw <- read_csv("LC_loans_granting_model_dataset.csv", guess_max = 20000)

View(lending_raw)

print(unique(lending_raw$purpose))

lending_baseP <- lending_raw %>%
  mutate(
    fecha_emision = parse_date_time(issue_d, orders = "b-Y", locale = "en_US"),
    Año = year(fecha_emision),
    Valor = factor(Default, levels = c(0, 1), labels = c("Paga", "No Paga"))
  ) %>%
  select(revenue, dti_n, loan_amnt, fico_n, emp_length, Año, addr_state, Valor) %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    años_empleo = emp_length,
    Estado = addr_state,
    estado_pago = Valor)

lending_baseP <- lending_baseP %>%
  mutate(años_empleo = case_when(
    str_detect(años_empleo, "10") ~ 10,
    str_detect(años_empleo, "9")  ~ 9,
    str_detect(años_empleo, "8")  ~ 8,
    str_detect(años_empleo, "7")  ~ 7,
    str_detect(años_empleo, "6")  ~ 6,
    str_detect(años_empleo, "5")  ~ 5,
    str_detect(años_empleo, "4")  ~ 4,
    str_detect(años_empleo, "3")  ~ 3,
    str_detect(años_empleo, "2")  ~ 2,
    str_detect(años_empleo, "1")  ~ 1,
    str_detect(años_empleo, "less") ~ 0,
    TRUE ~ NA_real_)) %>% mutate(años_empleo=as.numeric(años_empleo))

View(lending_baseP)

# Tasa de no pago por año
tasa_anual <- lending_baseP %>%
  group_by(Año) %>%
  summarise(
    tasa_no_pago = mean(estado_pago == "No Paga"),
    cantidad_casos = n())

ggplot(tasa_anual, aes(x = Año, y = tasa_no_pago)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Tasa de no pago por año de emisión",
    x = "Año de emisión", y = "Tasa de no pago") +
  theme_minimal()

# Tasa por estado
tasa_estado <- lending_baseP %>%
  group_by(Estado) %>%
  summarise(
    tasa_no_pago = mean(estado_pago == "No Paga"),
    cantidad_casos = n()
  ) %>%
  arrange(desc(tasa_no_pago))

tasa_estado %>%
  head(15) %>%
  ggplot(aes(x = reorder(Estado, tasa_no_pago), y = tasa_no_pago)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Top 15 estados con mayor tasa de no pago",
    x = "Estado", y = "Tasa de no pago"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Año con mas no pagos 

lending_base_2017 <- lending_baseP %>%
  filter(Año == 2017)

# Ver distribución de clases
lending_base_2017 %>%
  group_by(estado_pago) %>%
  summarise(
    cantidad = n(),
    porcentaje = round((n() / nrow(lending_base_2017)) * 100, 2))

lending_knn_2017 <- lending_base_2017 %>%
  select(
    ingreso,
    relacion_deuda_ingreso,
    monto_prestamo,
    puntaje_fico,
    años_empleo,
    estado_pago
  )


# Eliminar NA
lending_knn_2017 <- na.omit(lending_knn_2017)

# Ver proporción de clases
prop.table(table(lending_knn_2017$estado_pago))

set.seed(28)
indx_muestra_prop <- createDataPartition(
  y = lending_knn_2017$estado_pago,
  p = 0.10,
  list = FALSE
)
lending_knn_2017_muestra <- lending_knn_2017[indx_muestra_prop, ]

set.seed(28)
indx_entrena <- createDataPartition(
  y = lending_knn_2017_muestra$estado_pago,
  p = 0.7,
  list = FALSE
)

lend_entrena_2017 <- lending_knn_2017_muestra[indx_entrena, ]
lend_test_2017 <- lending_knn_2017_muestra[-indx_entrena, ]


lending_entrena_input_2017 <- lend_entrena_2017[, -6]
lending_entrena_output_2017 <- lend_entrena_2017$estado_pago

lending_test_input_2017 <- lend_test_2017[, -6]
lending_test_output_2017 <- lend_test_2017$estado_pago

set.seed(28)
lending_test_pred_2017 <- knn(
  train = lending_entrena_input_2017,
  test = lending_test_input_2017,
  cl = lending_entrena_output_2017,
  k = 5
)

# Evaluación
cat("Exactitud modelo KNN básico:\n")
print(mean(lending_test_pred_2017 == lending_test_output_2017))
cat("Matriz de confusión:\n")
print(table(lending_test_pred_2017, lending_test_output_2017))


k_valores <- 1:30
resultado <- data.frame(k = k_valores, precision = 0)

for (n in k_valores) {
  lending_test_pred_2017 <- knn(
    train = lending_entrena_input_2017,
    test = lending_test_input_2017,
    cl = lending_entrena_output_2017,
    k = n
  )
  resultado$precision[n] <- mean(lending_test_pred_2017 == lending_test_output_2017)
}

ggplot(resultado, aes(x = k, y = precision)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(
    title = "Precisión según número de vecinos (K)",
    x = "Número de vecinos (K)", y = "Precisión"
  ) +
  theme_minimal()

lend_entrena_2017$estado_pago <- factor(lend_entrena_2017$estado_pago, levels = c("Paga", "No Paga"))
lend_test_2017$estado_pago <- factor(lend_test_2017$estado_pago, levels = c("Paga", "No Paga"))

set.seed(28)
trctrl_smote <- trainControl(
  method = "cv", 
  number = 5,
  sampling = "smote" # <--- PRUEBA ESTO
)

knn_model_caret_2017 <- train(
  estado_pago ~ .,
  data = lend_entrena_2017,
  method = "knn",
  preProc = c("center", "scale"),
  tuneLength = 20,
  trControl = trainControl(method = "cv", number = 5))

# Resultados
knn_model_caret_2017
plot(knn_model_caret_2017)

# Predicciones y evaluación
pred_knn_caret_2017 <- predict(knn_model_caret_2017, newdata = lend_test_2017)
pred_knn_caret_2017 <- factor(pred_knn_caret_2017, levels = levels(lend_test_2017$estado_pago))

confusionMatrix(pred_knn_caret_2017, lend_test_2017$estado_pago)


# logit 2017

lending_base_2017 <- lending_base_2017 %>%
  mutate(
    default_num = ifelse(estado_pago == "No Paga", 1, 0)
  )


set.seed(28)
idx <- createDataPartition(y = lending_base_2017$estado_pago, p = 0.75, list = FALSE)
train <- lending_base_2017[idx, ]
test  <- lending_base_2017[-idx, ]


fit_logit <- glm(default_num ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
                   puntaje_fico + años_empleo,
                 data = train,
                 family = binomial())

summary(fit_logit)  # puedes revisar coeficientes y significancia


# Probabilidad de "No Paga"
p_hat <- predict(fit_logit, newdata = test, type = "response")

# Clasificación según umbral 0.5
pred_clase <- factor(ifelse(p_hat >= 0.5, "No Paga", "Paga"),
                     levels = c("Paga", "No Paga"))

# Matriz de confusión
confusionMatrix(pred_clase, test$estado_pago, positive = "No Paga")


roc_o <- roc(response = test$estado_pago,
             predictor = p_hat,
             levels = c("Paga", "No Paga"))

thr <- coords(roc_o, x = "best", best.method = "youden", ret = "threshold")
umbral <- as.numeric(thr)
cat("Umbral óptimo:", umbral, "\n")

# Recalcular clasificación con umbral óptimo
pred_clase_opt <- factor(ifelse(p_hat >= umbral, "No Paga", "Paga"),
                         levels = c("Paga", "No Paga"))

# Nueva matriz de confusión
confusionMatrix(pred_clase_opt, test$estado_pago, positive = "No Paga")


auc_val <- auc(roc_o)

plot(roc_o, main = sprintf("Curva ROC | AUC = %.3f | Umbral óptimo = %.3f", auc_val, umbral),
     col = "steelblue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")



