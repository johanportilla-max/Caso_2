library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)
library(ggthemes)

#base de datos

lending_base <- read_csv("LC_loans_granting_model_dataset.csv")

lending_base <- lending_base %>%
  select(revenue, dti_n, loan_amnt, fico_n, experience_c,
         emp_length, Default)

lending_base <- lending_base %>%
  mutate(emp_length = case_when(
    str_detect(emp_length, "10") ~ 10,
    str_detect(emp_length, "9")  ~ 9,
    str_detect(emp_length, "8")  ~ 8,
    str_detect(emp_length, "7")  ~ 7,
    str_detect(emp_length, "6")  ~ 6,
    str_detect(emp_length, "5")  ~ 5,
    str_detect(emp_length, "4")  ~ 4,
    str_detect(emp_length, "3")  ~ 3,
    str_detect(emp_length, "2")  ~ 2,
    str_detect(emp_length, "1")  ~ 1,
    str_detect(emp_length, "less") ~ 0,
    TRUE ~ NA_real_))

# Renombramos columnas 
lending_base <- lending_base %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    experiencia_lc = experience_c,
    años_empleo = emp_length,
    estado_pago = Default
  ) %>%
  mutate(
    estado_pago = as.factor(estado_pago),
    estado_pago = fct_recode(estado_pago,
                             "Paga" = "0",
                             "No_paga" = "1"))

lending_base <- na.omit(lending_base)

#Tema para los graficos 

tema<- theme_minimal() +
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
    legend.position = "none")

# Crear muestra representativa (10 000 observaciones)

set.seed(28)

lending_muestra <- lending_base %>%
  group_by(estado_pago) %>%
  sample_n(5000, replace = FALSE) %>%  # balancea ambas clases
  ungroup()
##Preguntar si es posible de esta forma o se debe tomar proporcional o al azar 

#  Partición entrenamiento 

set.seed(28)
indice <- createDataPartition(y = lending_muestra$estado_pago, p = 0.75, list = FALSE)
train <- lending_muestra[indice, ]
test  <- lending_muestra[-indice, ]

# MODELO KNN

vars_input <- c("ingreso", "relacion_deuda_ingreso", "monto_prestamo", "puntaje_fico", "años_empleo")

train_input_raw <- train[, vars_input]
test_input_raw <- test[, vars_input]

train_output <- train$estado_pago
test_output <- test$estado_pago

k_valores <- 1:50 
resultado_basico <- data.frame(k = k_valores, precision = 0)

for (n in k_valores) {
  pred_temp <- knn(
    train = train_input_raw, 
    test = test_input_raw,
    cl = train_output,
    k = n)  
  resultado_basico$precision[n] <- mean(pred_temp == test_output)
  }

k_optimo_basico <- resultado_basico$k[which.max(resultado_basico$precision)]

pred_knn_basico_opt <- knn(
  train = train_input_raw,
  test = test_input_raw,
  cl = train_output,
  k = k_optimo_basico
)

confusionMatrix(pred_knn_basico_opt, test_output, positive = "No_paga")

ggplot(resultado_basico, aes(x = k, y = precision)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point() +
  geom_vline(xintercept = k_optimo_basico, linetype = "dashed", color = "gray50") +
  labs(
    title = "Precisión vs. Vecinos (KNN Básico SIN ESCALAR)",
    x = "Número de vecinos (K)", y = "Precisión"
  ) +
  theme_minimal()

# Entrenamiento del modelo con caret

ctrl <- trainControl(
  method = "cv",            # Validación cruzada
  number = 5,               # 5 particiones
  classProbs = TRUE,        # calcular probabilidades
  summaryFunction = twoClassSummary
)

set.seed(28)
modelo_knn <- train(
  estado_pago ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
    puntaje_fico + años_empleo,
  data = train,
  method = "knn",
  metric = "ROC",                 # se optimiza según AUC
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 20
)## revisar que hace cada codigo

#  Resultados del modelo

modelo_knn
plot(modelo_knn)  # Visualizar el rendimiento según k

# conjunto de prueba

# Predicciones
pred_clase <- predict(modelo_knn, newdata = test)
pred_prob  <- predict(modelo_knn, newdata = test, type = "prob")

# Matriz de confusión
matriz <- confusionMatrix(pred_clase, test$estado_pago, positive = "No_paga")
matriz

# Curva ROC y AUC

roc_knn <- roc(response = test$estado_pago,
               predictor = pred_prob$No_paga,
               levels = c("Paga", "No_paga"))

auc(roc_knn)

plot(roc_knn, col = "blue", lwd = 2,
     main = sprintf("Curva ROC - KNN | AUC = %.3f", auc(roc_knn)))
abline(a = 0, b = 1, lty = 2, col = "gray")



# MODELO LOGIT 

# Entrenamiento del modelo Logit

fit_logit <- glm(
  estado_pago ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
    puntaje_fico + años_empleo,
  data = train,
  family = binomial()
)

summary(fit_logit)  # Ver coeficientes, significancia y dirección de los efectos


# Predicciones sobre el conjunto de prueba

# Predicciones de probabilidad de "No_paga"
p_hat <- predict(fit_logit, newdata = test, type = "response")

# Clasificación binaria con umbral 0.5
pred_clase <- factor(ifelse(p_hat >= 0.5, "No_paga", "Paga"),
                     levels = c("Paga", "No_paga"))

# Matriz de confusión inicial
confusionMatrix(pred_clase, test$estado_pago, positive = "No_paga")


# Cálculo del umbral óptimo (Índice de Youden)

roc_logit <- roc(response = test$estado_pago,
                 predictor = p_hat,
                 levels = c("Paga", "No_paga"))

# Umbral óptimo que maximiza Sens + Esp
umbral_opt <- coords(roc_logit, x = "best", best.method = "youden", ret = "threshold")
umbral_opt <- as.numeric(umbral_opt)

# Clasificamos nuevamente con el umbral óptimo
pred_clase_opt <- factor(ifelse(p_hat >= umbral_opt, "No_paga", "Paga"),
                         levels = c("Paga", "No_paga"))

# Nueva matriz de confusión con umbral ajustado
confusionMatrix(pred_clase_opt, test$estado_pago, positive = "No_paga")


# Curva ROC y AUC

auc_logit <- auc(roc_logit)
auc_logit

plot(roc_logit, col = "#FF0000", lwd = 2,
     main = sprintf("Curva ROC - Logit | AUC = %.3f | Umbral = %.3f", auc_logit, umbral_opt))
abline(a = 0, b = 1, lty = 2, col = "gray")
