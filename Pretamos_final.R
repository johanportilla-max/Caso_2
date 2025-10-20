# MODELO KNN - Predicción de pago de créditos

library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)

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
    TRUE ~ NA_real_
  ))

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
                             "No_paga" = "1")
  )

lending_base <- na.omit(lending_base)

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
)

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

