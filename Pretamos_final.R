library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)

lending_base <- read_csv("LC_loans_granting_model_dataset.csv")
View(lending_base)


lending_base=lending_base %>%
  select(revenue, dti_n, loan_amnt, fico_n, experience_c,
         emp_length, Default)

# Convertimos los años de empleo a número (por ejemplo, "10+ years" -> 10)
lending_base=lending_base %>%
  mutate(emp_length = case_when(
    str_detect(emp_length, "10") ~ 10,
    str_detect(emp_length, "9") ~ 9,
    str_detect(emp_length, "8") ~ 8,
    str_detect(emp_length, "7") ~ 7,
    str_detect(emp_length, "6") ~ 6,
    str_detect(emp_length, "5") ~ 5,
    str_detect(emp_length, "4") ~ 4,
    str_detect(emp_length, "3") ~ 3,
    str_detect(emp_length, "2") ~ 2,
    str_detect(emp_length, "1") ~ 1,
    str_detect(emp_length, "less") ~ 0,
    TRUE ~ NA_real_
  ))

lending_base=lending_base %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n,
    experiencia_lc = experience_c,
    años_empleo = emp_length,
    estado_pago = Default) %>%
  mutate(Salida = recode(estado_pago, "0" = "Paga","1" = "No paga"))

lending_base=na.omit(lending_base)

lending_base <- lending_base %>%
  mutate(
    estado_pago = as.factor(estado_pago),
    estado_pago = fct_recode(estado_pago,
                             "Paga" = "0",
                             "No_paga" = "1"))

View(lending_base)

set.seed(28)
tamaño_muestra <- 0.05

# Mantiene proporción entre "Paga" y "No_paga"
indice_muestra <- createDataPartition(
  y = lending_base$estado_pago,
  p = tamaño_muestra,
  list = FALSE
)

lending_muestra <- lending_base[indice_muestra, ]

# Usar la muestra para todo el modelado
set.seed(28)
indice <- createDataPartition(y = lending_muestra$estado_pago, p = 0.75, list = FALSE)
train <- lending_muestra[indice, ]

test  <- lending_muestra[-indice, ]

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(28)
modelo_knn <- train(
  estado_pago ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
    puntaje_fico + experiencia_lc + años_empleo,
  data = train,
  method = "knn",
  metric = "ROC",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 20)

table(lending_base$estado_pago)
table(lending_muestra$estado_pago)

modelo_knn

# Predicciones de clase

pred_clase <- predict(modelo_knn, newdata = test)

# Predicciones de probabilidad

pred_prob <- predict(modelo_knn, newdata = test, type = "prob")

confusionMatrix(pred_clase, test$estado_pago, positive = "No_paga")


roc_knn <- roc(response = test$estado_pago,
               predictor = pred_prob$No_paga, # la probabilidad de la clase positiva
               levels = c("Paga", "No_paga"))

# Mostrar AUC
auc(roc_knn)

# Graficar curva ROC
plot(roc_knn, col = "blue", lwd = 2,
     main = "Curva ROC - Modelo KNN")
abline(a = 0, b = 1, lty = 2, col = "gray")

varImp(modelo_knn)
