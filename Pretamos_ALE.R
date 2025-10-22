# modelo con distinta particion, aleatoria 

library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)
library(ggthemes)

# Base original
lending_base <- read_csv("LC_loans_granting_model_dataset.csv") %>%
  select(revenue, dti_n, loan_amnt, fico_n, experience_c,
         emp_length, Default) %>%
  mutate(
    emp_length = case_when(
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
    ),
    experiencia_lc = as.factor(experience_c),
    estado_pago = as.factor(Default)
  ) %>%
  rename(
    ingreso = revenue,
    relacion_deuda_ingreso = dti_n,
    monto_prestamo = loan_amnt,
    puntaje_fico = fico_n
  ) %>%
  mutate(estado_pago = fct_recode(estado_pago, "Paga" = "0", "No_paga" = "1")) %>%
  na.omit()   # eliminamos filas con NA

# muestra aleatorio

set.seed(28)
index_muestra <- sample(1:nrow(lending_base), 10000)
lending_muestra <- lending_base[index_muestra, ]

index_entrena <- sample(index_muestra, 7000)
index_test <- index_muestra[!index_muestra %in% index_entrena]

train <- lending_base[index_entrena, ]
test  <- lending_base[index_test, ]

# Preparación para KNN

train_knn <- train %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo,
         puntaje_fico, experiencia_lc, estado_pago)

test_knn <- test %>%
  select(ingreso, relacion_deuda_ingreso, monto_prestamo,
         puntaje_fico, experiencia_lc, estado_pago)

# Creamos variables dummy (solo para las predictoras)
dummies_model <- dummyVars(~ ., data = train_knn %>% select(-estado_pago))

x_train <- data.frame(predict(dummies_model, newdata = train_knn))
x_test  <- data.frame(predict(dummies_model, newdata = test_knn))

# Combinamos nuevamente con la variable dependiente
train_knn_final <- data.frame(x_train, estado_pago = train_knn$estado_pago)
test_knn_final  <- data.frame(x_test,  estado_pago = test_knn$estado_pago)

# Aseguramos que sea factor
train_knn_final$estado_pago <- as.factor(train_knn_final$estado_pago)
test_knn_final$estado_pago  <- as.factor(test_knn_final$estado_pago)

# Eliminamos filas con NA
train_knn_final <- na.omit(train_knn_final)
test_knn_final  <- na.omit(test_knn_final)

# Modelo KNN

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(28)
modelo_knn <- train(
  estado_pago ~ .,
  data = train_knn_final,
  method = "knn",
  metric = "ROC",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  tuneLength = 20
)

modelo_knn
plot(modelo_knn)

# Evaluación

pred_knn_clase <- predict(modelo_knn, newdata = test_knn_final)
pred_knn_prob  <- predict(modelo_knn, newdata = test_knn_final, type = "prob")

# Matriz de confusión
confusionMatrix(pred_knn_clase, test_knn_final$estado_pago, positive = "No_paga")

# Curva ROC y AUC
roc_knn <- roc(response = test_knn_final$estado_pago,
               predictor = pred_knn_prob$No_paga,
               levels = c("Paga", "No_paga"))

auc_knn <- auc(roc_knn)
plot(roc_knn, col = "#0066CC", lwd = 2,
     main = sprintf("Curva ROC - KNN | AUC = %.3f", auc_knn))
abline(a = 0, b = 1, lty = 2, col = "gray")
