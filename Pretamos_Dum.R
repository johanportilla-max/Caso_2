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

# Muestra balanceada estratificada (5000 Paga, 5000 No_paga)
set.seed(28)
lending_muestra <- lending_base_final %>%
  group_by(estado_pago) %>%
  sample_n(5000, replace = FALSE) %>%
  ungroup()

# Partición entrenamiento y prueba
set.seed(28)
indice <- createDataPartition(y = lending_muestra$estado_pago, p = 0.75, list = FALSE)
train <- lending_muestra[indice, ]
test <- lending_muestra[-indice, ]

# 3. MODELO KNN BÁSICO (SIN ESCALAR)

vars_input_num <- c("ingreso", "relacion_deuda_ingreso", "monto_prestamo", "puntaje_fico", "años_empleo")
train_input_raw <- train[, vars_input_num]
test_input_raw <- test[, vars_input_num]
train_output <- train$estado_pago
test_output <- test$estado_pago

k_valores <- 1:30
resultado_basico <- data.frame(k = k_valores, precision = 0)

for (n in k_valores) {
  pred_temp <- knn(
    train = train_input_raw, test = test_input_raw,
    cl = train_output, k = n
  )
  resultado_basico$precision[n] <- mean(pred_temp == test_output)
}

k_optimo_basico <- resultado_basico$k[which.max(resultado_basico$precision)]
pred_knn_basico_opt <- knn(train = train_input_raw, test = test_input_raw, 
                           cl = train_output, k = k_optimo_basico)


# MODELO KNN AVANZADO CON DUMMIES

ctrl_knn <- trainControl(
  method = "cv", number = 5, classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Definimos la fórmula, SOLAMENTE con proposito_agrupado
formula_modelo <- estado_pago ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
  puntaje_fico + años_empleo + proposito_agrupado

set.seed(28)
modelo_knn_mejorado <- train(
  formula_modelo,
  data = train,
  method = "knn",
  trControl = ctrl_knn,
  preProcess = c("center", "scale"), # ESCALAMIENTO Y DUMMIES AUTOMÁTICOS
  tuneLength = 20
)

pred_prob_knn <- predict(modelo_knn_mejorado, newdata = test, type = "prob")
pred_clase_knn <- predict(modelo_knn_mejorado, newdata = test)
print(confusionMatrix(pred_clase_knn, test$estado_pago, positive = "No_paga"))

roc_knn <- roc(response = test$estado_pago,
               predictor = pred_prob_knn$No_paga,
               levels = c("Paga", "No_paga"))
cat("AUC KNN Avanzado:", auc(roc_knn), "\n")

# MODELO LOGIT 

train_logit <- train %>% mutate(default_num = ifelse(estado_pago == "No_paga", 1, 0))
test_logit <- test %>% mutate(default_num = ifelse(estado_pago == "No_paga", 1, 0))

# Entrenamiento: R crea las dummy variables automáticamente
Loan_logit<- glm(
  default_num ~ ingreso + relacion_deuda_ingreso + monto_prestamo +
    puntaje_fico + años_empleo + proposito_agrupado,
  data = train_logit,
  family = binomial()
)

summary(Loan_logit)

# Probabilidad de "No Paga"
p_hat <- predict(Loan_logit, newdata = test_logit, type = "response")

# AUC y Umbral Óptimo
roc_logit <- roc(response = test_logit$estado_pago,
                 predictor = p_hat,
                 levels = c("Paga", "No_paga"))

thr <- coords(roc_logit, x = "best", best.method = "youden", ret = "threshold")
umbral <- as.numeric(thr)

# Clasificación con umbral óptimo
pred_clase_opt_logit <- factor(ifelse(p_hat >= umbral, "No_paga", "Paga"),
                               levels = c("Paga", "No_paga"))

confusionMatrix(pred_clase_opt_logit, test_logit$estado_pago, positive = "No_paga")
