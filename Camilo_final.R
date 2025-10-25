library(readr)
library(tidyverse)
library(class)
library(caret)
library(pROC)
library(ggthemes)
library(lubridate)

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
