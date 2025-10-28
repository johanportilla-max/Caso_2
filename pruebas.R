```{r particion_class}
library(class)

vars_input <- c("Ingreso", "Relacion_deuda_ingreso", "Monto_prestado", "FICO")

train_clean <- na.omit(train[c(vars_input, "Estado")])
test_clean <- na.omit(test[c(vars_input, "Estado")])

train_input <- train_clean[, vars_input]
test_input <- test_clean[, vars_input]
train_output <- train_clean$estado_pago
test_output <- test_clean$estado_pago



k_vals <- 1:100
resultado <- data.frame(k = k_vals, precision = 0)

scaler <- preProcess(train_input, method = c("center", "scale"))
train_input_scaled <- predict(scaler, train_input)
test_input_scaled  <- predict(scaler, test_input)


for (n in k_vals) {
  pred_temp <- knn(
    train = train_input_scaled, 
    test = test_input_scaled, 
    cl = train_output, 
    k = n
  )
  resultado$precision[n] <- mean(pred_temp == test_output)
}

k_optimo <- resultado$k[which.max(resultado$precision)]

prec_opt <- max(resultado$precision)

pred_knn <- knn(train = train_input, test =test_input, cl = train_output,
                k = k_optimo)
```library(plotly)

SC <- sebastian_colors

# Calcular estadísticas por grupo para las anotaciones
stats_fico <- lending_base %>%
  group_by(estado_pago) %>%
  summarise(
    mediana = median(puntaje_fico, na.rm = TRUE),
    q1 = quantile(puntaje_fico, 0.25, na.rm = TRUE),
    q3 = quantile(puntaje_fico, 0.75, na.rm = TRUE),
    n = n()
  )

p_fico_box_interactivo <- plot_ly(
  lending_base, 
  x = ~estado_pago, 
  y = ~puntaje_fico, 
  color = ~estado_pago,
  type = "box",
  boxpoints = "all",  # Muestra todos los puntos
  pointpos = -1.8,    # Posición de los puntos
  jitter = 0.3,       # Jitter para evitar sobreposición
  marker = list(
    size = 4,
    opacity = 0.6,
    line = list(width = 1, color = "white")
  ),
  line = list(width = 2),
  whiskerwidth = 0.8,
  fillcolor = "rgba(255,255,255,0)",  # Relleno transparente
  hoverinfo = "y+x+name"
) %>%
  layout(
    title = list(
      text = "<b>Distribución del Puntaje FICO por Estado de Pago</b>",
      font = list(
        family = "Playfair Display",
        size = 22,
        color = SC["primary_red"]
      ),
      x = 0.05
    ),
    xaxis = list(
      title = list(
        text = "Estado de Pago",
        font = list(
          family = "Playfair Display",
          size = 14,
          color = SC["dark_red"]
        )
      ),
      gridcolor = SC["light_red"],
      tickfont = list(family = "Source Serif Pro", size = 12)
    ),
    yaxis = list(
      title = list(
        text = "Puntaje FICO",
        font = list(
          family = "Playfair Display", 
          size = 14,
          color = SC["dark_red"]
        )
      ),
      gridcolor = SC["light_red"],
      zerolinecolor = SC["light_red"],
      range = c(600, 850)  # Rango típico de FICO
    ),
    plot_bgcolor = SC["parchment"],
    paper_bgcolor = "white",
    font = list(family = "Source Serif Pro", size = 12),
    margin = list(l = 80, r = 50, t = 80, b = 80),
    hoverlabel = list(
      font = list(family = "Source Serif Pro")
    ),
    showlegend = FALSE,  # Ocultamos la leyenda ya que el eje X ya muestra los grupos
    annotations = list(
      # Anotaciones para el grupo "Pagado"
      list(
        x = 0,  # Primera caja (Pagado)
        y = stats_fico$mediana[stats_fico$estado_pago == "Pagado"],
        text = paste("Mediana:", round(stats_fico$mediana[stats_fico$estado_pago == "Pagado"])),
        showarrow = FALSE,
        font = list(
          family = "Source Serif Pro",
          size = 10,
          color = SC["dark_red"]
        ),
        bgcolor = "white",
        bordercolor = SC["dark_red"],
        borderpad = 4,
        borderwidth = 1
      ),
      # Anotaciones para el grupo "Incumplido"
      list(
        x = 1,  # Segunda caja (Incumplido)
        y = stats_fico$mediana[stats_fico$estado_pago == "Incumplido"],
        text = paste("Mediana:", round(stats_fico$mediana[stats_fico$estado_pago == "Incumplido"])),
        showarrow = FALSE,
        font = list(
          family = "Source Serif Pro",
          size = 10,
          color = SC["dark_red"]
        ),
        bgcolor = "white",
        bordercolor = SC["dark_red"],
        borderpad = 4,
        borderwidth = 1
      ),
      # Información general
      list(
        x = 1,
        y = 600,
        text = paste(
          "Total: N =", scales::comma(nrow(lending_base)), "observaciones<br>",
          "Pagado: N =", scales::comma(stats_fico$n[stats_fico$estado_pago == "Pagado"]), "<br>",
          "Incumplido: N =", scales::comma(stats_fico$n[stats_fico$estado_pago == "Incumplido"])
        ),
        showarrow = FALSE,
        xref = "paper",
        yref = "y",
        xanchor = "right",
        align = "right",
        font = list(
          family = "Source Serif Pro",
          size = 10,
          color = SC["secondary_red"]
        ),
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = SC["light_red"],
        borderwidth = 1,
        borderpad = 4
      ),
      # Fuente
      list(
        x = 1,
        y = -0.15,
        text = "Fuente: Dataset Lending Club",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        font = list(
          family = "Source Serif Pro",
          size = 10,
          color = SC["secondary_red"]
        )
      )
    )
  ) %>%
  config(
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
    displaylogo = FALSE
  )

p_fico_box_interactivo

pred_clase_knn <- predict(modelo_knn, newdata = test)
pred_prob_knn  <- predict(modelo_knn, newdata = test, type = "prob")

roc_knn <- roc(response = test$estado_pago, predictor = pred_prob_knn$No_paga, levels = c("Paga", "No_paga"))
auc_knn <- round(auc(roc_knn), 4)