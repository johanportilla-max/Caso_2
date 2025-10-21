library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)

lending_raw=read_csv("LC_loans_granting_model_dataset.csv", guess_max = 20000)

unique(lending_raw$fecha_emision[1:20])
View(lending_raw)

lending_baseP=lending_raw %>%
  rename(fecha_emision = issue_d)

lending_base=na.omit(lending_base)

fechas_raw=lending_raw$issue_d

unique(fechas_raw[1:20])
fechas_convertidas=parse_date_time(fechas_raw, orders = "b-Y", locale = "en_US")

summary(fechas_convertidas)
head(fechas_convertidas)

lending_baseP <- lending_raw %>%
  mutate(
    fecha_emision = parse_date_time(issue_d, orders = "b-Y", locale = "en_US"),
    anio = year(fecha_emision),
    estado_pago = factor(Default, levels = c(0,1), labels = c("Paga", "No Paga")))

tasa_anual <- lending_baseP %>%
  group_by(anio) %>%
  summarise(
    tasa_no_pago = mean(Default == 1),
    cantidad_casos = n())

ggplot(tasa_anual, aes(x = anio, y = tasa_no_pago)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Tasa de no pago por año de emisión",
       x = "Año de emisión", y = "Tasa de no pago") +
  theme_minimal()


tasa_estado <- lending_baseP %>%
  group_by(addr_state) %>%
  summarise(
    tasa_no_pago = mean(Default == 1),
    cantidad_casos = n()
  ) %>%
  arrange(desc(tasa_no_pago))

tasa_estado %>%
  head(15) %>%
  ggplot(aes(x = reorder(addr_state, tasa_no_pago), y = tasa_no_pago)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 estados con mayor tasa de no pago",
       x = "Estado", y = "Tasa de no pago") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


