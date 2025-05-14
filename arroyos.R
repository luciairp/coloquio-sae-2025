library(tidyverse)
library(lubridate)


# cargo datos caudal:

Qm <- read.csv("Q_medido.csv",sep = ";",dec = ",")
Qm <- select(Qm,1:4)
Qm$fecha <- as.Date(Qm$fecha, format = "%d/%m/%Y")
Qm

# cargo datos Temp y Presión
# TP <- read.csv("P_T_todos.csv",sep = ";",dec = ",")
# TP$fecha <- as.Date(TP$fecha, format = "%d/%m/%Y")
# TP$datetime <- paste(TP$fecha,TP$hora,TP$am_pm)
# TP$datetime <- parse_date_time(TP$datetime, orders = "ymd HMS p")
data_arroyos <- read_csv2("P_T_todos.csv")
TP <- data_arroyos %>%
  mutate(
    fecha_formateada = as.Date(fecha,format = "%d/%m/%Y"),
    # Construir variable tipo datetime que entienda am_pm de donde sacar todo:
    fechahora = paste(fecha_formateada,hora,am_pm) %>% parse_date_time(orders = "ymd HMS p"),
    fecha = fecha_formateada,
    ) %>%
  # sacar datos mal tomados:
  slice(-c(9581:9604)) %>% 
  # sacar columnas redundantes:
  select(-am_pm,-hora, -fecha_formateada) %>% 
  mutate(horaredonda = hour(fechahora))

head(TP)

# x es QM , y es TP
# quiero traer a Qm las mediciones que cumplen ser iguales en fecha y arroyo, para 
airports |> 
  semi_join(flights2, join_by(faa == origin))

resultado <- Qm %>%
  inner_join(TP, by = c("arroyo", "fecha")) %>%
  filter(horaredonda >= 11, horaredonda <= 13)

qvsp <- ggplot(resultado,aes(x=pabs,y=q))+
  geom_point()
qvsp

qvst <- ggplot(resultado,aes(x=temp,y=q))+
  geom_point()
qvst

range(TP$pabs)
