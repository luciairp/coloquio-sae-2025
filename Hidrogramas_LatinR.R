#analizar las pabs como proxy al caudal o nivel de agua. 
#https://www.unesco-floods.eu/r-software-in-hydrology/
#https://rpubs.com/marenas/917409

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(lubridate)
library(dtw)  #Dynamic Time Warping
library(tidyr)
library(rstatix)
library(lmom)
library(lattice)
library(lfstat)
library(zoo)
library(hydrostats)
library(hydroTSM)

#install.packages("remotes")
#remotes::install_github("USGS-R/EflowStats")
#library(EflowStats)

################
data_arroyos <- read_csv2("P_T_todos_MEL.csv")
TP1 <- data_arroyos %>%
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
  mutate(horaredonda = hour(fechahora)) %>% 
  mutate(season= case_when(
    month(fechahora) %in% c(12,1,2) ~ 'core_templado',
    month(fechahora) %in% c(3,4,11) ~ 'templado',
    month(fechahora) %in% c(6,7,8) ~ 'core_frio',
    month(fechahora) %in% c(5,9,10) ~ 'frio',
  )) %>% 
  mutate(manejo = factor(manejo,c("sin","con")))

head(TP1)
#para verificar que haya tomado bien los niveles de season
TP1$season <- factor(TP1$season)
class(TP1$season)
levels(TP1$season)

TP1$arroyo<-factor(TP1$arroyo, levels=c("16","20","96","55","69","71","73"))
######################################################################################################

#perfiles generales con datos de una vez por hora promediados por dia

hidrograma_diario <- TP1 %>%
  mutate(fecha = as_date(fechahora)) %>%
  group_by(fecha, arroyo) %>%
  summarise(pabs_media = mean(pabs, na.rm = TRUE), .groups = "drop")

ggplot(hidrograma_diario, aes(x = fecha, y = pabs_media, color = arroyo)) +
  geom_line() +
  facet_wrap(~ arroyo, scales = "free_y") + # o scales = "fixed" para misma escala
  ylim(710,780)+ 
  theme_minimal() +
  labs(title = "Hidrogramas diarios por arroyo", x = "Fecha", y = "Presión media")

#Comparar si dos hidrogramas tienen formas similares desplazadas en el tiempo.
h1 <- hidrograma_diario %>% filter(arroyo == "16") %>% pull(pabs_media)
h2 <- hidrograma_diario %>% filter(arroyo == "20") %>% pull(pabs_media)
h3 <- hidrograma_diario %>% filter(arroyo == "96") %>% pull(pabs_media)
h4 <- hidrograma_diario %>% filter(arroyo == "55") %>% pull(pabs_media)
h5 <- hidrograma_diario %>% filter(arroyo == "69") %>% pull(pabs_media)
h6 <- hidrograma_diario %>% filter(arroyo == "71") %>% pull(pabs_media)
h7 <- hidrograma_diario %>% filter(arroyo == "73") %>% pull(pabs_media)

dtw::dtw(h1, h2, keep.internals = TRUE) %>% plot()
dtw::dtw(h1, h3, keep.internals = TRUE) %>% plot()
dtw::dtw(h1, h4, keep.internals = TRUE) %>% plot()
dtw::dtw(h1, h5, keep.internals = TRUE) %>% plot()
dtw::dtw(h1, h6, keep.internals = TRUE) %>% plot()
dtw::dtw(h1, h7, keep.internals = TRUE) %>% plot()

#Métricas hidrológicas para cada arroyo ----
metricas_por_arroyo <- TP1 %>%
  group_by(arroyo) %>%
  summarise(
    mean_pabs = mean(pabs, na.rm = TRUE),
    sd_pabs   = sd(pabs, na.rm = TRUE),
    min_pabs  = min(pabs, na.rm = TRUE),
    max_pabs  = max(pabs, na.rm = TRUE),
    cv_pabs   = sd_pabs / mean_pabs
  )

print(metricas_por_arroyo)

##################################
#https://rpubs.com/marenas/917409
#los análisis aquí descriptos precisan de un dato diario, no aceptan uno por hora
#por lo tanto se filtrarán de ese modo pero se esta perdiendo informacion
#la idea es desarrollarlo igual y luego encontrar la manera "manual" de hacerlo 
#con nuestros datos
#posteriormente hay que enfocarse en una perspectiva de series de tiempo, tasa de cambio y comparaciones de perfiles
#no olvidar el objetivo de comparar entre arroyos con manejo y sin manejo

library(zoo)
library(hydroTSM)
library(dplyr)
library(lubridate)
library(ggplot2)
library(EflowStats)

#Conversion de los Datos Cargados a Formato Serie de Tiempo
head(TP1)
tail(TP1)

#Crear lista de series zoo, una por arroyo

###hidrograma diario 2 tiene pabs media por día porque necesita un valor diario
hidrograma_diario2 <- TP1 %>%
  mutate(fecha = as_date(fecha)) %>%
  group_by(fecha, arroyo) %>%
  summarise(pabs_media = mean(pabs, na.rm = TRUE), .groups = "drop")

series_por_arroyo <- hidrograma_diario2 %>%
  group_by(arroyo) %>%
  group_map(~ zoo(x = .x$pabs_media, order.by = .x$fecha))

# Nombres de cada elemento sino por default los nombra ordinalmente
names(series_por_arroyo) <- unique(TP1$arroyo)

# Visualización individual con hydroTSM ----
#análisis exploratorio y gráfico para un arroyo específico
smry(series_por_arroyo[["16"]])  # Cambiar "16" por el código del arroyo
hydroplot(series_por_arroyo[["16"]],
          var.type = "Flow",
          main = "Arroyo 16",
          pfreq = "dm")
smry(series_por_arroyo[["20"]])  
hydroplot(series_por_arroyo[["20"]],
          var.type = "Flow",
          main = "Arroyo 20",
          pfreq = "dm")
smry(series_por_arroyo[["96"]])  
hydroplot(series_por_arroyo[["96"]],
          var.type = "Flow",
          main = "Arroyo 96",
          pfreq = "dm")
smry(series_por_arroyo[["55"]])  
hydroplot(series_por_arroyo[["55"]],
          var.type = "Flow",
          main = "Arroyo 55",
          pfreq = "dm")
smry(series_por_arroyo[["69"]])  
hydroplot(series_por_arroyo[["69"]],
          var.type = "Flow",
          main = "Arroyo 69",
          pfreq = "dm")
smry(series_por_arroyo[["71"]])  
hydroplot(series_por_arroyo[["71"]],
          var.type = "Flow",
          main = "Arroyo 71",
          pfreq = "dm")
smry(series_por_arroyo[["73"]])  
hydroplot(series_por_arroyo[["73"]],
          var.type = "Flow",
          main = "Arroyo 73",
          pfreq = "dm")

#Crear columna datetime a partir de fecha y hora redondeada
TP2 <- TP1 %>%
  mutate(datetime = as.POSIXct(paste(fecha, horaredonda),
                               format = "%Y-%m-%d %H",
                               tz = "UTC") ) %>% 
  mutate(pabs = as.numeric(pabs)) %>%
  arrange(arroyo, datetime) %>% 
    select(-fechahora)

#Comparación visual entre arroyos con ggplot2
ggplot(TP2, aes(x = datetime, y = pabs, color = arroyo)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ arroyo, scales = "free_y") +
  theme_light() +
  labs(x = "Fecha", y = "Presión absoluta (pabs)", color = "Arroyo",
       title = "Evolución temporal de pabs en arroyos")

#Comparación directa de hidrogramas
ggplot(TP2, aes(x = datetime, y = pabs, color = arroyo)) +
  geom_line() +
  theme_light() +
  labs(title = "Comparación de hidrogramas", x = "Fecha", y = "pabs (proxy de caudal)")

# ---- 7. Comparación de forma de hidrogramas (ejemplo con dtw) ----
# Comparar el primer y segundo arroyo de la lista
serie16 <- coredata(series_por_arroyo[["16"]])
serie20 <- coredata(series_por_arroyo[["20"]])
serie96 <- coredata(series_por_arroyo[["96"]])
serie55 <- coredata(series_por_arroyo[["55"]])
serie69 <- coredata(series_por_arroyo[["69"]])
serie71 <- coredata(series_por_arroyo[["71"]])
serie73 <- coredata(series_por_arroyo[["73"]])

# Ajustar longitud si es necesario
min_len <- min(length(serie16), length(serie20))
serie16 <- serie16[1:min_len]
serie20 <- serie20[1:min_len]

alineacion <- dtw(serie16, serie20, keep = TRUE)

plot(alineacion, type = "twoway", main = "DTW - Comparación de hidrogramas")

# Distancia DTW como medida de similitud
cat("Distancia DTW entre los dos arroyos:", alineacion$distance, "\n")

# métricas hidrológicas más completas con hydrostats:verificar porque no reconoce la funcion calc_allHIT
metricas_hidrostats <- TP1 %>%
  group_by(arroyo) %>%
  group_map(~ {
    df <- data.frame(Date = .x$fecha, Q = .x$pabs)
    calc_allHIT(df)
  })
arroyo16 <- TP1[TP1$arroyo=="16",] %>% 
  select(fecha,pabs)

para_calc <- hidrograma_diario2 %>%
  filter(arroyo == "16") %>%
  select(fecha, pabs_media) %>%
  mutate(Date = as_date(fecha)) %>%  # convertimos a Date
  select(Date, Q = pabs_media) %>% 
  mutate(Q = round(Q))

calc_allHIT(para_calc,yearType = "calendar",stats = "all",
            digits = 1,pref="mean",drainArea = 50)

names(metricas_hidrostats) <- unique(TP1$arroyo)

#######################################################
#Perspectiva de series de tiem´p ára no perder datos
library(zoo)
library(hydrostats)
library(dtw)
library(xts)

TP2 <- TP1 %>%
  mutate(datetime = as.POSIXct(paste(fecha, horaredonda),
                               format = "%Y-%m-%d %H",
                               tz = "UTC") ) %>% 
  mutate(pabs = as.numeric(pabs)) %>%
  arrange(arroyo, datetime) %>% 
  select(-fechahora)

serie_xts <- xts(TP2$pabs, order.by = TP2$datetime)
head(serie_xts)
periodicity(serie_xts) 


