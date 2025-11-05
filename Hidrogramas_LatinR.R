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

#Matriz de distancias DTW entre todos los arroyos
# Crear lista de series por arroyo
series <- hidrograma_diario %>%
  group_by(arroyo) %>%
  summarise(pabs_media = list(pabs_media)) %>%
  deframe()  # convierte en lista nombrada

# Crear una matriz vacía
n <- length(series)
dtw_matrix <- matrix(NA, nrow = n, ncol = n,
                     dimnames = list(names(series), names(series)))

# Calcular distancias DTW pareadas
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      d <- dtw::dtw(series[[i]], series[[j]])$distance
      dtw_matrix[i, j] <- d
      dtw_matrix[j, i] <- d
    }
  }
}

dtw_matrix

library(reshape2)

dtw_melt <- melt(dtw_matrix, na.rm = TRUE)

ggplot(dtw_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  theme_minimal(base_size = 13) +
  labs(title = "Similitud de hidrogramas (Distancia DTW)",
       x = "Arroyo", y = "Arroyo", fill = "Distancia DTW")

# Convertir distancia en similitud (1 = idénticos, 0 = muy diferentes)
dtw_similarity <- 1 - (dtw_matrix / max(dtw_matrix, na.rm = TRUE))

hc <- hclust(as.dist(dtw_matrix), method = "average")
plot(hc, main = "Clustering de arroyos según hidrogramas DTW")

###########esto funciona bien###############
#Métricas hidrológicas para cada arroyo ----
metricas_por_arroyo <- TP1 %>%
  group_by(arroyo) %>%
  summarise(
    mean_pabs = mean(pabs, na.rm = TRUE),
    sd_pabs   = sd(pabs, na.rm = TRUE),
    min_pabs  = min(pabs, na.rm = TRUE),
    max_pabs  = max(pabs, na.rm = TRUE),
    cv_pabs   = sd_pabs / mean_pabs,
    rango     = max_pabs - min_pabs
  )

print(metricas_por_arroyo)

##############
# Calcular estadísticos básicos

pabs <- "pabs"

# a) Máximos y mínimos diarios
diario <- TP1 %>%
  group_by(arroyo, manejo, fecha) %>%
  summarise(
    max_pabs = max(.data[[pabs]], na.rm = TRUE),
    min_pabs = min(.data[[pabs]], na.rm = TRUE),
    mean_pabs = mean(.data[[pabs]], na.rm = TRUE),
    .groups = "drop"
  )

# b) Diferencias horarias (tasa de cambio)
TP1 <- TP1 %>%
  group_by(arroyo) %>%
  arrange(fechahora) %>%
  mutate(dpabs = c(NA, diff(.data[[pabs]]))) %>%
  ungroup()

# 4. Detección automática de eventos de crecida

percentil_umbral <- 0.75
duracion_min_h <- 2

# Definir umbral por arroyo (percentil 95)
umbral <- TP1 %>%
  group_by(arroyo) %>%
  summarise(threshold = quantile(pabs, percentil_umbral, na.rm = TRUE))

#dejo las dos opciones de mutate porque a veces me falla una y no toma a threshold como variable
TP1 <- TP1 %>%
  left_join(umbral, by = "arroyo") %>%
  mutate(sobre_umbral = pabs > threshold)

TP1 <- left_join(TP1, umbral, by = "arroyo")
TP1 <- TP1 %>%
  mutate(sobre_umbral = pabs > threshold)


# 5. Detectar eventos de crecida continuos (≥3 horas sobre umbral)

TP1_dt <- as.data.table(TP1)
TP1_dt[, evento_id := rleid(sobre_umbral), by = arroyo]
eventos_tabla <- TP1_dt[sobre_umbral == TRUE, .(
  inicio = min(fechahora),
  fin = max(fechahora),
  duracion_h = as.numeric(difftime(max(fechahora), min(fechahora), units = "hours")),
  pico = max(get("pabs"), na.rm = TRUE)
), by = .(arroyo, manejo, evento_id)]


# Filtrar eventos cortos (< 3 horas)
eventos_filtrados <- eventos_tabla[duracion_h >= duracion_min_h]

# 6. Graficar resultados
# Serie temporal con umbral y eventos detectados
ggplot(TP1, aes(x = fechahora, y = .data[[pabs]], color = manejo)) +
  geom_line(alpha = 0.7) +
  geom_hline(aes(yintercept = threshold), linetype = "dashed", color = "red") +
  geom_point(
    data = eventos_filtrados,
    aes(x = inicio, y = pico),
    color = "black", size = 2
  ) +
  facet_wrap(~ arroyo, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Eventos de crecida detectados automáticamente",
    x = "Fecha-Hora", y = "pabs"
  )

# 7. Comparación entre grupos (manejo)

# Resumen por manejo
comparacion <- diario %>%
  group_by(manejo) %>%
  summarise(
    media_max = mean(max_pabs, na.rm = TRUE),
    cv_max = sd(max_pabs, na.rm = TRUE) / mean(max_pabs, na.rm = TRUE),
    media_min = mean(min_pabs, na.rm = TRUE),
    .groups = "drop"
  )

# Test t de diferencias entre grupos (máximos diarios)
t.test(max_pabs ~ manejo, data = diario)

install.packages("effectsize")
library(effectsize)
t_test_res <- t.test(max_pabs ~ manejo, data = diario)
cohens_d(max_pabs ~ manejo, data = diario)

ggplot(diario, aes(x = manejo, y = max_pabs, fill = manejo)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Comparación de máximos diarios de pabs por manejo",
       y = "Máximo diario de pabs", x = "Tipo de manejo")

###############################################
###analisis con un dato diario y mensual
library(dplyr)
library(data.table)
library(ggplot2)
library(effectsize)

pabs <- "pabs"
percentil_umbral <- 0.75   # umbral más sensible
duracion_min_d <- 2        # duración mínima en días

# --- 1. Promedio diario por arroyo y manejo ---
data_diaria <- TP1 %>%
  group_by(arroyo, manejo, fecha) %>%
  summarise(
    mean_pabs = mean(.data[[pabs]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 2. Umbral por arroyo (percentil 70 de los promedios diarios) ---
umbral_dia <- data_diaria %>%
  group_by(arroyo) %>%
  summarise(threshold = quantile(mean_pabs, percentil_umbral, na.rm = TRUE))

# --- 3. Detección de eventos de crecida diaria ---
data_diaria <- data_diaria %>%
  left_join(umbral_dia, by = "arroyo") %>%
  mutate(sobre_umbral = mean_pabs > threshold)

data_diaria_dt <- as.data.table(data_diaria)
data_diaria_dt[, evento_id := rleid(sobre_umbral), by = arroyo]

eventos_diarios <- data_diaria_dt[sobre_umbral == TRUE, .(
  inicio = min(fecha),
  fin = max(fecha),
  duracion_d = as.numeric(difftime(max(fecha), min(fecha), units = "days")),
  pico = max(mean_pabs, na.rm = TRUE)
), by = .(arroyo, manejo, evento_id)]

# Filtrar eventos cortos (< 2 días)
eventos_diarios_filtrados <- eventos_diarios[duracion_d >= duracion_min_d]

# --- 4. Graficar resultados diarios ---
ggplot(data_diaria, aes(x = fecha, y = mean_pabs, color = manejo)) +
  geom_line(alpha = 0.7) +
  geom_hline(aes(yintercept = threshold), linetype = "dashed", color = "red") +
  geom_point(data = eventos_diarios_filtrados,
             aes(x = inicio, y = pico),
             color = "black", size = 2) +
  facet_wrap(~ arroyo, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Eventos de crecida detectados (datos diarios)",
    x = "Fecha", y = "pabs promedio diario"
  )

# --- 5. Comparación entre manejos (diaria) ---
t.test(mean_pabs ~ manejo, data = data_diaria)
cohens_d(mean_pabs ~ manejo, data = data_diaria)

#############################
pabs <- "pabs"
percentil_umbral <- 0.50   # umbral más sensible
duracion_min_d <- 1        # duración mínima en meses
# --- 1. Promedio mensual ---
data_mensual <- TP1 %>%
  mutate(mes = format(fechahora, "%Y-%m")) %>%
  group_by(arroyo, manejo, mes) %>%
  summarise(
    mean_pabs = mean(.data[[pabs]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 2. Umbral mensual (percentil 75) ---
umbral_mes <- data_mensual %>%
  group_by(arroyo) %>%
  summarise(threshold = quantile(mean_pabs, percentil_umbral, na.rm = TRUE))

# --- 3. Detección de eventos de crecida mensual ---
data_mensual <- data_mensual %>%
  left_join(umbral_mes, by = "arroyo") %>%
  mutate(sobre_umbral = mean_pabs > threshold)

data_mensual_dt <- as.data.table(data_mensual)
data_mensual_dt[, evento_id := rleid(sobre_umbral), by = arroyo]

eventos_mensuales <- data_mensual_dt[sobre_umbral == TRUE, .(
  inicio = min(mes),
  fin = max(mes),
  duracion_m = as.numeric(difftime(as.Date(paste0(max(mes), "-01")),
                                   as.Date(paste0(min(mes), "-01")),
                                   units = "days")) / 30,
  pico = max(mean_pabs, na.rm = TRUE)
), by = .(arroyo, manejo, evento_id)]

# Filtrar eventos cortos (< 2 meses)
eventos_mensuales_filtrados <- eventos_mensuales[duracion_m >= 2]

# --- 4. Graficar resultados mensuales ---
ggplot(data_mensual, aes(x = as.Date(paste0(mes, "-01")), y = mean_pabs, color = manejo)) +
  geom_line(alpha = 0.7) +
  geom_hline(aes(yintercept = threshold), linetype = "dashed", color = "red") +
  geom_point(data = eventos_mensuales_filtrados,
             aes(x = as.Date(paste0(inicio, "-01")), y = pico),
             color = "black", size = 2) +
  facet_wrap(~ arroyo, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Eventos de crecida detectados (datos mensuales)",
    x = "Mes", y = "pabs promedio mensual"
  )

# --- 5. Comparación entre manejos (mensual) ---
t.test(mean_pabs ~ manejo, data = data_mensual)
cohens_d(mean_pabs ~ manejo, data = data_mensual)
########
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
#Perspectiva de series de tiempo para no perder datos
library(zoo)
library(hydrostats)
library(dtw)
library(xts)
library(pracma)       # findpeaks



