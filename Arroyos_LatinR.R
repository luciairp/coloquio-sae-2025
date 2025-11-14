
#Set de datos HOBO de siete arroyos de enero del 2018 a febrero del 2019
#datos de temperatura del agua y presión absoluta, un dato por hora.
#se asume presión atmosférica homogénea en los 7 sitios
#la diferencia de presión se dará por la diferencia del nivel de agua

#Análisis Descriptivo de Temp y Presión
rm(list = ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(stringr)
library(ggpubr)
library(car)
library(broom)
library(rstatix)
library(nortest)
library(mvnormalTest)

data_arroyos <- read_csv2("P_T_todos_MEL.csv")
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
  mutate(horaredonda = hour(fechahora)) %>% 
  mutate(season= case_when(
    month(fechahora) < 5 | month(fechahora) > 10 ~ 'templado',
    #fecha < "01/05/2018" | fecha >"31/10/2018" ~ 'templado',
    .default = 'frio'  
  )) %>% 
  mutate(manejo = factor(manejo,c("sin","con")))

head(TP)
#para verificar que haya tomado bien los niveles de season
TP$season <- factor(TP$season)
class(TP$season)
levels(TP$season)

TP$arroyo<-factor(TP$arroyo, levels=c("16","20","96","55","69","71","73"))

###############################################################################
#meses core de frio y de verano 
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
###############################################################################

#histogramas
hist(TP$pabs, main = "Distribución de presión", xlab = "pabs", col = "skyblue")
hist(TP$temp, main = "Distribución de temperatura", xlab = "temp", col = "salmon")

hist(TP1$temp, breaks = 50, col = "blue")
qqnorm(TP1$temp); qqline(TP1$temp, col = "red")
hist(TP1$pabs, breaks = 50, col = "blue")
qqnorm(TP1$pabs); qqline(TP1$pabs, col = "red")

hist(TP_hora$temp, breaks = 50, col = "lightblue")
qqnorm(TP_hora$temp); qqline(TP$temp, col = "red")
hist(TP_hora$pabs, breaks = 50, col = "lightblue")
qqnorm(TP_hora$pabs); qqline(TP$pabs, col = "red")

hist <- ggplot(TP,aes(x=temp))+
  geom_histogram()+
  facet_wrap(vars(arroyo))
hist

hist2 <- ggplot(TP,aes(x=pabs))+
  geom_histogram()+
  facet_wrap(vars(arroyo))
hist2

#presion vs temp
ggplot(TP, aes(x = temp, y = pabs)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre temperatura y presión")

## Calculo temp y presion diaria (independiente de arroyo,está separado por manejo)
data_diaria <- TP %>%
  group_by(fecha, manejo) %>%
  summarise(
    temp_media_diaria = mean(temp, na.rm = TRUE),
    pabs_media_diaria = mean(pabs, na.rm = TRUE)
  )

head(data_diaria)

#CORRELACIÓN PEARSON TvsP
cor.test(TP$temp, TP$pabs)
str(TP$temp)
str(TP$pabs)

#Temp media diaria vs presion media diaria
ggplot(data_diaria, aes(x = temp_media_diaria, y = pabs_media_diaria)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relación entre temperatura y presión")

cor.test(data_diaria$temp_media_diaria, data_diaria$pabs_media_diaria)
str(data_diaria$temp_media_diaria)
str(data_diaria$pabs_media_diaria)

# Temp a lo largo del año
ggplot(data_diaria, aes(x = fecha, y = temp_media_diaria, color = manejo)) +
  geom_line(size = 1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = as.Date(c("2018-01-01", "2019-02-28")),
    expand = c(0, 0)
  ) +
  labs(
    title = "Temperatura media diaria ",
    x = "Fecha",
    y = "Temp media (°C)",
    color = "Manejo"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# presion a lo largo del year
ggplot(data_diaria, aes(x = fecha, y = pabs_media_diaria, color = manejo)) +
  geom_line(size = 1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = as.Date(c("2018-01-01", "2019-02-28")),
    expand = c(0, 0)
  ) +
  labs(
    title = "Presion media diaria ",
    x = "Fecha",
    y = "Presión ",
    color = "Manejo"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##qqplots

temperatura <- ggplot(TP,aes(y=temp,group = arroyo,fill=manejo))+
  geom_boxplot()+
  theme_light()
temperatura

presion <- ggplot(TP,aes(y=pabs,group = arroyo, fill=manejo))+
  geom_boxplot()+
  theme_light()
presion

temperatura2 <- ggplot(TP1,aes(x = season, y=temp,fill=manejo))+
  geom_boxplot()+
  theme_light()
temperatura2

presion2 <- ggplot(TP1,aes(x = season, y=pabs, fill=manejo))+
  geom_boxplot()+
  theme_light()
presion2

temperatura1 <- ggplot(TP_hora,aes(x = season, y=temp,fill=manejo))+
  geom_boxplot()+
  theme_light()
temperatura1

presion1 <- ggplot(TP_hora,aes(x = season, y=pabs, fill=manejo))+
  geom_boxplot()+
  theme_light()
presion1

#########################
##me quedo solo con una hora de medicion por dia en todos los arroyos (las 00 horas)
TP_hora <- TP1 %>%
  filter(horaredonda == 0)

head(TP_hora)
levels(TP_hora$season)
TP_hora$season <- factor(TP_hora$season,
                         levels = c("core_templado","templado","core_frio","frio"))
######################
#elipses todas las horas
biplot <- ggplot(TP,aes(y=temp,x=pabs,colour = arroyo, z=arroyo))+
  stat_ellipse(aes(group = arroyo, color = manejo), level=0.40)+
  theme_light()
biplot

biplot_1 <- ggplot(TP, aes(x = pabs, y = temp, colour = manejo)) +
  stat_ellipse(aes(group = arroyo, color = manejo), level = 0.40)+
  facet_grid(.~season) +
  theme_light()
biplot_1

biplot_11 <- ggplot(TP1, aes(x = pabs, y = temp, colour = manejo)) +
  stat_ellipse(aes(group = arroyo, color = manejo), level = 0.40)+
  facet_grid(.~season) +
  theme_light()
biplot_11

#elipses solo con dato de las 00 h 
biplot_0 <- ggplot(TP_hora,aes(y=temp,x=pabs,colour = arroyo, z=arroyo))+
  stat_ellipse(aes(group = arroyo, color = manejo), level=0.40)+
  theme_light()
biplot_0

biplot_00 <- ggplot(TP_hora, aes(x = pabs, y = temp, colour = manejo)) +
  stat_ellipse(aes(group = arroyo, color = manejo), level = 0.40)+
  facet_grid(.~season) +
  theme_light(base_size = 13)+
  labs(
    x= "Presión Absoluta (hPa)",
    y= "Temperatura del agua (°C)")+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )
biplot_00

#elipses con datos de solo una hora y para las estaciones core frio y core templado
TP_hora.2 <- TP_hora %>%
  filter(season %in% c("core_frio", "core_templado"))

biplot_000 <- ggplot(TP_hora.2, aes(x = pabs, y = temp, colour = manejo)) +
  stat_ellipse(aes(group = arroyo, color = manejo), level = 0.40)+
  facet_grid(.~season) +
  theme_light(base_size = 13)+
  labs(
    x= "Presión Absoluta (hPa)",
    y= "Temperatura del agua (°C)")+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right"
  )
biplot_000

#####elipses por arroyo
ellipses<-ggplot(TP,aes(y=temp,x=pabs,group = arroyo,color=arroyo))+
  stat_ellipse()+
  theme_light()
ellipses

ellipse_month<-ggplot(TP1, aes(y=temp, x=pabs, group=arroyo, color=manejo))+
  stat_ellipse(level=.95)+
  facet_wrap(vars(month(fechahora)))+
  theme_light()
ellipse_month

###############################################################################################################
## CODIGO DE SIBER ACTUALIZADO DE: 
##https://cran.r-project.org/web/packages/SIBER/vignettes/siber-comparing-populations.html

# -----------------------------
# 1. 
# -----------------------------

rm(list=ls())
graphics.off()

set.seed(123)

library(SIBER)
library(ggplot2)
#library(magrittr) # to enable piping
library(dplyr)
#library(rjags)
#library(mvtnorm)

# load in the included demonstration dataset
data_arroyos<- read_csv2("P_T_todos_MEL.csv")
#mydata2<-dplyr::filter(mydata, group %in% c("2014", "2015","2016","2017"))
#str(mydata2)
data_arroyos$arroyo<-factor(data_arroyos$arroyo, levels=c("16","20","96","55","69","71","73"))

mydata <- data_arroyos %>%
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


# Seleccionar solo las columnas de interés y renombrarlas
mydata2 <- mydata %>%
  filter(horaredonda == 0) %>% 
  filter(season == "core_templado") %>% 
  select(iso1 = pabs, iso2 = temp, group = arroyo)

mydata3 <- mydata %>%
  filter(horaredonda == 0) %>% 
  filter(season == "core_frio") %>% 
  select(iso1 = pabs, iso2 = temp, group = arroyo)

# Agregar una nueva columna llamada "community" con valor 1
mydata2 <- mydata2 %>%
  mutate(community = "all")

mydata2 <- mydata2 %>%
  mutate(
    community = case_when(
      group %in% c("16", "20", "96") ~ "sin_manejo",
      group %in% c("55", "69", "71", "73") ~ "con_manejo",
      TRUE ~ NA_character_   # por si hay otros grupos no clasificados
    )
  )
mydata2<-as.data.frame(mydata2)

mydata2$group<-as.factor(mydata2$group)
mydata2$community<-as.factor(mydata2$community)

#ahora cor-frio
mydata3 <- mydata3 %>%
  mutate(community = "all")

mydata3 <- mydata3 %>%
  mutate(
    community = case_when(
      group %in% c("16", "20", "96") ~ "sin_manejo",
      group %in% c("55", "69", "71", "73") ~ "con_manejo",
      TRUE ~ NA_character_   # por si hay otros grupos no clasificados
    )
  )
mydata3<-as.data.frame(mydata3)

mydata3$group<-as.factor(mydata3$group)
mydata3$community<-as.factor(mydata3$community)

# -----------------------------
# 2. Crear el objeto SIBER
# -----------------------------
# create the siber object
siber.data<- createSiberObject(mydata2)

siber.data.3<- createSiberObject(mydata3)

# Calculate sumamry statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber.data)
print(group.ML)

community.ML <- communityMetricsML(siber.data)
print(community.ML)

#core frio
group.ML.3 <- groupMetricsML(siber.data.3)
print(group.ML.3)

community.ML.3 <- communityMetricsML(siber.data.3)
print(community.ML.3)

#graficar las metricas de comunidad
#convertrir la tabla a formato largo
library(tidyverse)

# Convertimos la matriz de core_templado a data frame largo
comm_df <- as.data.frame(community.ML) %>%
  rownames_to_column("metric") %>%
  pivot_longer(cols = -metric, names_to = "community", values_to = "value")

comm_df


ggplot(comm_df, aes(x = community, y = value, fill = community)) +
  geom_col(width = 0.6, color = "black") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = c("tan3", "forestgreen")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Comparación de métricas SIBER por tipo de manejo CT",
    x = "Tipo de manejo",
    y = "Valor de la métrica"
  ) +
  theme(legend.position="none")

# Convertimos la matriz de core_frio a data frame largo
comm_df.3 <- as.data.frame(community.ML.3) %>%
  rownames_to_column("metric") %>%
  pivot_longer(cols = -metric, names_to = "community", values_to = "value")

comm_df.3


ggplot(comm_df.3, aes(x = community, y = value, fill = community)) +
  geom_col(width = 0.6, color = "black") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = c("tan3", "forestgreen")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Comparación de métricas SIBER por tipo de manejo CF",
    x = "Tipo de manejo",
    y = "Valor de la métrica"
  ) +
  theme(legend.position="none")


# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = NULL, lty = 1, lwd = 2)

group.hull.args      <- list(lty = 2)

par(mfrow=c(1,1))

plotSiberObject(siber.data,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2)
)
# -----------------------------
# 3. Graficar los datos y las elipses
# -----------------------------
plotSiberObject(
  siber.data,
  ax.pad = 1,
  hulls = FALSE,   # no mostrar convex hulls
  ellipses = TRUE, # mostrar las elipses SEA
  group.hulls.args = list(col = "gray"),
  ellipses.args = list(n = 100, lwd = 2)
)
title("Espacio temperatura-presión (elipses SIBER)")

# -----------------------------
# 4. Calcular métricas de elipses SEA y SEAB
# -----------------------------
# SEA (Standard Ellipse Area)
#SEA <- siberEllipses(siber.data)
#SEA
#tapply(siber.data$original.data$iso1, siber.data$original.data$group, sd)
#tapply(siber.data$original.data$iso2, siber.data$original.data$group,sd)


# Definir los priors (según recomendación del paquete SIBER)
priors <- list(
  R = 1 * diag(2),   # matriz de covarianza inicial
  k = 2,              # número de dimensiones
  tau.mu = 1.0E-3     # precisión del prior para la media
)
# Parámetros MCMC para la simulación bayesiana
parms <- list(
  n.iter = 2 * 10^4,  # número total de iteraciones
  n.burnin = 1 * 10^3, # iteraciones de burn-in
  n.thin = 10,         # frecuencia de muestreo
  n.chains = 2          # número de cadenas
)

# Ajustar modelo bayesiano para core_templado
ellipses.post.ct <- siberMVN(siber.data, parms, priors)

# Ajustar modelo bayesiano para core_frio
ellipses.post.cf <- siberMVN(siber.data.3, parms, priors)

# Core templado
SEA.B.ct <- siberEllipses(ellipses.post.ct)
SEA.B.ct.summary <- siberDensityPlot(SEA.B.ct, xticklabels = levels(mydata2$group), 
                                     xlab = "Arroyos", ylab = "SEA (bayesiana)",
                                     main = "core_templado")
SEA.B.ct.summary

# Core frío
SEA.B.cf <- siberEllipses(ellipses.post.cf)
SEA.B.cf.summary <- siberDensityPlot(SEA.B.cf, xticklabels = levels(mydata3$group),
                                     xlab = "Arroyos", ylab = "SEA (bayesiana)",
                                     main = "core_frío")
SEA.B.cf.summary

colnames(SEA.B.ct) <- c("16","20","96","55","69","71","73")
colnames(SEA.B.cf) <- c("16","20","96","55","69","71","73")

#Comparar SEA entre comunidades (sin vs con manejo)
# Definir a qué comunidad pertenece cada arroyo
sin_manejo <- c("16", "20", "96")
con_manejo <- c("55", "69", "71", "73")

# Calcular promedio de SEA por comunidad en cada iteración
SEA.B.ct.sin <- rowMeans(SEA.B.ct[, sin_manejo])
SEA.B.ct.con <- rowMeans(SEA.B.ct[, con_manejo])

SEA.B.cf.sin <- rowMeans(SEA.B.cf[, sin_manejo])
SEA.B.cf.con <- rowMeans(SEA.B.cf[, con_manejo])

# Probabilidad bayesiana de que SEA(sin) > SEA(con)
SEA.B.ct.prob <- mean(SEA.B.ct.sin > SEA.B.ct.con)
SEA.B.cf.prob <- mean(SEA.B.cf.sin > SEA.B.cf.con)

cat("Probabilidad SEA(sin) > SEA(con) - Core templado:", SEA.B.ct.prob, "\n")
cat("Probabilidad SEA(sin) > SEA(con) - Core frío:", SEA.B.cf.prob, "\n")

# ------------------------------------------------------------
# 6. Visualización de distribuciones SEA bayesianas
# ------------------------------------------------------------
# Combinar en un data frame largo para graficar
SEA.all <- bind_rows(
  data.frame(SEA = SEA.B.ct.sin, Manejo = "sin_manejo", Estacion = "core_templado"),
  data.frame(SEA = SEA.B.ct.con, Manejo = "con_manejo", Estacion = "core_templado"),
  data.frame(SEA = SEA.B.cf.sin, Manejo = "sin_manejo", Estacion = "core_frio"),
  data.frame(SEA = SEA.B.cf.con, Manejo = "con_manejo", Estacion = "core_frio")
)

# Gráfico comparativo
ggplot(SEA.all, aes(x = Manejo, y = SEA, fill = Manejo)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Estacion, scales = "free_y") +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("tan3", "forestgreen")) +
  labs(y = "Área elíptica estándar (SEA.B)",
    x = "Tipo de manejo")+
  theme(legend.position = "none")


# 5. Opcional: modelo bayesiano (MCMC)
# -----------------------------
ellipses.posterior <- siberMVN(siber.data)
SEA.B <- siberEllipses(ellipses.posterior)
apply(SEA.B, 2, mean)

# -----------------------------
# 6. Interpretación
# -----------------------------
# - Cada elipse representa la dispersión bivariada (temperatura-presión) de un grupo.
# - El área de la elipse (SEA) mide cuán amplio es el rango conjunto de T y P.
# - Comparar SEA entre grupos permite ver cuál tiene mayor "espacio ambiental ocupado".
#################################################################################

library(tidyverse)

# Convertimos ambas matrices en data frames largos
templada_df <- as.data.frame(community.ML) %>%
  rownames_to_column("metric") %>%
  pivot_longer(cols = -metric, names_to = "community", values_to = "value") %>%
  mutate(station = "templada")

fria_df <- as.data.frame(community.ML.3) %>%
  rownames_to_column("metric") %>%
  pivot_longer(cols = -metric, names_to = "community", values_to = "value") %>%
  mutate(station = "fria")

# Unimos ambas estaciones
all_df <- bind_rows(templada_df, fria_df)

# Revisamos
head(all_df)

ggplot(all_df %>% filter(metric %in% c("TA", "SEA")), 
       aes(x = community, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7), color = "black") +
  facet_wrap(~ station) +
  scale_fill_manual(values = c("TA" = "steelblue", "SEA" = "skyblue")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparación de TA y SEA por comunidad y estación",
    x = "Tipo de manejo forestal",
    y = "Área",
    fill="Métrica"
  )

