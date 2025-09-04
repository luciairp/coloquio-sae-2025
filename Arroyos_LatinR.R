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
  theme_light()
biplot_00

TP_hora <- TP_hora %>%
  filter(season != "medio")
biplot_000 <- ggplot(TP_hora, aes(x = pabs, y = temp, colour = manejo)) +
  stat_ellipse(aes(group = arroyo, color = manejo), level = 0.40)+
  facet_grid(.~season) +
  theme_light()
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

rm(list=ls())
graphics.off()
