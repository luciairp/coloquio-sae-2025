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
##########################################################
#MANOVA
#sigue a https://www.r-bloggers.com/2021/11/manovamultivariate-analysis-of-variance-using-r/
##https://www.datanovia.com/en/lessons/one-way-manova-in-r/
#no se cumple ningún supuesto para sorpresa de nadie pero se especifica lo hecho

#normalidad univariada de las variables respuestas 

shapiro.test(data_diaria$temp_media_diaria)
shapiro.test(data_diaria$pabs_media_diaria)
shapiro.test(data$temp_media_diaria)
shapiro.test(data$pabs_media_diaria)
shapiro.test(TP_hora$pabs)
shapiro.test(TP_hora$temp)

datos <- TP_hora %>%
  select(pabs, temp, manejo, arroyo, season)

data <- TP1 %>%
  select(season, manejo, fecha, arroyo, pabs, temp)%>%
  group_by(fecha, arroyo, manejo, season) %>%
  summarise(
    temp_media_diaria = mean(temp, na.rm = TRUE),
    pabs_media_diaria = mean(pabs, na.rm = TRUE),
    .groups = "drop"
  )

#Anderson-Darling normality test para mas de 5000 observaciones
ad.test(TP_hora$temp)
ad.test(TP_hora$pabs)

ad.test(TP1$pabs)
ad.test(TP1$temp)

ad.test(data$pabs_media_diaria)
ad.test(data$temp_media_diaria)

hist(TP1$temp, breaks = 50, col = "blue")
qqnorm(TP1$temp); qqline(TP1$temp, col = "red")
hist(TP1$pabs, breaks = 50, col = "blue")
qqnorm(TP1$pabs); qqline(TP1$pabs, col = "red")

hist(TP_hora$temp, breaks = 50, col = "lightblue")
qqnorm(TP_hora$temp); qqline(TP$temp, col = "red")
hist(TP_hora$pabs, breaks = 50, col = "lightblue")
qqnorm(TP_hora$pabs); qqline(TP$pabs, col = "red")

#para verificar los grupos
datos %>%
  group_by(season) %>%
  summarise(n = n())
datos %>%
  group_by(manejo) %>%
  summarise(n = n())
datos %>%
  group_by(manejo,season) %>%
  summarise(n = n())

datos %>%
  group_by(manejo) %>%
  get_summary_stats(pabs, temp, type = "mean_sd")
datos %>%
  group_by(manejo,season) %>%
  get_summary_stats(pabs, temp, type = "mean_sd")

datos %>% 
  group_by(manejo) %>%  
  shapiro_test(pabs, temp)

datos %>% 
  group_by(season) %>%  
  shapiro_test(pabs, temp)

datos %>% 
  group_by(manejo,season) %>%  
  shapiro_test(pabs, temp)


#normalidad multivariada 
mardia(TP_hora[, c("pabs", "temp")])$mv.test
mardia(TP1[, c("pabs", "temp")])$mv.test #este da un vector de 33,8Gb por eso se trabaja con la matriz e una medición por hora
mardia(data_diaria[, c("pabs_media_diaria", "temp_media_diaria")])$mv.test
mardia(data[, c("pabs_media_diaria", "temp_media_diaria")])$mv.test

#homogeneidad de varianzas 
library(rstatix)
box_m(datos[, c("pabs", "temp")], group = datos$manejo)
box_m(datos[, c("pabs", "temp")], group = datos$season)
#no cumple

#modelo manova
mod1 <- manova(cbind(pabs, temp) ~ manejo, data = TP1)
summary(mod1, test = "Pillai") #pillai es mas rousto al no cumplimiento de supuestos
mod2 <- manova(cbind(pabs, temp) ~ season, data = TP1)
summary(mod2, test = "Pillai")
mod3 <- manova(cbind(pabs, temp) ~ manejo * season, data = TP1)
summary(mod3, test = "Pillai")

summary.aov(mod3) #analisis univariado post manova, para pabs y temp por separado

mod4 <- manova(cbind(pabs, temp) ~ manejo, data = TP_hora)
summary(mod4, test = "Pillai") #pillai es mas rousto al no cumplimiento de supuestos
mod5 <- manova(cbind(pabs, temp) ~ season, data = TP_hora)
summary(mod5, test = "Pillai")
mod6 <- manova(cbind(pabs, temp) ~ manejo * season, data = TP_hora)
summary(mod6, test = "Pillai")

summary.aov(mod6) #analisis univariado post manova, para pabs y temp por separado

mod7 <- manova(cbind(pabs_media_diaria, temp_media_diaria) ~ manejo, data = data)
summary(mod7, test = "Pillai") #pillai es mas rousto al no cumplimiento de supuestos
mod8 <- manova(cbind(pabs_media_diaria, temp_media_diaria) ~ season, data = data)
summary(mod8, test = "Pillai")
mod9 <- manova(cbind(pabs_media_diaria, temp_media_diaria) ~ manejo * season, data = data)
summary(mod9, test = "Pillai")

summary.aov(mod9) #analisis univariado post manova, para pabs y temp por separado
###############################################################################################################

