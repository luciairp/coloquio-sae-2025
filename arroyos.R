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
  mutate(horaredonda = hour(fechahora)) %>% 
  mutate(season= case_when(
    month(fechahora) < 9 & month(fechahora) > 5 ~ 'core_frio',
    month(fechahora) < 2 | month(fechahora) > 11 ~ 'core_templado'
    #fecha < "01/05/2018" | fecha >"31/10/2018" ~ 'templado',
  #.default = '...'  
  ))%>% 
  mutate(manejo=factor(manejo, c("sin","con")))%>% 
  mutate(month=month(fecha,label = TRUE, abbr = TRUE))


head(TP)

TP$arroyo<-factor(TP$arroyo,levels = c("16","20","96","55","69","71","73"))

#write.csv(TP, "TP_edit.csv")

# x es QM , y es TP
# quiero traer a Qm las mediciones que cumplen ser iguales en fecha y arroyo, para 
resultado <- Qm %>%
  inner_join(TP, by = c("arroyo", "fecha")) %>%
  filter(horaredonda >= 11, horaredonda <= 13)
resultado$arroyo <- as.factor(resultado$arroyo)

qvsp <- ggplot(resultado,aes(x=pabs,y=q,color=arroyo))+
  geom_point()+
  facet_wrap(vars(season))
qvsp

TP$arroyo <- factor(TP$arroyo,levels=c("16","20","96","55","69","71","73"))
qvst <- ggplot(resultado,aes(x=temp,y=q,color=arroyo))+
  geom_point()
qvst

plot <- ggplot(TP,aes(y=temp,group = arroyo))+
  geom_boxplot()
plot

temperatura <- ggplot(TP,aes(y=temp,group=arroyo, fill=manejo))+
  geom_boxplot()+
  theme_light()

presion <- ggplot(TP,aes(y=pabs,group=arroyo, fill=manejo))+
  geom_boxplot()+
  theme_light()
 

biplot <- ggplot(TP,aes(y=temp,x=pabs,colour = arroyo, z=arroyo))+
  stat_ellipse(aes(group = arroyo, color = manejo))+
  theme_light()
biplot

ellipses<-ggplot(TP,aes(y=temp,x=pabs,group = arroyo,color=arroyo))+
stat_ellipse()+
  theme_light()

ellipse_month<-ggplot(TP, aes(y=temp, x=pabs, group=arroyo, color=manejo))+
  stat_ellipse(level=.95)+
  facet_wrap(vars(season))+
  theme_light()

