##### Penguins Monte Leon ####
rm(list = ls())
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

##############----- Penguins- Monte Leon -----##############

## Exploratorio##
#ruta_PM <- "Monte_Leon/ER 2010-2024.xlsx"

#data_PM <- read_excel(ruta_PM, sheet = "ER 2010-2024")
data_PM <- read_csv("ER 2010-2024.csv",n_max = 1222) %>% select(1:8)

data_amb <- read_csv("data_ambiental_PM_monte_leon.csv")
str(data_amb)

data_full <- data_PM %>% 
  inner_join(data_amb, by = c("season"))

head(data_full)


#info <- read_excel(ruta_PM, sheet = "info")

str(data_PM)
glimpse(data_PM)
summary(data_PM)

table(data_PM$pichon)
hist(data_PM$pichon)

# Histogramas 
data_PM %>%
  select(huevo, pichon) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~name, scales = "free") +
  geom_histogram(bins = 20, fill = "steelblue", color = "white")

#calculo de exito reproductivo #

data_ER <- data_PM %>%
  group_by(season) %>%
  summarise(
    nidos_monitoreados = n(),
    total_pichones = sum(pichon, na.rm = TRUE),
    exito_reproductivo = total_pichones / nidos_monitoreados
  ) %>%
  select(season, exito_reproductivo)


#graficos
ggplot(data_ER, aes(x = season, y = exito_reproductivo, group = 1)) +
  #geom_line(color = "#2c7fb8", size = 1) +
  geom_point(size = 3, color = "#2c7fb8") +
  labs(
    title = "Éxito reproductivo por temporada",
    x = "Temporada",
    y = "Éxito reproductivo"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data_ER, aes(x = season, y = exito_reproductivo)) +
  geom_linerange(aes(ymin = 0, ymax = exito_reproductivo), color = "#41ab5d", size = 1.2) +
  geom_point(color = "#41ab5d", size = 2) +  # opcional: marca en la punta
  labs(
    title = "Éxito reproductivo por temporada",
    x = "Temporada",
    y = "Éxito reproductivo"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### Exito reproductivo (old school) ###

library(tidyverse)
library(dplyr)

data_PM <- read_csv("ER 2010-2024.csv",n_max = 1222) %>% select(1:8)
data_PM$fecha_puesta <- as.Date(data_PM$fecha_puesta, format = "%d/%m/%Y")
data_PM$fecha_walkout <- as.Date(data_PM$fecha_walkout, format = "%d/%m/%Y")

str(data_PM)
data_PM <- data_PM %>% 
mutate(ER = pichon/huevo)

ggplot(data_PM,aes(x=ER,y=season))+
  geom_point(position = "jitter")

ERestaca <- data_PM %>% filter(season == "s10-11") %>% 
ggplot(aes(x=ER,y=estaca))+
  geom_violin(scale = "count")+
  scale_y_continuous(limits = c(0,1))
ERestaca

ggplot(data_PM, aes(x = factor(estaca), y = ER)) +
  geom_violin(fill = "darkviolet", color = "gray30",scale = "count") +
  facet_wrap(~ season, ncol = 5) +
  labs(x = "estaca", y = "ER") +
  scale_y_continuous(limits = c(0,1))+
  theme_minimal(base_size = 12)


data_ER_season <- data_PM %>%
  group_by(season) %>%
  summarise(
    nidos_monitoreados = n(),
    total_pichones = sum(pichon, na.rm = TRUE),
    exito_reproductivo = total_pichones / nidos_monitoreados,
    ER = mean(ER)
  ) %>%
  select(season,nidos_monitoreados,exito_reproductivo,ER)

data_ER_estaca_season <- data_PM %>%
  group_by(season,estaca) %>%
  summarise(
    nidos_monitoreados = n(),
    total_pichones = sum(pichon, na.rm = TRUE),
    exito_reproductivo = total_pichones / nidos_monitoreados,
    ER = mean(ER)
  ) %>%
  select(season,estaca,nidos_monitoreados,exito_reproductivo,ER)

data_ER_season
data_ER_estaca_season

ggplot(data_ER_estaca_season,aes(x=season,y=ER, group=factor(estaca),color=factor(estaca)))+
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

data_amb <- read_csv("data_ambiental_PM_monte_leon.csv")
str(data_amb)

data_full <- data_PM %>% 
  inner_join(data_amb, by = c("season"))

head(data_full)

#write.csv(data_full, "PM_Monte_Leon_full.csv")

################## Análisis Multinomial  ####################

library(glmmTMB)
library(tidyr)
library(dplyr)
library(lme4)
library(MASS)
library(effects)

#Nueva Tabla con variables ambientales
data_PM_full <- read.csv("PM_Monte_Leon_full.csv")
str(data_PM_full)

# Columna pichon, analisis multinomial numero de pichones por nido#
# Ver que pasa por season primero:
# Uso paquete MASS

# Convertir variable pichon a un factor ordenado
data_PM_full$pichon <- factor(data_PM_full$pichon, ordered = TRUE)

# Pasar season a factor (lo estaba tomando como chr por default)
data_PM_full$season <- as.factor(data_PM_full$season)

# Modelo ordinal
modelo_ordinal <- polr(pichon ~ season, data = data_PM_full, Hess = TRUE)
modelo_ordinal <- polr(pichon ~ SAM, data = data_PM_full, Hess = TRUE)
summary(modelo_ordinal)

# Obtener p-valores (no los temino de entender)
ctable <- coef(summary(modelo_ordinal))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
cbind(ctable, "p value" = p_values)

# Para visualización de probabilidades de pichon 0,1, y 2 paquete effects

plot(allEffects(modelo_ordinal)) 

table(data_PM_full$pichon,data_PM_full$season)
table(data_PM_full$season)


#### Con no linealidad ---> nos quedamos con este
library(ordinal)
library(splines)
library(ggeffects)
library(MuMIn)
library(sjPlot)

# Eliminar casos donde pichon == 3
data_filtrada <- subset(data_PM_full, pichon %in% c(0, 1, 2))
table(data_filtrada$pichon)


# Filtrar y dropear niveles
data_filtrada <- droplevels(subset(data_PM_full, pichon %in% c(0, 1, 2)))

# Confirmar que solo tenés los niveles correctos
levels(data_filtrada$pichon)

data_filtrada$pichon <- factor(data_filtrada$pichon, ordered = TRUE)


# Modelo ordinal con efecto no lineal 
# 1) Analisis de correlaciones entre variables ambientales

cor.test(data_filtrada$SST,data_filtrada$SSTA)#r=0.9
cor.test(data_filtrada$SST,data_filtrada$Chla)# r=0.5
cor.test(data_filtrada$SSTA,data_filtrada$Chla)#0.7
cor.test(data_filtrada$SAM, data_filtrada$SST)# 0.34
cor.test(data_filtrada$SAM, data_filtrada$SSTA)#0.27
cor.test(data_filtrada$SAM, data_filtrada$Chla)#0.15

#2) Modelo
modelo_no_lineal <- clm(pichon ~ ns(SST, df = 2)+ns(Chla, df = 3)+ns(SAM, df = 2), 
                        data = data_filtrada,na.action = na.fail)

summary(modelo_no_lineal)
dredge(modelo_no_lineal)
tab_model(modelo_no_lineal)


# Predicciones
library(ggeffects)
plot(ggpredict(modelo_no_lineal, terms = "SSTA [all]"))

#### GRAFICAR ####
library(ggeffects)
library(ggplot2)
plot(ggpredict(modelo_no_lineal))
# Renombrando palenes

# Obtener predicciones respetando los splines
pred_sst  <- ggpredict(modelo_no_lineal, terms = "SST [all]")
pred_chla <- ggpredict(modelo_no_lineal, terms = "Chla [all]")
pred_sam  <- ggpredict(modelo_no_lineal, terms = "SAM [all]")

# Modificar los niveles del factor que controla las facetas/categorias
pred_sst$response.level  <- factor(pred_sst$response.level,
                                   levels = c("1", "2", "3"),
                                   labels = c("0 pichón", "1 pichón", "2 pichones"))
pred_chla$response.level <- factor(pred_chla$response.level,
                                   levels = c("1", "2", "3"),
                                   labels = c("0 pichón", "1 pichón", "2 pichones"))
pred_sam$response.level  <- factor(pred_sam$response.level,
                                   levels = c("1", "2", "3"),
                                   labels = c("0 pichón", "1 pichón", "2 pichones"))

# Graficar 
plot(pred_sst)  + ggtitle("Efecto de la temperatura superficial del mar (SST)")
plot(pred_chla) + ggtitle("Efecto de la clorofila-a (Chla)")
plot(pred_sam)  + ggtitle("Efecto del índice ")









#### Multivariado ####
library(dplyr)
library(vegan)


data_multivariado <- data_amb %>%
  dplyr::select(season, SST, SSTA, Chla, SAM)

data_multivariado <- column_to_rownames(data_multivariado,'season')

str(data_multivariado)
euc <- vegdist(data_multivariado, method = "euclidean", na.rm = T)

nmds <- metaMDS(euc,k=2,trymax = 100)

nmds$stress
stressplot(nmds)
stressplot(nmds, pch = 19, p.col = "gray70", l.col = "black")
plot(nmds)

plot(nmds, type = "n")  # Gráfico vacío para personalizar
# Añadir los puntos de las muestras
points(nmds, display = "sites", pch = 21, bg = "lightblue", col = "black", cex = 1.5)

# Añadir las especies (vectores de especies)
ordisurf(nmds ~ data_multivariado$SAM, col = "red", add = TRUE)
ordisurf(nmds ~ data_multivariado$Chla, col = "darkgreen", add = TRUE)
ordisurf(nmds ~ data_multivariado$SST, col = "blue", add = TRUE)
ordisurf(nmds ~ data_multivariado$SSTA, col = "darkviolet", add = TRUE)


# Añadir etiquetas
text(nmds_result, display = "species", col = "darkred", cex = 1.2)
text(nmds, display = "sites", col = "black", pos = 4, cex = 0.9)


# Ajustar variables ambientales al NMDS
fit <- envfit(nmds, data_multivariado, perm = 999, na.rm=T)

# Extraer coordenadas de sitios
sites <- as.data.frame(scores(nmds, display = "sites"))
sites$season <- rownames(sites)

# Extraer vectores de variables ambientales
species <- as.data.frame(scores(fit, display = "vectors"))
species <- cbind(species, Variable = rownames(species))

# Plot con flechas
ggplot() +
  geom_point(data = sites, aes(x = NMDS1, y = NMDS2),
             color = "gray20", size = 3, alpha = 0.8) +
  geom_text(data = sites, aes(x = NMDS1, y = NMDS2, label = season),
            vjust = -0.7, size = 3, color = "gray20") +
  geom_segment(data = species,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25,"cm")),
               color = "darkviolet", linewidth = 0.7) +
  geom_text(data = species,
            aes(x = NMDS1, y = NMDS2, label = Variable),
            color = "darkviolet", vjust = -0.7, size = 3.5) +
  theme_minimal(base_size = 14) +
  labs(title = "NMDS con variables ambientales",
       x = "NMDS1", y = "NMDS2")
