
library(tidyverse)

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
