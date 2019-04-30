# Packages ####
library(tidyverse)
library(readxl)
library(RColorBrewer)
library(gganimate)

# Data - OECD countries vector ####
oecd <- c("Austria","Belgium","Canada","Chile","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea","Latvia","Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States") %>% 
  as.data.frame() %>% 
  mutate(oecd="OECD") %>% 
  select("country"=1, "oecd"=2)
# Data - LAC vector ####
lac <- c("Argentina", "Brazil","Bolivia", "Colombia", "Ecuador", "Peru", "Mexico", "Uruguay", "Paraguay", "Venezuela", "Chile", "Costa Rica", "Honduras", "Nicaragua", "Dominican Republic") %>%
  as.data.frame() %>% 
  mutate(lac="LAC") %>% 
  select("country"=1, "lac"=2)

# SWIID Solt (2019) ####
read.csv("https://raw.githubusercontent.com/fsolt/swiid/master/data/swiid_summary.csv") %>%
  mutate(country = as.character(country)) %>%
  left_join(oecd, by="country") %>%
  left_join(lac, by="country") %>%
  filter(!is.na(lac) | !is.na(oecd), year>1989) %>%
  mutate(t = case_when(!is.na(oecd) ~ "OECD",
                       !is.na(lac) ~ "LAC")) %>%
  mutate(t = if_else(country=="Colombia", "Colombia", t) %>% factor()) %>% 
    #if_else(is.na(oecd), "LAC", "OECD") %>%  factor()) %>%
  ggplot(aes(x=reorder(country, abs_red),y=abs_red, color=t, shape=t))+
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed")+
  coord_flip()+
  theme(legend.position = "top", panel.background = element_blank(), legend.title = element_blank())+
  scale_color_brewer(palette="Set1")+
  labs(title="Los que redistribuyen con impuestos y los que no (1990-2017)",
       subtitle='Cuando se mueve -> Gini baja \n {frame_time}',
       x = "",
       y = "Redistribucion absoluta: Gini market income - Gini disposable income",
       caption = "Source: SWIID Solt (2019) \n OSF. https://osf.io/3djtq")+
  transition_time(year)+ #Seleccionamos la variable de animacion
  shadow_wake(0.7, alpha=0.5)+
  ease_aes('linear') #Tipo de animacion
  
anim_save("swiid.gif",animation=last_animation()) 
