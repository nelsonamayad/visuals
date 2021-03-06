#####################################################
#### Las trayectorias de dos contratos sociales: ####
#### Colombia y Venezuela entre 1960 y 2017      ####
#####################################################

# Cargar/Instalar paquetes ####
library(WDI)
library(tidyverse)
library(gganimate)
library(gifski)
library(RColorBrewer)
library(ggrepel)

# Animacion gganimante y datos de World Development Indicators ####
# Primer paso: descargar los datos de GDP per capita usando WDI()
WDI(country="all",indicator="NY.GDP.PCAP.KD", start=1960, end=2017) %>%
  #filtramos para los dos paises de interes
  filter(country=="Colombia" | country=="Venezuela, RB") %>%
  #Renombramos la variable clave
  rename("gdppc"="NY.GDP.PCAP.KD") %>%
  #Creamos un codigo que sea pais-ano para merge los datos de la otra variable. No es necesario, pero es util saber como se hace
  mutate(code=str_c(iso2c,year)) %>%
  #Merge con los datos de expectativa de vida
  left_join(WDI(country="all",indicator="SP.DYN.LE00.IN", start=1960, end=2017) %>%
              filter(country=="Colombia" | country=="Venezuela, RB") %>%
              rename("life_exp"="SP.DYN.LE00.IN") %>%
              mutate(code=str_c(iso2c,year)), 
            #Aca usamos el codigo nuevo para merge ambos datos
            by="code") %>%
  #Merge con datos de poblacion
  left_join(WDI(country="all",indicator="SP.POP.TOTL", start=1960, end=2017) %>%
              filter(country=="Colombia" | country=="Venezuela, RB") %>%
              rename("pop"="SP.POP.TOTL") %>%
              mutate(code=str_c(iso2c,year)), 
            #Aca usamos el codigo nuevo para merge ambos datos
            by="code") %>%
  #La grafica
  ggplot(aes(y=life_exp,x=gdppc, color=country.x))+
  #Ahora le decimos a ggplot que tipo de grafico nos deberia mostrar
  geom_point(aes(size=pop), show.legend=F)+
  geom_point()+
  #Le ponemos tags a los puntos con los nombres de los paises
  #geom_text(aes(label=country.x), vjust=-.2, size=5)+
  #Con eso le damos mejores colores a la grafica
  #scale_x_log10()+
  scale_colour_brewer(palette="Set1") +
  #Le damos una estetica simple a la grafica, sin color de fondo
  theme_classic()+
  theme(legend.position = "top", legend.title = element_blank())+
  #Ahora usamos la animacion de gganimate
  labs(title="Colombia y Venezuela entre 1960 y 2017",
       subtitle = '{frame_time} \n Poblacion = Tamano del punto' ,
       x = 'PIB per capita',
       y = 'Expectativa de vida',
       caption = "Fuente: World Development Indicators")+ #Le ponemos la variable de animacion al titulo
  transition_time(year.x)+ #Seleccionamos la variable de animacion
  shadow_wake(0.8, alpha=0.5)+
  ease_aes('linear') #Tipo de animacion

# Asi se guarda como gif
anim_save("col_ven.gif", animation = last_animation())
