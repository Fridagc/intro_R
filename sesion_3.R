rm(list = ls())

##### Clase 3 #####
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(viridis)

#### GGPLOT ####

# Cualquier gráfica hecha con ggplot2 puede incluir hasta siete parámetros

# ggplot(data = <DATOS>) +      # 1) Nombre del data frame del cual obtener los datos
#   <GEOM_FUNCIÓN>(             # 2) Objeto geométrico que representará variable(s)
#     mapping = aes(<MAPEO>),   # 3) Instrucciones para asignar variables a canales
#     stat = <STAT>,            # 4) Transformación estadística a realizar en variables
#     position = <POSICIÓN>) +  # 5) Ajuste de posición de los elementos en el geom
#   <COORDENADAS> +     # 6) Tipo de sistema de coordenadas a utilizar
#   <FACET>             # 7) Función para generar facetas

data <- read_csv("inp/sesion_3/interrupcion-legal-del-embarazo.csv",
                 locale = locale(encoding = "UTF-8")) # o Latin1 o UTF-8

data <- janitor::clean_names(data)
data <- data %>% 
        mutate(edocivil_descripcion = 
                       ifelse(edocivil_descripcion == "NA", 
                              NA, 
                              edocivil_descripcion))

# geom_histogram()
data %>%
        ggplot() + 
        geom_histogram(mapping = aes(x = nhijos))

# geom_density()
data %>%
        ggplot() +
        geom_density(mapping = aes(x = edad))

# geom_point()

# Versión 1
data %>%
        ggplot() +
        geom_point(aes(x = edocivil_descripcion, 
                       y = motiles))

unique(data$entidad)

# Versión 2
data %>%
        filter(ano < 2019, entidad == "Ciudad de México") %>% # Filtrar datos
        ggplot() +
        geom_point(mapping = aes(x = edad, y = nile),
                   alpha = 0.3) # Cambio en la opacidad de los puntos

colnames(data)


# Versión 3
data %>% 
        filter(!is.na(parentesco) & !is.na(edocivil_descripcion) &
                       entidad == "Baja California") %>% 
        ggplot() +
        geom_point(
                   mapping = aes(x = edad, y = parentesco, 
                                 color = as.factor(edocivil_descripcion) # Color por nivel educativo
                   ), alpha = 0.8, size = 4)


# Versión 4
data %>% 
        group_by(entidad) %>%
        summarise(edad_mean = mean(edad)) %>% 
        filter(entidad %in% c("Aguascalientes", "Baja California")) %>% 
        ggplot() +
        geom_segment(aes(x=entidad, xend=entidad, 
                         y=0, yend=edad_mean), color="grey") +
        geom_point(aes(x=entidad, y = edad_mean), color="orange", size=4) +
        theme_minimal() +
        # theme_bw() +
        # theme_stata() +
        # theme_economist() +
        # theme_excel() +
        theme(
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                           color = "red", 
                                           size = c("Aguascalientes" = 20, 
                                                    "Baja California" = 10)),
                axis.title.y = element_text(size = 14, color = "blue"),
                plot.title = element_text(size = 20, color = "pink")
        ) +
        # ylab("Nombre Y") + # Forma 1
        # xlab("Nombre de entidad") +# Forma 1
        
        labs(x = "Entidad Hola", # Forma 2
             y = "Nombre Y Hola",
             title = "Gráfico 1",
             caption = "Gráfico hecho\npor yo mera")

###### break

#### reordenar geom_bar
tempo <- data %>% 
        filter(ano == 2019) %>% 
        group_by(entidad) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        arrange(- n) %>% # - mayor a menor (en reordenamiento) + es inverso
        slice(1:10)

ggplot(tempo, mapping = aes(x = reorder(entidad, n), y = n)) +
        geom_bar(aes(fill = entidad), stat = "identity") +
        geom_text(aes(label = n), vjust = -.5) +
        ylim(0, 6000) +
        theme(
                axis.text.x = element_text(hjust = 1,
                                           size = 10,
                                           angle = 45),
                axis.title.y = element_text(size = 14),
                plot.title = element_text(size = 20,
                                          hjust = .75),
                legend.position = "bottom"
        ) +
        labs(x = "Entidad",
             y = "",
             title = "Ejemplo reorder") +
        guides(fill = F) +
        coord_flip()


#### facets ####

tempo <- data %>% 
        rename()
        filter(!is.na(entidad)) %>% 
        group_by(entidad, aa_u_0091_o) %>% 
        summarise(n = n(), .groups = "drop") %>% 
        ungroup() 

tempo <- tempo %>% 
        arrange(entidad, - n) # ordena entidad segun may a men de N

expansor <- expand.grid(entidad = unique(tempo$entidad),
                        ano = unique(tempo$aa_u_0091_o))

expansor <- left_join(expansor, tempo, by = c("entidad", "aa_u_0091_o"))
sum(is.na(expansor$n))

expansor[is.na(expansor)] <- 0

ggplot() +
        geom_line(expansor, mapping = aes(ano, n, group = entidad)) +
        facet_wrap(entidad ~ ., scales = "free_y")
        theme(strip.text.x = element_text(size = 5))

#### geom_tile
tempo <- data %>% 
        group_by(ano, mes) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        group_by(ano) %>% 
        mutate(total = sum(n),
               porc = round((n / total)*100,1))

expansor <- expand.grid(mes = unique(tempo$mes),
                        ano = unique(tempo$ano))

expansor <- left_join(expansor, tempo)

expansor[is.na(expansor)] <- 0

ggplot(expansor, aes(ano, reorder(mes, ano), fill= n)) + 
        geom_tile() +
        geom_text(aes(label = paste(porc, "%", sep = ""))) +
        scale_fill_viridis(discrete=FALSE)

#### Escalas ####

# Las escalas nos permiten ajustar diversos aspectos relacionados con los elementos estéticos 
# (aes()) de una gráfica.
# 
# Todas las escalas se construyen igual:
#   
#   scale_[aes]_[nombre escala]()

# El [aes] en scale_[aes]_[nombre escala]() puede referise a:
#   
# - Canales de posición: x | y
# - Canales de color: colour | fill | alpha
# - Canales de tamaño: size | radius
# - Canales variaditos: shape| linetype

## Cambiar etiquetas en los ejes
#
#  scale_x/y_continuous() | scale_x/y_discrete()

# scale_x/y_continuous() y scale_x/y_discrete() nos permiten modificar diferentes elementos de los ejes:
#   
# - breaks = posición donde aparecen las etiquetas del eje
# - labels = etiquetas que apareceran en los breaks
# - limits = límites del eje
# - position = posición del eje: "top", "right", "bottom" o "left" 
colnames(data)
table(data$religion)
tempo <- data %>% 
        filter(!is.na(religion) &
                       religion != "católica" &
                       religion != "ninguna" &
                       religion != "creyente" &
                       religion != "otra") %>%
        group_by(religion) %>% 
        summarise(n = n()) %>% 
        arrange(- n) %>%
        slice(1:5) %>% 
        ungroup()

ggplot() +
        geom_point(tempo, mapping = aes(x = religion, 
                                        y = n, 
                                        color = religion, 
                                        size = n)) +
        scale_color_manual(values = c(
                "#8dd3c7",
                "#ffffb3",
                "#bebada",
                "#fb8072",
                "#80b1d3"
        ),
        labels = c("Religión 1", "Religión 2", 
                   "Religión 3", "Religión 4", 
                   "Religión 5")
        ) +
        scale_x_discrete(
                labels = c("Religión 1", "Religión 2", 
                           "Religión 3", "Religión 4", 
                           "Religión 5")) +
        theme_bw() +
        # guides(color = F, size = F) + # seleccionar cual se queda
        theme(legend.position = "none") # se van todas

rm(expansor)
# scale continuo

tempo <- data %>%
        mutate(fecha = as.Date(fingreso, "%d/%m/%y")) %>% 
        group_by(fecha) %>% 
        summarise(n = n()) %>% 
        ungroup()

ggplot() +
        geom_line(tempo, mapping = aes(x = fecha, y = n), 
                  color = "red") +
        #geom_vline(xintercept = as.Date("2016-01-01"), colour = "red") +
        #geom_label(tempo, mapping = aes(as.Date("2016-01-01"), 90, label = "2016"))
        ylim(0,100) +
        scale_x_date(
                date_breaks = "1 year",
                date_labels = "%Y",
                limits = as.Date(c('2016-01-01','2019-06-15')))

