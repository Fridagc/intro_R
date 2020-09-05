#### CLASE 2 ####
rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(readr)
library(janitor)
library(tidyquant) # rezagos
library(lubridate) # manipular fechas

carpetas <- read_csv("inp/sesion_2/carpetas.csv")
nueve_once <- read_csv("inp/sesion_2/nueve_once.csv", 
                       locale = locale(encoding = "UTF-8"))
salud <- readr::read_csv("inp/sesion_2/salud.csv", 
                         locale = locale(encoding = "UTF-8"))

#### conociendo nuestras tablas ####
head(salud)    # Imprime los primeros renglones del objeto
tail(salud)    # Imprime los últimos renglones del objeto
View(salud)    # Abre el objeto entero en una nueva ventana
nrow(salud)     # Número de renglones
ncol(salud)     # Número de columnas
dim(salud)      # Número de renglones y columnas
rownames() # Muestra los nombres de los renglones
colnames() # Muestra los nombres de las columnas
names(salud)    # Muestra los nombres de las columnas
str(salud)      # Muestra la estructura del data frame (dimensiones y tipos de cada variable)

#### cambios de nombre ####
carpetas <- carpetas %>% 
        rename(ao = año)

salud <- janitor::clean_names(salud)

carpetas_tempo <- carpetas %>% 
        mutate(mes = month(as.Date(fecha), label = T, abbr = F)) %>% 
        arrange(ao, mes) %>% 
        mutate(lag = lag(tasa_ver, 12),
                      dif = tasa_ver - lag)

# necesito variable entidad para poder pegar con llamadas nueve once
carpetas_tempo$entidad <- "Veracruz de Ignacio de la Llave"

# altas a bajas
nueve_once <- nueve_once %>% 
        mutate(mes = tolower(mes))

join <- left_join(carpetas_tempo, nueve_once, by = c("entidad", 
                                                     "ao" = "ano",
                                                     "mes"))

nchar(unique(carpetas_tempo$entidad)) # contar los caracteres
nchar(unique(nueve_once$entidad[nueve_once$entidad== "Veracruz de Ignacio de la Llave"]))

unique(carpetas_tempo$entidad) # valores unicos en dataframe
unique(nueve_once$entidad)

sum(is.na(nueve_once$tasa_muj_vio_fam_911))
sum(is.na(join$tasa_muj_vio_fam_911)) # ¿cuantos na hay?

join <- join %>% 
        select(ao, mes, fecha.x, viol_fam_ver,
               tasa_ver, lag, dif, vio_fam_911_mensual,
               tasa_muj_vio_fam_911) %>% 
        filter(!is.na(tasa_muj_vio_fam_911)) %>% 
        arrange(ao, mes) %>% 
        dplyr::group_by(ao) %>% 
        summarise(sum_vio_fam = sum(viol_fam_ver),
                  sum_911 = sum(vio_fam_911_mensual)) %>% 
        dplyr::ungroup()

ggplot(data = join, aes(x = as.character(ao), 
                        y = sum_vio_fam,
                        color = as.character(ao),
                        size = sum_vio_fam)) +
        geom_point()
dev.off()
ggsave("graficos/sesion_2/grafica_fea.jpg", width = 5, height = 3)

###### parte 2 

data <- read_csv("inp/sesion_2/afluencia-diaria-del-metro-cdmx.csv", 
                 locale = locale(encoding = "UTF-8"))

unique(data$Año)

data1 <- data %>%
        spread(key = "Año",
               value = "Afluencia",
               fill = NA)

# write.csv(data, "out/afluencia_metro_cdmx.csv", row.names = F)

data <- data1 %>%
        gather("2018", "2019", "2020",
               key = "ao",
               value = "afluencia")

# write.csv(data, "out/sesion_1/afluencia_metro_cdmx.csv", row.names = F)

# data <- read_csv("out/afluencia_metro_cdmx.csv", locale = locale(encoding = "Latin1"))

str(data)

data$`2019` <- as.numeric(data$`2019`)
data$`2020` <- as.numeric(data$`2020`)

data_1 <- reshape2::melt(data, id = c("Linea", "Fecha", "Estacion"))

data <- data.frame(data)

data <- data %>%
        gather("X2018", "X2019", "X2020",
               key = "ao",
               value = "afluencia")

data <- data %>% 
        mutate(ao = ifelse(ao == "X2018", "2018",
                           ifelse(ao == "X2019", "2019",
                                  ifelse(ao == "X2020", "2020", "FIN"))))

data <- janitor::clean_names(data)

tempo <- data %>% 
        mutate(fecha = as.Date(fecha),
               mes = month(fecha, label = T, abbr = F),
               dia = weekdays(fecha))

sum(is.na(tempo$afluencia))

tempo <- tempo %>% 
        mutate(afluencia = ifelse(is.na(afluencia), 0, afluencia))

sum(is.na(tempo$afluencia)) # perdidos
sum(!is.na(tempo$afluencia)) # no perdidos

table(tempo$linea) 
table(tempo$linea, tempo$mes) # primer arg filas, seg columnas
table(tempo$linea, tempo$dia)

prop.table(table(tempo$linea, tempo$mes))
prop.table(table(tempo$linea, tempo$mes))*100

ejemplo <- tempo %>% 
        group_by(linea, mes) %>% 
        summarise(sum_a = sum(afluencia), .groups = "drop") %>% # suma de afluencia por mes y linea
        ungroup() %>% 
        group_by(mes) %>% 
        mutate(sum = sum(sum_a), # suma de afluc de cada mes
               pro = sum_a / sum) %>% # prop de afluencia de cada mes por linea
        ungroup()

tempo <- tempo %>% 
        mutate(dia = # case_when es un condicional, al igual que ifelse
        case_when(dia == "lunes" ~ "lunes", # sino se cumple esta condicion pasa a sig
                  dia ==  "martes" ~ "martes", 
                  T ~ as.character(dia)) # sino es monday ni tuesday deja día
        )
unique(tempo$dia)

tempo$dia <- factor(x = tempo$dia, 
                    levels = c("lunes",
                               "martes",
                               "miércoles",
                               "jueves",
                               "viernes",
                               "sábado",
                               "domingo"))

tempo <- tempo %>% 
        mutate(dia = ifelse(dia == "Monday", "Lunes",
                            ifelse(dia == "Tuesday", "Martes", dia)))

levels(tempo$dia)
nlevels(tempo$dia)

ggplot(tempo[tempo$ano == 2018, ], aes(x = dia, y = linea)) +
        geom_tile(aes(fill = afluencia))
