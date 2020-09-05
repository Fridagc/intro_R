
### Sesion_1 ###

rm(list = ls())

# install.packages("tidyverse") # la primera vez
library(tidyverse) # de aqui en más
# install.packages("readr", dependencies = T) # para csv
# install.packages("readxl", dependencies = T) # para excel
library(readr)
library(readxl)
# install.packages("janitor") # para nombre


##### R como calculadora ####
2 * 2
2 < 5
5 < 2
10/5

"a" < "b"
"b" < "a"

a = c("a", "e", "i", "o", "u")

b = c(1,2,3,4,5)

str(a) # str --- estructura
str(b)

min(a) # min elemento
max(a)

max(b)
min(b)

mean(b)
sum(b)
sd(b)
summary(b)

a[3]
a[1]

b[6]

c <- c(b,a) # lista uniendo a y b
str(c)
c

d <- data.frame(letras = a ,
                numero = b) # dataframe de a y b

View(d) # ver objeto
d
str(d)

#### acceder a elemento en data.frame
d$letras

d$letras <- as.character(d$letras)

d$mes <- c("ene", "feb", "mar", "abr", "may")

d$mes <- NULL # borrar elemento dentor de df
rm(c) # borrar objetos, vectores o df
d$uno <- "hola" # debo poner c cuando es conjunto sino sin c

d$uno <- 1
d$ope <- d$numero / d$uno

e <- data.frame(letras = c("a", "e", "i", "o", "u") ,
                numero = b) # dataframe de a y b
rm(e)

a[3]
d[3,1] # acceder segun fila y columna por posición
d[3,"letras"] # acceder por posición y por nombre
d[3, ]
d[ ,"mes"]
d[ ,c("mes", "letras")]

d[3,3] <- 5
d[1:2, 3] <- 6

d[c(1:2,5), 3] <- 8

d[c(1:2,5), "uno"] <- 9

e <- data.frame()
e <- d$letras

e <- data.frame(e)
rm(e)

##### Tidyverse ####
e <- d %>%
        mutate(nombres = c("Ana", # crear columna nombres
                           "Bertha",
                           "Claudia",
                           "Diana",
                           "Elsa"),
               otra = 2, # columna otra
               tres = 3) %>% # columna 3
        dplyr::rename(abc = letras) %>%  # renombrar (nombre nuevo = nombre viejo)
        distinct() %>% # sirve para quitar duplicados
        filter(uno != 1)

as.character() ### caracter
as.numeric() ### numeric
as.factor() ### factor

#### Lo mismo pero en R base ####
d$nombres = c("fulanita", "perenganita", "sutanita", "merenganta", "fulanito")

d$nombres_2 <- c("Ana",
                 "Bertha",
                 "Claudia",
                 "Diana",
                 "Elsa")

d$letras <- a
d$abc <- d$letras
d$letras <- NULL # borrar columna

data <- readr::read_csv("inp/sesion_1/200414COVID19MEXICO.csv",
                locale = locale(encoding = "UTF-8"))

data <- readxl::read_xlsx("inp/sesion_1/200414COVID19MEXICO.xlsx", 
                          sheet = 1)

data <- janitor::clean_names(data)

str(data)

summary(data$edad)
colnames(data)

tempo <- data %>%
        dplyr::select(SEXO, ENTIDAD_NAC, EDAD) %>%
        # filter(SEXO == 2) %>%
        # mutate(GENERO = "Mujer") %>% 
        group_by(SEXO) %>% 
        summarise(
                n = n()
                ) %>% 
        ungroup() %>% 
        mutate(sum = sum(n),
               porc = (n / sum)*100, #### hasta acá 08/08/2020
               genero = ifelse(SEXO == 2, "Mujer", "Hombre")) %>% 
        rename(porcentaje = porc)

rm(tempo)

tempo <- data %>% 
        select(ENTIDAD_RES, SEXO) %>% 
        group_by(ENTIDAD_RES, SEXO) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        mutate(SEXO = ifelse(SEXO != 2, "Hombre", "Mujer")) %>% 
        filter(ENTIDAD_RES <= 3) %>% 
        mutate(ENTIDAD_RES = 
                       ifelse(ENTIDAD_RES == 1, "Aguascalientes",
                              ifelse(ENTIDAD_RES == 2, "Baja California", 
                                     "Baja California Sur")))

colnames(tempo)


ggplot(tempo, mapping = aes(x = ENTIDAD_RES, y = n, fill = SEXO)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Hola") +
        xlab("Estado") +
        ylab("Número")

ruta <- "graficos/sesion_1/prueba.jpg"
ggsave(ruta, width = 8, height = 3)

write.csv(tempo, "out/sesion_1/clase15082020.csv", row.names = F)
