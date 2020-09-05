rm(list=ls())

library(tidyverse)
library(rgdal)
library(rgeos)
library(postGIStools)
library(raster)
library(ggplot2)
library(viridis)
library(srvyr)
library(survey)
library(foreign)

cdmx <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/cdmx.csv")

cdmx <- cdmx %>% 
        mutate(ao = lubridate::year(anfitrion_desde))

cdmx_points <- SpatialPointsDataFrame(cdmx[ , c("longitud", "latitud")],
                                       cdmx,
                                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(cdmx_points)

colonias <- rgdal::readOGR("inp/sesion_4/Colonias/colonias.shp", stringsAsFactors = F)
colonias <- sp::spTransform(colonias, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coloniasF <- fortify(colonias, region = "idcolonia")
coloniasF$id <- substr(coloniasF$id,2,3)
coloniasF$id <- as.numeric(coloniasF$id)

alcaldias <- readOGR("inp/sesion_4/alcaldias/alcaldias.shp", stringsAsFactors = F)
alcaldias <- sp::spTransform(alcaldias, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
alcaldiasF <- fortify(alcaldias)
alcaldiasF$id <- as.numeric(alcaldiasF$id) + 2

airbnb_col <- over(cdmx_points, colonias)

cdmx <- bind_cols(cdmx, airbnb_col)

cols_airbnb <- cdmx %>% 
        group_by(idcolonia, nombre) %>% 
        dplyr::summarise(total = n (), .groups = "drop") %>% 
        ungroup()

cols_airbnb$idcolonia <- substr(cols_airbnb$idcolonia,2,3)

cols_airbnb$idcolonia <- as.numeric(cols_airbnb$idcolonia)
coloniasF$id <- as.numeric(coloniasF$id)

coloniasF <- left_join(coloniasF, cols_airbnb, by = c("id" = "idcolonia"))
#coloniasF[is.na(coloniasF)] <- 0

ggplot() +
        # geom_polygon(data = coloniasF, aes(x = long, y = lat, group = group, 
        #                                    fill = total), 
        #              color = "black", size = 0.2) 
        geom_polygon(data = alcaldiasF, aes(x = long, y = lat, group = group, fill = id),
                     color = "black", size = 0.4) +
        labs(title = "Colonias con más Airbnb", fill="Total") +
        theme(plot.title = element_text(hjust = 1, face = "bold", size = 15)) +
        scale_fill_viridis(option = "inferno", na.value = "white", direction = -1) +
        # #scale_fill_gradient2(high = "#49006a", low="white", mid = "white", na.value = "white") +
        guides(fill = F) +
        coord_map() +
        theme_void()
dev.off()


id <- unique(alcaldiasF$id)

dir.create("graficos/sesion_4")
id

for (i in 1:length(id)) {
        
        ggplot() +
                geom_polygon(data = alcaldiasF[alcaldiasF$id == id[i] ,], 
                             aes(x = long, y = lat, 
                                 group = group, fill=id), 
                             color = "black", size = 0.2) +
                labs(title = "Colonias con más Airbnb", fill="Total") +
                theme(plot.title = element_text(hjust = 1, face = "bold", size = 15)) +
                scale_fill_viridis(option = "inferno", na.value = "white", direction = -1) +
                guides(element_text()) +
                coord_map() +
                theme_void()
        
ggsave(paste("graficos/sesion_4/mapa_", id[i], ".jpg", sep=""),  width = 8, height = 4)
        
}

#### SURVEY ####
## ENIGH
rm(list = ls())
ingresos.enigh <- read.csv("inp/enigh/ingresos.csv")
poblacion.enigh <- read.csv("inp/enigh/poblacion.csv")
concen.enigh <- read.csv("inp/enigh/concentradohogar.csv")

# Conteo de viejitos
poblacion.enigh$con <- 1

pob.65.enigh <- poblacion.enigh %>% 
        filter(edad >= 65) %>% 
        group_by(ï..folioviv, foliohog, sexo) %>% 
        summarise(suma = sum(con)) %>% 
        spread(sexo,suma) %>% 
        ungroup()

names(pob.65.enigh) <- c("folioviv","foliohog", "H", "M")

pob.65.enigh$H[is.na(pob.65.enigh$H)] <- 0
pob.65.enigh$M[is.na(pob.65.enigh$M)] <- 0

conc.pob <- left_join(concen.enigh, pob.65.enigh, by=c("ï..folioviv" = "folioviv","foliohog"))

conc.pob$H[is.na(conc.pob$H)] <- 0
conc.pob$M[is.na(conc.pob$M)] <- 0

#Se define el objeto tipo encuesta
pob.survey <- svydesign(id=~upm, strata=~est_dis, data=conc.pob, weights=~factor)
svymean(~jubilacion,pob.survey)

#Dummy si hay viejito o no
conc.pob$viejito <- ifelse(conc.pob$H >=1 | conc.pob$M >=1, 1,0)
summary(conc.pob$transfer[conc.pob$viejito==1])
summary(conc.pob$transfer[conc.pob$viejito==0])

############ MEDIAS ###########
# jubilacion 
svyby(~jubilacion,~viejito, pob.survey, svymean)

# becas
svyby(~becas,~viejito, pob.survey, svymean)

# donativos
svyby(~donativos,~viejito, pob.survey, svymean)

# beneficios gubernamentales
svyby(~bene_gob,~viejito, pob.survey, svymean)

# transferencias hog
svyby(~transf_hog,~viejito, pob.survey, svymean)

# transferencias inst
svyby(~trans_inst,~viejito, pob.survey, svymean)

########## CRUCES ####### 
#svyby(~bene_gob, ~viejito+luchona, pob.survey, svymean)
svyby(~bene_gob, ~viejito, pob.survey, svymean)
#svyby(~bene_gob, ~viejito+jefe.no.viejito, pob.survey, svymean)
#svyby(~bene_gob, ~viejito+total.nini, pob.survey, svymean)
#svyby(~bene_gob, ~viejito+total.nini+luchona, pob.survey, svymean)
#svyby(~bene_gob, ~viejito+joven, pob.survey, svymean)

svyby(~bene_gob, by=~viejito, denominator=~ing_cor, pob.survey, svyratio)




