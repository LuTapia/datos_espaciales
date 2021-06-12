#install.packages("ggmap")
#install.packages("zipcodeR")
#install.packages("rgeos")
#install.packages("sp")
#install.packages("maptools")

#cargo librerias
library(readxl)
library(ggplot2) #se requiere ggplot 2 para el uso de ggmap
library(ggmap)
library(zipcodeR)
library(rgeos)
library(sp)
library(maptools)

##Descarga de datos
#https://www.psc.isr.umich.edu/dis/census/Features/tract2zip/
##Cargo los datos
DF <- read_xlsx("IngFamMedios.xlsx", sheet = "Median")

##Limpieza de datos
#Cambio nombre variables
names(DF) <- c('Zip','Median_income','Population')
#Cambio formato zip
str(DF)
DF$Zip<-as.factor(DF$Zip)
str(DF)
#zipcodeR <- download_zip_data(force = FALSE)
#Obtengo coordenadas 
coord <- geocode_zip(DF$Zip)
#Uno los df
DF = merge (DF, coord, by.x = 'Zip', by.y = 'zipcode')

#Consulta a google maps
mapa<- get_map(location = 'united_states',zoom=4,maptype = 'terrain',source = 'google',color = 'color')#source 'google'
#Trazado del mapa base
ggmap(mapa) + 
  #Trazado de puntos de long y lat
  geom_point(aes(x=lng,y=lat
                 ,color = Median_income) #color en base al ingreso familiar medio
             ,data=DF
             ,na.rm = T
             ,size = .5
  ) +
  #Degradado de color en base al ingreso familiar medio
  scale_color_gradient(low="coral", high="blue")