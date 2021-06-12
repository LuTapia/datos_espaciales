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

# Obtenga los puntos de ingresos más altos y más bajos
x_max = DF [ DF $ Median_inome == max ( DF $ Median_inome ), longitud ]
y_max = DF [ DF $ Median_inome == max ( DF $ Median_inome ), latitud ]

x_min = DF [ DF $ Median_inome == min ( DF $ Median_inome ), longitud ]
y_min = DF [ DF $ Ingresos_medios == min ( DF $ Ingresos_medios ), latitud ]


# Leer archivo de forma
crswgs84 = CRS ( " + proj = longlat + ellps = WGS84 + datum = WGS84 + no_defs " )
state_shape  <- readShapePoly ( " / home / rstudio / states_21basic / states " , proj4string = crswgs84 , verbose = TRUE )
plot ( states_shape , border = " wheat3 " , col = " wheat1 " )

# tomar el subconjunto de la región este-norte-central
East_North_Central <- subconjunto ( states_shape , STATE_NAME == " Illinois "  |  STATE_NAME == " Indiana " | STATE_NAME == ' Michigan ' | STATE_NAME == ' Ohio ' | STATE_NAME == ' Wisconsin ' )
parcela ( East_North_Central , border = " wheat3 " , col = " wheat1 " )
texto ( East_North_Central , East_North_Central $ STATE_ABBR , cex = 0.8 )


# trazar puntos de ingresos altos y bajos
plot ( states_shape , border = " wheat3 " , col = " wheat1 " )
texto ( forma_estados , forma_estados $ STATE_ABBR , cex = 0.4 )
puntos ( x_max , y_max , col = ' red ' , pch = 16 )
puntos ( x_min , y_min , col = ' green ' , pch = 16 )

# Convertir latitud y longitud en objetos SpatialPoints
co_max  = cbind ( x_max , y_max )
co_min = cbind ( x_min , y_min )
pt_max_income  = SpatialPoints ( co_max , proj4string  =  crswgs84 )
pt_min_income  = SpatialPoints ( co_min , proj4string  =  crswgs84 )


res_high  <- colSums (gContains ( state_shape , pt_max_income , byid  =  TRUE ))
Ingreso_alto_medio = setNames ( res_alto , forma_estados @ datos $ NOMBRE_ESTADO )
HIgh_median_Income [ HIgh_median_Income > 0 ]

res_low  <- colSums (gContains ( state_shape , pt_min_income , byid  =  TRUE ))
Ingresos_medios_bajos = setNames ( res_low , states_shape @ data $ STATE_NAME )
Ingresos_medios_bajos [ Ingresos_medios_bajos > 0 ]


gDistance ( ingresos_máx_ptos , ingresos_mín_ptos )

# dividir los datos en grupos altos y bajos
Ingresos_altos = DF [( DF $ Ingresos_medios > resumen ( DF $ Ingresos_medios ) [ 5 ])]
ingreso_bajo = DF [( DF $ Ingresos_medios < resumen ( DF $ Ingresos_medios ) [ 2 ])]

# convertir el marco de datos en SpatialPointsDataFrame
xy_alto  <-  Ingresos_altos [, c ( 6 , 7 )]
spdf_high  <- SpatialPointsDataFrame ( coords  =  xy_high , data  =  High_income , proj4string  =  crswgs84 )
xy_low  <-  ingresos_bajos [, c ( 6 , 7 )]
spdf_low  <- SpatialPointsDataFrame ( coords  =  xy_low , data  =  low_income , proj4string  =  crswgs84 )

high_poly = gConvexHull ( spdf_high )
low_poly = gConvexHull ( spdf_low )

gIsValid ( high_poly , motivo = VERDADERO )
gIsValid ( low_poly , motivo = VERDADERO )

trama ( high_poly , border = ' rojo ' )
plot (gIntersection ( low_poly , high_poly ), add = TRUE , col = ' beige ' )
plot (gCentroid ( high_poly ), col = ' red ' , add = TRUE )
plot (gCentroid ( low_poly ), col = ' blue ' , add = TRUE )


gArea ( high_poly )
gLength ( high_poly )
gBoundary ( high_poly )

# Leer archivo de forma de condados y centroides
condados  <- readShapePoly ( " condados " )
centroides  <- readShapePoints ( " centroides " )
parcela ( condados , frontera = " trigo3 " , col = " trigo1 " )
puntos ( centroides , col = ' rojo ' , pch = 16 , cex = 0.3 )
