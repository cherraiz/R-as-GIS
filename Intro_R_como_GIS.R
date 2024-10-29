#################### INTRODUCCIÓN A RSTUDIO COMO SIG ####################
##### César Herraiz -- Marzo 2024

# Directorio de trabajo
setwd("C:/Users/Cesar.Herraiz/Documents/Colaboracion docente/MUIBARC/GIS/Datos")

# Instalar paquetes
install.packages("sf")
install.packages("sp")
install.packages("rgdal")
install.packages("raster")
install.packages("mapview")
install.packages("terra")

# Cargar paquetes
{require(sf)
require(sp)
require(rgdal)
require(raster)
require(mapview)
require(terra)}

# 1. Cargar archivos espaciales en R: tipos de formato ----
# 1.1. Cargar archivos shapefile ----
# Paquete sf
comarcas_sf <- st_read("Comarcas.shp")
str(comarcas_sf) # sf almacena la geometria del shp en el campo "geometry"
head(comarcas_sf)  # Cabecera de la tabla
class(comarcas_sf)
plot(comarcas_sf)  # Representa todas las variables
plot(comarcas_sf[1])  # Representar una variable
plot(comarcas_sf[0])  # Representar geometría
plot(comarcas_sf[,c("COD_COMAR")]) # Representar una variable por nombre

ExplotacionesTB_sf <- st_read("Explotaciones_TB.shp")
head(ExplotacionesTB_sf)
plot(ExplotacionesTB_sf[,c("Pos_")])

# Paquete terra
comarcas_terra <- vect("Comarcas.shp")
str(comarcas_terra)
head(comarcas_terra)
class(comarcas_terra)
plot(comarcas_terra)
plot(comarcas_terra[1])
plot(comarcas_terra[,c("COD_COMAR")])


# 1.2. Cargar archivos ráster ----
alt_raster <- raster("CLM_ALT.tif"); plot(alt_raster)     # Paquete raster
class(alt_raster)
str(alt_raster)
plot(alt_raster$BinValues)

alt_terra <- rast("CLM_ALT.tif"); plot(alt_terra)         # Paquete terra
alt_terra <- catalyze(alt_terra); plot(alt_terra)         # Convertir a numérico
class(alt_terra)
str(alt_terra)

usos_raster <- raster("CLM_usos.tif"); plot(usos_raster)  # Paquete raster
usos_terra <- rast("CLM_usos.tif"); plot(usos_terra)    # Paquete terra

# A partir de aquí, vamos a usar el paquete sf para polígonos 
# y terra para raster


# 2. Crear un shapefile a partir de un csv ----
# Cargamos el csv
cabra_tbl <- read.csv("caprapyrenaica.csv", sep = "\t", header = TRUE, 
                      encoding = "UTF-8") # Separación: tabulador
str(cabra_tbl)
cabra_tbl <- cabra_tbl[!is.na(cabra_tbl$decimalLatitude) |    # Quitar NA
                         !is.na(cabra_tbl$decimalLongitude),] # "|" significa "or"
str(cabra_tbl)
plot(cabra_tbl[,c("occurrenceID")])

# Lo converstimos en un objecto del paquete sf
cabra_sf <- st_as_sf(cabra_tbl, coords = c("decimalLongitude","decimalLatitude"))
str(cabra_sf)
plot(cabra_sf[,c("occurrenceID")])
# OJO! Se quedan sin sistema de coordenadas (punto 5)


# 3. Acceso a la base de datos ----
# Paquete sf
View(cabra_sf) # Ver la tabla
cabra_sf$especie <- "Cabra montes" # Nuevo campo "especie"
cabra_sf <- cabra_sf[cabra_sf$countryCode == "ES",]  # Solo registros de España
str(cabra_sf)


# 4. Representacion con Mapview ----
# Plot base de R (combinable entre paquetes)
plot(comarcas_sf[1]); plot(ExplotacionesTB_sf[,c("N")], add = TRUE)
plot(comarcas_terra); plot(ExplotacionesTB_sf[,c("N")], add = TRUE)
plot(usos_raster); plot(ExplotacionesTB_sf[,c("N")], add = TRUE)
plot(alt_terra$BinValues); plot(ExplotacionesTB_sf[,c("N")], add = TRUE)

# Mapview: objetos sf
mapview(comarcas_sf)  # Espacialmente
mapview(comarcas_sf[5])  # Representar un campo
mapview(comarcas_sf[5]) + 
  mapview(ExplotacionesTB_sf[,c("Pos_")]) # Varias capas

# Mapview: objetos raster
mapview(usos_raster) +
  mapview(ExplotacionesTB_sf[,c("Pos_")])

mapview(usos_terra) +
  mapview(ExplotacionesTB_sf[,c("Pos_")]) # Mapview no soporta terra (POR AHORA)


# 5. Sistemas de coordenadas ----
plot(cabra_sf[,c("occurrenceID")])
mapview(cabra_sf)  # No sabe donde situarla porque no tiene SRC
mapview(cabra_tbl) # La tabla no es un objecto espacial, no lo representa


## 5.1. Consultar SRC ----
# Paquete sf
st_crs(cabra_sf)
st_crs(comarcas_sf)

# Paquete terra
terra::crs(alt_terra)

## 5.2. Definir SRC ----
# Paquete sf
# Los datos de cabra están en geográficas WGS84
st_crs(cabra_sf)
cabra_sf <- st_set_crs(cabra_sf, 
                       "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs 
                       +towgs84=0,0,0")
st_crs(cabra_sf)
cabra_sf <- st_set_crs(cabra_sf, 4326)  # Preferible código EPSG
st_crs(cabra_sf)
mapview(cabra_sf)

# Paquete terra
terra::crs(alt_terra) <- "epsg:32630"  # UTM WGS 84


# 5.3. Transformar SRC ----
# Paquete sf
st_crs(cabra_sf)
cabra_sf_ETRS <- st_transform(cabra_sf, crs = 25830) # UTM ETRS 89
st_crs(cabra_sf_ETRS)
comarcas_sf_ETRS <- st_transform(comarcas_sf, crs = 25830) # UTM ETRS 89

# Paquete terra
terra::crs(alt_terra)
alt_ETRS <- project(alt_terra, "EPSG:25830")  # tarda un poco
terra::crs(alt_ETRS)

# Comprobacion
plot(alt_terra$BinValues); plot(cabra_sf[0], add = TRUE)
# No es capaz de representarlos juntos

plot(alt_ETRS$BinValues); plot(cabra_sf_ETRS[0], add = TRUE)
# Representacion conjunta


# 6. Cambiar de formato ----
# Algunas funciones solo están disponibles en determinados paquetes, para lo
# que puede ser útil cambiar el formato del archivo a ese paquete para 
# poder utilizar esa función y luego volver a cambiarlo

# 6.1. Shapefiles ----
# sf a sp
class(cabra_sf); mapview(cabra_sf)
cabra_spatialpolygon <- as(cabra_sf, "Spatial")
class(cabra_spatialpolygon); mapview(cabra_spatialpolygon)

# sp a sf
cabra_simplefeature <- st_as_sf(cabra_spatialpolygon)
class(cabra_simplefeature); mapview(cabra_simplefeature)


# 6.2. Raster ----
# paquete terra a paquete raster 
class(alt_terra); plot(alt_terra$BinValues)
alt_rasterlayer <- raster(alt_terra)
class(alt_rasterlayer); mapview(alt_rasterlayer)

# paquete raster a paquete terra
alt_spatraster <- rast(alt_rasterlayer)
# alt_spatraster <- as(alt_rasterlayer, "SpatRaster") # lo mismo que la anterior
class(alt_spatraster); plot(alt_spatraster)


# 6.3. Rasterizar
# Necesistamos que el vector esté en formato vectorial de terra
comarcas_vect <- vect(comarcas_sf_ETRS)

# Luego necesitamos un ráster vacío. Tenemos que darle una extensión (límintes
# de coordenadas x e y) y una resolución (tamaño de píxel). Aquí lo que hago
# es extraer esos valores del ráster (usos_terra), de forma que los píxeles
# coincidan. También le doy un sistema de coordenadas.
r <- rast(comarcas_vect, crs = "EPSG:25830",
          extent = ext(usos_terra), resolution = res(usos_terra))
plot(r)
plot(comarcas_vect, add = TRUE)

# Para rasterizarlo le damos la capa vectorial, el ráster vacío para rellenarlo
# con valores, el campo con el que queremos rellenarlo, y la función a usar
# cuando en un píxel hay varios valores de polígono (en este caso media)
comarcas_rast <- rasterize(comarcas_vect, r, field = "TMED_media",fun = mean,
                           na.rm = TRUE) 
plot(comarcas_rast)



# 7. Preparar los puntos: geometrías, disolver, unir, recortar y buffer ----
# Calcular geometrías
comarcas_sf$area <- st_area(comarcas_sf)  # Nuevo campo "área"
head(comarcas_sf[c("NOM_COMAR", "area")])
# st_length()  # sirve para la longitud de lineas

# Compatibilidad con tidyverse: Disolver
require(tidyverse)
municipios <- st_read("TM.shp", crs = 25830)
unique(municipios$PROVMUN)   # Ver registros únicos (sin repeticiones)
municipios <- municipios %>% 
  separate(PROVMUN, into = c("PROV", "MUN"), sep = 2) # Separamos PROV y MUN
unique(municipios$PROV)
plot(municipios[1])
provincias <- municipios %>%       # Partimos de la capa municipios
  group_by(PROV) %>%               # Agrupamos en base a provincia
  select (PROV) %>%                # Nos quedamos solo con el campo provincia
  summarise_each(funs = length)    # Disolvemos en base al campo que hemos 
                                   # utilizado para agrupar. Se podrían usar
                                   # otras funciones (medias, sumas, etc)
plot(provincias)

# Unir
names(cabra_sf_ETRS)
cabra_sf_ETRS <- st_join(cabra_sf_ETRS, provincias, 
                         join = st_within)   # Unimos los que estén dentro
names(cabra_sf_ETRS)

# Recortar capa de cabras con poligono de comarcas
cabra_sf_ETRS <- st_join(cabra_sf_ETRS, comarcas_sf, 
                         join = st_within) # SRC debe coincidir !!
comarcas_sf_ETRS <- st_transform(comarcas_sf, crs = 25830)
cabra_sf_ETRS <- st_join(cabra_sf_ETRS, comarcas_sf_ETRS, 
                         join = st_within)  # Unir shapefiles
plot(cabra_sf_ETRS[0])
cabra_CLM <- cabra_sf_ETRS[!is.na(cabra_sf_ETRS$NOM_COMAR),] # Cabras en CLM
comarcas_geom <- st_geometry(comarcas_sf_ETRS)   # Limites de poligonos
plot(comarcas_geom); plot(cabra_CLM[0], add = TRUE, col = "red")

# Buffer
cabra_buffer <- st_buffer(cabra_CLM, 5000, endCapStyle = "SQUARE") # cuadrado
plot(cabra_buffer[0], add = TRUE)

expl_buffer <- st_buffer(ExplotacionesTB_sf, 5000, 
                         endCapStyle = "ROUND")  # redondo
plot(comarcas_geom); plot(ExplotacionesTB_sf[0], add = TRUE, col = "red")
plot(expl_buffer[0], add = TRUE)



# 8. Extraer datos de ráster ----
# Cortar un raster: mask
plot(alt_ETRS$BinValues); plot(comarcas_geom, add = TRUE)
alt_CLM <- terra::mask(alt_ETRS, vect(comarcas_sf_ETRS)) # Debe ser objeto terra
plot(alt_CLM$BinValues); plot(comarcas_geom, add = TRUE)

# Extraer informacion: extract
# Con puntos
cabra_CLM$alt <- terra::extract(alt_CLM$BinValues, cabra_CLM)

# Con poligonos (tarda un poco...)
cabra_buffer$alt <- terra::extract(alt_CLM$BinValues, 
                                   cabra_buffer,
                                   fun = mean, na.rm = T) # mean, min, max...


# 9. Guardar capas ----
# Paquete sf
st_write(cabra_CLM, "cabra_CLM", driver = "ESRI Shapefile")

# Paquete terra
terra::writeRaster(alt_CLM$BinValues, "ALT_CLM_mask.tif", filetype = "GTiff")
