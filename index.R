# Paquete para manipulación de datos
library(dplyr)

# Paquete para manejo de datos vectoriales
library(sf)

# Paquete para manejo de datos raster
library(terra)

# Paquete para simplificación y edición de geometrías
library(rmapshaper)

# Paquetes con datos geoespaciales para ejemplos
library(spData)
library(spDataLarge)


# datos

# archivo CSV con registros de denuncias
denu_depu <-
  st_read(
    "/vsicurl/https://raw.githubusercontent.com/MaureenArg/datostarea/master/denucdepu.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLon",
      "Y_POSSIBLE_NAMES=decimalLat"
    ),
    quiet = TRUE
  )

st_crs(denu_depu) = 4326

# capa vectorial (GeoJSON) de provincias de Costa Rica
provincias <-
  st_read(
    "https://github.com/tpb728O-programaciongeoespacialr/2021ii/raw/main/datos/ign/delimitacion-territorial-administrativa/provincias-simplificadas_100m.geojson",
    quiet = TRUE
  )


# asp
asp <-
  st_read(
    "https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/main/datos/sinac/areas-silvestres-protegidas-simplificadas_100m.geojson",
    quiet = TRUE
  )

#red vial 

# Carga de datos
red_vial <-
  st_read(
    "https://raw.githubusercontent.com/tpb728O-programaciongeoespacialr/2021ii/main/datos/ign/infraestructura/redvial-simplificadas_500m.geojson",
    quiet = TRUE
  )

rios <-
  st_read(
    "https://raw.githubusercontent.com/MaureenArg/datostarea/master/rios.geojson",
    quiet = TRUE
  )
rios1 <-
  st_read(
    "https://raw.githubusercontent.com/MaureenArg/datostarea/master/rios.geojson",
    quiet = TRUE
  )

# mapeo
plot (
  denu_depu$geometry, 
  main= "denucias", 
  axes= TRUE, 
  graticule= TRUE, 
  rest= FALSE
)

#centroides

# provincias y sus centroides calculados con st_centroid() y st_point_on_surface()
plot(
  provincias$geometry,
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  main = "Centroides de provincias: st_centroid (rojo) y st_point_on_surface (verde)",
  axes = TRUE,
  graticule = TRUE)

plot(st_centroid(provincias),
     add = TRUE,
     pch = 16,
     col = "red")

plot(
  st_point_on_surface(provincias),
  add = TRUE,
  pch = 16,
  col = "green")


#denuncias cerca de cuerpo de agua de tipo quebrada 
plot(
  provincias$geometry,
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  main = "Centroides de la ruta 32: st_centroid (rojo) y st_point_on_surface (verde)",
  axes = TRUE,
  graticule = TRUE)


quebradas <-
  rios %>%
  filter(TIPO == "QUEBRADA") %>%
  st_transform(crs = 5367)

plot(
  quebradas$geometry,
  add = TRUE,
  col = "blue")

buffer_quebradas <-
  quebradas %>%
  st_buffer (dist = 1000)

denu_depu_crt <-
  denu_depu %>%
  st_transform(crs = 5367)


denuncias_buffer_quebradas <-
  st_join (denu_depu_crt, buffer_quebradas)

plot (
  st_union( buffer_quebradas),
  main = "Denuncias ambientales alrededor de quebradas", 
  axes = TRUE, 
  graticule = TRUE
)  

plot(quebradas$geometry,
     col = "blue",
     add = TRUE)

plot(
  denuncias_buffer_quebradas,
  pch = 16,
  col = "red",
  add = TRUE
)

#denuncias cerca de autopistas un buffer de 5km 

autopistas <-
  red_vial %>%
  filter(categoria == "AUTOPISTA")


buffer_autopistas <-
  autopistas %>%
  st_buffer (dist = 5000)


denuncias_buffer_autopistas <-
  st_join (denu_depu_crt, buffer_autopistas)

plot (
  st_union( buffer_autopistas),
  main = "Denuncias ambientales alrededor de autopistas", 
  axes = TRUE, 
  graticule = TRUE
)  

plot (autopistas$geometry, col = "brown", add= TRUE)

plot (denuncias_buffer_autopistas, pch =16, col = "red", add = TRUE)


# tipos de denuncias con más registros cerca de autopistas y quebradas

denuncias_buffer_autopistas %>%
  st_drop_geometry () %>%
  filter(!is.na(categoria) & categoria != "") %>%
  group_by(TIPO_inf) %>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)

denuncias_buffer_quebradas %>%
  st_drop_geometry () %>%
  filter(!is.na(TIPO) & TIPO != "") %>%
  group_by(TIPO_inf) %>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)

# humedal y denuncias 

humedal <-
  asp %>%
  filter(cat_manejo == "Humedal")

# Mapa
plot(
  provincias$geometry,
  main = "Denuncias ambientales y ASP con categoría de manejo tipo humedal",
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  axes = TRUE,
  graticule = TRUE)

plot(
  humedal$geometry,
  border = "green",
  add = TRUE)

plot (
  denu_depu_crt$geometry, 
  col = "red",
  add= TRUE
  
)

# denuncias en el pacifico 

provincias_pacifico <-
  provincias %>%
  filter(provincia == "Guanacaste" | provincia == "Puntarenas")

denu_depu_crt_pacif <-
  denu_depu_crt %>%
  filter(Provincia == "Guanacaste" | Provincia == "Puntarenas")


plot(
  provincias_pacifico$geometry,
  main = "Denuncias ambientales ocurridas en el Pacífico",
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  axes = TRUE,
  graticule = TRUE)

plot (
  denu_depu_crt_pacif$geometry, 
  col = "red",
  add= TRUE
  
)

# 10 tipos de denuncias más ocurridas en el pacifico 
denu_depu_crt_pacif%>%
  st_drop_geometry()%>%
  group_by(TIPO_inf)%>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)



# denuncias en el caribe

provincias_caribe <-
  provincias %>%
  filter(provincia == "Limón")

denu_depu_crt_caribe <-
  denu_depu_crt %>%
  filter(Provincia == "Limón")


plot(
  provincias_caribe$geometry,
  main = "Denuncias ambientales ocurridas en el Caribe",
  extent = st_bbox(c(xmin = 500000, xmax = 660000, ymin = 990000, ymax= 1250000)),  
  axes = TRUE,
  graticule = TRUE)

plot (
  denu_depu_crt_caribe$geometry, 
  col = "red",
  add= TRUE
  
)

# 10 tipos de denuncias más ocurridas en el pacifico 
denu_depu_crt_caribe%>%
  st_drop_geometry()%>%
  group_by(TIPO_inf)%>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)
