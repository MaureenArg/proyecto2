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




plot(
  provincias1$geometry,
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  main = "Quebradas",
  axes = TRUE,
  graticule = TRUE)




Row {data-width=300}
-----------------------------------------------------------------------
  
  ### Cantidad y tipo de denuncias ambientales cerca de autopistas y quebradas
  
  ```{r}



```
# 10 tipos de denuncias más ocurridas en el humedales

h3 ("Las 10 tipos de denuncias ambientales ocurridas con más frecuencia en humedales")

provincias_caribe <-
  provincias1 %>%
  filter(provincia == "Limón")

denu_depu_crt_humedal <-
  denu_depu_crt %>%
  filter(Provincia == "Limón",Provincia == "Puntarenas", Provincia == "Guanacaste" )




humedal_denu%>%
  st_drop_geometry()%>%
  group_by(TIPO_inf)%>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)



renderDT({
  denuncias_buffer_quebradas<-filtrarRegistros()
  
  denuncias_buffer_quebradas %>%
    st_drop_geometry () %>%
    filter(!is.na(TIPO) & TIPO != "") %>%
    group_by(TIPO_inf) %>%
    summarise (registros = n ()) %>%
    arrange (desc(registros)) %>%
    slice (1:10)%>%
    datatable(rownames= FALSE, colnames = c( "Tipo de denuncia", "Cantidad de denuncias", dom = "Bfrtip"))  
  
})







# Denuncias y Áreas Silvestres Protegidas

Column {data-width=300}
-----------------------------------------------------------------------
  
  ### Denuncias ambientales y Áreas Silvestres Protegidas
  
  ```{r}
# humedal y denuncias 

humedal <-
  asp %>%
  filter(cat_manejo == "Humedal")

# Mapa
plot(
  provincias1$geometry,
  main = "Denuncias ambientales y ASP con humedal",
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  axes = TRUE,
  col = "transparent",
  graticule = TRUE)

plot(
  humedal$geometry,
  border = "green",
  add = TRUE)

plot(
  asp$geometry,
  border = "brown",
  add = TRUE) 

plot (
  denu_depu_crt$geometry, 
  col = "red",
  add= TRUE)

```

### Denuncias Ambientales cercanas a ASP con categoría de manejo tipo Humedal

```{r}

provincias1 <-
  provincias %>%
  st_transform(crs = 5367)

prov_humedal <-
  provincias1 %>%
  filter(provincia == "Puntarenas"| provincia == "Limón"| provincia == "Guanacaste")

denu_depu_crt_humedal <-
  denu_depu_crt %>%
  filter(Provincia == "Limón" | Provincia== "Puntarenas"| Provincia == "Guanacaste")



#Mapa

plot (
  prov_humedal$geometry, 
  main= "Denuncias Ambientales cercanas a ASP con humedal", 
  col= "grey",
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),
  axes= TRUE, 
  graticulate = TRUE
)

plot (
  denu_depu_crt_humedal$geometry, 
  col = "red",
  add= TRUE
  
)  

plot (provincias1$geometry, add= TRUE, axes = TRUE)

plot (humedal, add= TRUE, col = "green")

```






### Denuncias registradas a una distancia de 7 km de cuerpos de agua de tipo: Quebradas

```{r}
provincias1 <-
  provincias %>%
  st_transform(crs = 5367)

quebradas <-
  rios %>%
  filter(TIPO == "QUEBRADA") %>%
  st_transform(crs = 5367)


buffer_quebradas <-
  quebradas %>%
  st_buffer (dist = 7000)

denu_depu_crt <-
  denu_depu %>%
  st_transform(crs = 5367)


denuncias_buffer_quebradas <-
  st_join (denu_depu_crt, buffer_quebradas)

plot (
  st_union( buffer_quebradas),
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
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

plot (provincias1$geometry, add= TRUE)


```


### Quebradas de la GAM

```{r}
GAM <-
  provincias1 %>%
  filter(provincia == "San José" | provincia == "Alajuela"| provincia == "Heredia"| provincia == "Cartago") 


Denuncias_GAM<-
  denuncias_buffer_quebradas %>%
  filter (Provincia == "San José" | Provincia == "Alajuela"| Provincia == "Heredia"| Provincia == "Cartago")


plot (
  st_union( Denuncias_GAM),
  extent = st_bbox(c(xmin = 280000, xmax = 660000, ymin = 880000, ymax= 1250000)),  
  main = "Denuncias ambientales alrededor de quebradas de la GAM", 
  axes = TRUE, 
  graticule = TRUE
)  

plot(quebradas$geometry,
     col = "blue",
     add = TRUE)

plot(
  Denuncias_GAM$geometry,
  pch = 16,
  col = "red",
  add = TRUE
)



plot (provincias1$geometry, add = TRUE
      
)





```






### Denuncias ambientales registradas a una distancia de 5 km de autopistas/rutas importantes (1, 2, 27, 32)


```{r}
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
  main = "Denuncias ambientales alrededor de rutas importantes", 
  axes = TRUE, 
  graticule = TRUE
)  

plot (autopistas$geometry, col = "brown", add= TRUE)

plot (denuncias_buffer_autopistas, pch =16, col = "red", add = TRUE)

plot (provincias1$geometry, add= TRUE)

```




Column {data-width=200}
-----------------------------------------------------------------------
  ### Cantidad de denuncias ambientales alrededor de cuerpos de agua de tipo: Quebradas
  
  ```{r tabla}



denuncias_buffer_quebradas %>%
  st_drop_geometry () %>%
  filter(!is.na(TIPO) & TIPO != "") %>%
  group_by(TIPO_inf) %>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)%>%
  datatable(rownames= FALSE, colnames = c( "Tipo de denuncia", "Cantidad de denuncias"),options = list (language = list (url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"), dom = "Bfrtip"))  




```



### Cantidad de denuncias ambientales alrededor de rutas importantes de Costa Rica

```{r }
## Cantidad de denuncias ambientales alrededor de autopistas

denuncias_buffer_autopistas %>%
  st_drop_geometry () %>%
  filter(!is.na(categoria) & categoria != "") %>%
  group_by(TIPO_inf) %>%
  summarise (registros = n ()) %>%
  arrange (desc(registros)) %>%
  slice (1:10)%>%
  datatable(rownames= FALSE, colnames = c( "Tipo de denuncia", "Cantidad de denuncias"),options = list (language = list (url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"), dom = "Bfrtip"))  
```

denu_depu1_GAM <-
  denu_depu %>%
  st_transform(crs = 5367)%>%
  filter(Provincia == "San José" | Provincia== "Alajuela"| Provincia == "Heredia" | provincia == "Cartago")
