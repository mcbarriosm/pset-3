## Mayra Catalina Barrios Mesa
## Codigo: 201911563
## Version de R:
R.version.string
## [1] "R version 4.2.2 (2022-10-31)"

## ------- PROBLEM-SET 3 -------

## Configuracion inicial 
rm(list = ls()) # limpia el entorno de R

## Llamar y/o instalar las librerias a usar:
 
require(pacman)
p_load(tidy, tidyverse, rio, skimr, coefplot,
       arrow, ## read parque files
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer ,# export tables to latex 
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar
       ggmap, ## get_stamenmap 
       osmdata, ## packages with census data
       ggspatial, ## mapas de datos espaciales
       rvest, ## web-scraping
       textcat, stringi, tm, cluster, worldcloud ## text-data
) 

## PUNTO 1:

## 1.1. Estimaciones:

## Importar los datos de las regresiones
datos <- read_rds("input/data_regresiones.rds", refhook = NULL) %>% as.tibble()
head(datos)

## Modelo econometrico 1:

# lm function

lm(formula = price ~ rooms + bathrooms , data = datos) 
lm(formula = price ~ rooms + bathrooms - 1, data = datos) 

# Linear regression
ols_1 = lm(price ~ rooms + bathrooms , data = datos)
ols_1 %>% summary() 
summary(ols_1)$r.squared # R^2
summary(ols_1)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols_1)
ols_1$call # model
ols_1$coefficients # get coefficients
ols_1$na.action # rows's NA
ols_1$residuals # get residuals
summary(ols_1$residuals)
hist(ols_1$residuals)

# get predict values
ols_1 %>% predict()
datos$predict_ols = predict(object = ols_1 , newdata = datos )

## Modelo econometrico 2:

# lm function

lm(formula = price ~ property_type + surface_total , data = datos) 
lm(formula = price ~ property_type + surface_total - 1, data = datos) 

# Linear regression
ols_2 = lm(price ~ property_type + surface_total , data = datos)
ols_2 %>% summary() 
summary(ols_2)$r.squared # R^2
summary(ols_2)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols_2)
ols_2$call # model
ols_2$coefficients # get coefficients
ols_2$na.action # rows's NA
ols_2$residuals # get residuals
summary(ols_2$residuals)
hist(ols_2$residuals)

# get predict values
ols_2 %>% predict()
datos$predict_ols = predict(object = ols_2 , newdata = datos )

## Modelo econometrico 3:

# lm function

lm(formula = price ~ dist_cbd + dist_cole , data = datos) 
lm(formula = price ~ dist_cbd + dist_cole - 1, data = datos) 

# Linear regression
ols_3 = lm(price ~ dist_cbd + dist_cole , data = datos)
ols_3 %>% summary() 
summary(ols_3)$r.squared # R^2
summary(ols_3)$adj.r.squared # R^2 ajustado

# What is ols object?
View(ols_3)
ols_3$call # model
ols_3$coefficients # get coefficients
ols_3$na.action # rows's NA
ols_3$residuals # get residuals
summary(ols_3$residuals)
hist(ols_3$residuals)

# get predict values
ols_3 %>% predict()
datos$predict_ols = predict(object = ols_3 , newdata = datos )

## 1.2. Presentar resultados:

# joint models (modelsummary)
coeficientes <- list(ols_1, ols_2, ols_3)
msummary(coeficientes, output = "data.frame")
tabla <- msummary(coeficientes, output = "data.frame")
view(tabla)

# coefplot
mods = list('Modelo 1' = ols_1 , 'Modelo 2' = ols_2 , 'Modelo 3' = ols_3)

modelplot(mods) + coord_flip() + 
  labs(title = "Modelos econometricos con el precio de la vivienda como variable dependiente" , 
       subtitle = "Comparacion de modelos")

## 1.3. Exportar resultados:

# export table
export(tabla, 'output/resultados_regresiones.xlsx')

## PUNTO 2:

## 2.1. Descargar datos

## Poligono Bogotá, Colombia

bogota <- opq(bbox = getbb("Bogotá Colombia")) %>%
            add_osm_feature(key = "boundary", value = "administrative") %>%
            osmdata_sf()

bogota <- bogota$osm_multipolygons %>% subset(admin_level==9)

## Restaurantes (puntos):

## objeto osm
osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
          add_osm_feature(key="amenity" , value="restaurant") 
class(osm)

## extraer Simple Features Collection
osm_sf <- osm %>% osmdata_sf()
osm_sf

## Obtener un objeto sf
restaurantes <- osm_sf$osm_points 
restaurantes

## Parques (poligonos):

parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
            add_osm_feature(key = "leisure", value = "park") %>%
            osmdata_sf() %>% .$osm_polygons 
parques

## 2.2. Visualizar la info anterior:

##Visualizar los restaurantes (puntos):

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurantes , col="red")

##Visualizar los parques (poligonos):

leaflet() %>% addTiles() %>% addPolygons(data=parques)

## 2.3. Geocodificar direcciones:

##Restaurante Emilia Grace (Calle 65 #4a - 51) en Bogota

rest_emilia <- geocode_OSM("Calle 65 %4% A - 51, Bogotá", as.sf = T)
rest_emilia

## 2.4. Exportar mapa:

## Creacion un unico mapa de los restaurantes, parques y el restaurante Emilia Grace de la 
## ciudad de Bogota. Se adiciona la barra de escalas, la estrella del norte y un theme para 
## mejorar la apariencia del mapa.

mapa_bog <- ggplot(data = bogota) +
      geom_sf(color = "black") +
      xlab("Longitud") + ylab("Latitud") +
      ggtitle("Mapa de los restaurantes y parques de Bogotá, Colombia", 
              subtitle = "Ubicación del restaurante Emilia Grace (punto verde)") +
      geom_sf(data = restaurantes, color = "red") +
      geom_sf(data = parques, color = "blue") +
      geom_sf(data = rest_emilia, color = "green") +
      scalebar(data = bogota, dist = 5, transform = T, dist_unit = "km") + 
      annotation_scale() + annotation_north_arrow(location = "topleft") + theme_linedraw()

mapa_bog

## PUNTO 3:

## 3.1. Leer URL y crear un objeto que contenga el HTML de la pagina

## Leer URL

my_url <- "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"

browseURL(my_url) ## Ir a la pagina

## Crear objeto que contiene el HTML de la pagina como un objeto xml_document

my_html <- read_html(my_url) ## Leer el HTML de la pagina

class(my_html) ## Ver la clase del objeto

view(my_html)

## 3.2. Extraer el titulo de la pagina (Departamentos de Colombia)

my_html %>% html_elements("h1") %>% html_text()

## Usando el xpath:

my_html %>% html_node(xpath = '//*[@id="firstHeading"]/span') %>% html_text()

## 3.3. Extraer la tabla que contiene los departamentos de Colombia

my_table <- my_html %>% html_table()

## numero de tablas extraidas

length(my_table)

my_table[[4]]

## creacion del objeto

tabla_departamentos <- my_table[[4]]
tabla_departamentos

## Exportar la tabla de departamentos de Colombia

export(tabla_departamentos, 'output/tabla_departamento.xlsx')

## 3.4. Extraer los parrafos del documento (elementos con etiqueta p)

my_html %>% html_elements("p") %>% html_text()

## crear objeto

parrafos <- my_html %>% html_elements("p") %>% html_text()

## vector de caracteres a corpus

corpus <- Corpus(VectorSource(parrafos)) ## formato de texto
class(corpus)

## matriz con terminos

tdm_corpus <- TermDocumentMatrix(corpus)
class(tdm_corpus)

## en columnas estan los parrafos y en las filas el numero de palabras

dim(tdm_corpus)

## frecuencia de palabras (se repiten al menos 5 veces)

findFreqTerms(tdm_corpus, lowfreq = 5)
frecuentes <- findFreqTerms(tdm_corpus, lowfreq = 5)

## palabras con las que mas se asocian las primeras 5 palabras del vector frecuentes

findAssocs(tdm_corpus, frecuentes[1:5], rep(x = 0.45, rep = 50))

## convertir el objeto en una matriz de frecuencias

matriz_parrafos <- as.matrix(tdm_corpus) ## lo vuelve una matriz
dim(matriz_parrafos)
view(matriz_parrafos)

## sumar la frecuencia de cada palabra

frec_words <- sort(rowSums(matriz_parrafos), decreasing = T)
class(frec_words)
df_words <- data.frame(word = names(frec_words), n = frec_words)

## Graficar la nube de palabras

wordcloud(words = df_words$word, freq = df_words$n, min.freq = 5,
          max.words = 20, random.order = T, rot.per = 0.15, scale = c(2,1))

wordcloud(words = df_words$word, freq = df_words$n, min.freq = 1,
          max.words = 2000, random.order = F, colors = brewer.pal(10,"Dark2"))
