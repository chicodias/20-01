#########  HOMICIDIOS DOLOSOS

if(!require("dplyr")) install.packages("dplyr")  # para usar o operador "%>%"
if(!require("ggplot2")) install.packages("ggplot2") 
if(!require("plotly")) install.packages("plotly") 
if(!require("plyr")) install.packages("plyr") 
if(!require("RootsExtremaInflections")) install.packages("RootsExtremaInflections") 
if(!require("sf")) install.packages("sf") #para utilizar o filtro para o shapefile
if(!require("rgdal")) install.packages("rgdal") # para carregar o shapefile
if(!require("tmap")) install.packages("tmap") # para carregar o shapefile



library(rgdal)   
library(leaflet) 
library(dplyr)   
library(ggplot2)
library(sf)  
library(ggplot2)
library(plotly)
library(plyr)
library(RootsExtremaInflections)
library(tmap)



#Carregar base de dados
completo <- st_read(dsn = ".", layer = "35MIE250GC_SIR")

tm_shape(completo) + tm_polygons("RED_HOMICI", style = "jenks",  title = 'Variação do número de\n homicídios em SP', textNA = 'NA') +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))
