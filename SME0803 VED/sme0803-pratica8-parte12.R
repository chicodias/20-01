####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 8 (REFERENTE À PARTE 12 e etc ) ########
####################################################

rm(list=ls(all=TRUE))

### comentando brevemente sobre o pipe ###

library(dplyr)

exemplo <- c(2,4,8,12,16)

exemplo %>% sqrt %>% mean

mean(sqrt(exemplo))

# fonte: https://uc-r.github.io/pipe
attach(mtcars)
mtcars %>%
  filter(carb > 1) %>%
  group_by(cyl) %>%
  summarise(Avg_mpg = mean(mpg)) %>%
  arrange(desc(Avg_mpg))

### comparando
###############
### GGPLOT2 ###
###############

# Segundo Caio Moura:
# Os gráficos em ggplot são compostos por

# Dados
# Camadas
# Escalas
# Coordenadas
# Facetamento
# Temas

# O mínimo necessário para fazer um gráfico em ggplot é

# Dados
# Aesthetics 
# Camadas (dado pelo comando geom)

library(ggplot2) 

dados <- read.csv("DenguePorEstado.csv",header = TRUE)
dados <- data.frame(dados)
View(dados)
attach(dados)

# população versus dengue2019
ggplot(dados, aes(x = Populacao/10^6, y = Dengue2019)) + 
  geom_point()

# ou 
p <-dados %>%  ggplot(aes(x = Populacao/10^6, y = Dengue2019)) + 
               geom_point()
p

# acrescentando labels nos pontos
p + geom_text(label = Abreviacao, nudge_y = -1)

# mudando a escala das variaveis
p + scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  geom_text(label = Abreviacao, nudge_y = 0.1)

# outra forma
p + scale_x_log10() + scale_y_log10() + geom_text(label = Abreviacao, nudge_y = 0.1)

# se quiser pode carregar as escalas no p 
# (sem as legendas propositadamente)
p <- p + scale_x_log10() + scale_y_log10()
p

# acrescentando título e nomeando os eixos
p <- p + xlab("Log Populaćão em Milhoes") +
  ylab("Total de Casos de dengue (log)") +
  ggtitle("Casos de dengue no Brasil de Jan a Mar 2019")
p

# colorindo os pontos
p + geom_point(color = "#009E73")

# colorindo os pontos de acordo com uma variável
p + geom_point(aes(colour = Regiao), size = 3) +
  scale_color_brewer(palette = "Set1", name = "Região")

# deixando as labels sem encobrir ou longe dos pontos
library(ggrepel)
p + geom_text_repel(label = Abreviacao)

# o tamanho do ponto indica os casos de Chikungunya na região
p + geom_point(aes(colour = Regiao, size = Chi2019)) +
  labs(size = "Chikungunya", colour = "Regiao") + 
  geom_text_repel(label = Abreviacao)

# usando uma escala de cores melhor para daltonicos
library(viridis)

# já escolhi cores e legendas, vou carregar as opções em p
p <- p + geom_point(aes(colour = Regiao, size = Chi2019)) +
  labs(size = "Chikungunya", colour = "Regiao") + 
  geom_text_repel(label = Abreviacao) + 
  scale_color_viridis(option = "viridis", discrete = T,
                      name = "Região")

# acrescentando a linha da média de assassinatos
r = dados %>% summarise(rate = sum(Dengue2019)/sum(Populacao)*10^6) %>% 
  .$rate
r

# intercepto da linha log10 de r
p + geom_abline(intercept = log10(r), lty = 2, col = "darkgrey")

# posso atualizar o p e salvá-lo ou salvar 
# diretamente o resultado anterior

jpeg(file = "pontos_fig1-aula.jpg", width = 600, height = 400)
p
dev.off()


# agora pode tratar do gráfico de dispersão com outros gráficos usando
# o conjunto de dados 
library(tigerstats)
library(ggExtra)
head(m111survey)
attach(m111survey)

# com modificacoes fonte:https://www.r-graph-gallery.com/277-marginal-histogram-for-ggplot2/

# classic plot
pp <- m111survey %>% ggplot(aes(x = height, y =  sleep)) +
  geom_point()+
  xlab("Altura (em polegadas)") + 
  ylab("Quantidade de horas dormidas na última noite")
pp 

# diferenciando por sexo
pp + geom_point(aes(colour = sex)) +
  scale_color_manual(name = "Sexo", labels = c("Feminino", "Masculino"), 
                     values = c("female" = "red", "male" = "blue"))


# diferenciando por sexo
pp + geom_point(aes(colour = sex, size = GPA)) +
  scale_color_manual(name = "Sexo", labels = c("Feminino", "Masculino"), 
                     values = c("female" = "red", "male" = "blue")) + 
  labs(size = "Grade Point Average")
  
# formato do ponto 
pp <- pp + geom_point(aes(colour = sex, size = GPA, shape = seat)) +
  scale_color_manual(name = "Sexo", labels = c("Feminino", "Masculino"), 
                     values = c("female" = "red", "male" = "blue")) + 
  labs(size = "Grade Point Average") +
  scale_shape_manual(name = "Carteira", labels = c("Frente", "Meio","Fundo"),
                     values = c("1_front" = 19, "2_middle" = 17, "3_back" = 15))

pp

# mudando o tema do gráfico, o layout 
pp <- pp + 
  theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        legend.key = element_blank()) 
pp

# with marginal histogram
ggMarginal(pp, type = "histogram") 

# marginal density
ggMarginal(pp, type = "density") 

# marginal boxplot
ggMarginal(pp, type = "boxplot") 

# também podemos salvar o que quisermos
jpeg(file = "m11.dispersao.boxplot-aula.jpg", width = 600, height = 400)
p
dev.off()



###################
### Violin plot ###
###################

# Baseado em: STHDA (online)
# (Statistical tools for high-throughput data analysis)
library(tidyverse)
library(ggplot2)
library(titanic)
library(Hmisc)

titanic <- titanic_train
View(titanic)

titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

names(titanic)
attach(titanic)

# violin plot: preço passagem por classe 
p <- titanic %>% ggplot(aes(x = Pclass, y = Fare)) +
  geom_violin()
p

# violin plot: idade por classe
p <- titanic %>% ggplot(aes(x = Pclass, y = Age)) +
  geom_violin()
p

# change the labs
p <- p + 
  labs(title="Gráfico das idades por classe",
       x="Classe", y = "Idade")
p

# Rotate the violin plot
p + coord_flip()

# Choose which items to display :
p + scale_x_discrete(limits = c("2", "3"))

# violin plot with mean points
p + stat_summary(fun.y = mean, geom = "point",
                 shape = 23, size = 2, color = "red")

# violin plot with median points
p + stat_summary(fun.y = median, geom = "point",
                 shape = 23, size = 2, color = "red")

# Add median and quartile
p + geom_boxplot(width = 0.1)

p + geom_boxplot(width = 0.1, color = "blue")

# Function to produce summary statistics (mean and +/- sd) 
data_summary <- function(x) { 
  m <- mean(x) 
  ymin <- m-sd(x)   
  ymax <- m+sd(x) 
  return(c(y=m,ymin=ymin,ymax=ymax)) 
}

p + stat_summary(fun.data= data_summary)

# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

# Change violin plot line colors by groups
p <- titanic %>% ggplot(aes(x = Pclass, y = Age, color = Pclass)) +
  geom_violin(trim = F)
p

# Use custom color palettes
p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Use brewer color palettes
p + scale_color_brewer(palette="Dark2")

# Use grey scale
p + scale_color_grey() + theme_classic()   

# Use single color
ggplot(titanic, aes(x=Pclass, y=Age))+
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()


# change the legend position
p + theme(legend.position = "top")
p + theme(legend.position = "bottom")
p + theme(legend.position = "none")


# change the order of items in the legend
p +  scale_x_discrete(limits = c("2","1", "3"))

# Separate violin plot colors by groups
ggplot(titanic, aes(x=Pclass, y=Age, fill = Sex)) +
  geom_violin()

# Add dots
p + geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(1))

# Change colors
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# change the theme
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_classic()

p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_minimal()



# Dica: se o grafico tiver muitos bytes 
compactPDF("/")

#################
### GGANIMATE ###
#################

### Materia elaborado pelos aprendizes do NEA
### Flávio de Cassio Appolinario e
### Teh Led Red
rm(list=ls(all=TRUE))

library(gganimate)
library(magrittr)
library(gapminder)
library(gifski)
library(png)
library(dplyr)

dados <- read.csv("tendencia.csv", sep=",", header = T)

View(dados)

attach(dados)

# Somente Araraquara
Araraquara <- dados %>%
  filter(Regiao == "Araraquara")

# <- dados[dados$Regiao == "Araraquara",]

ggplot(Araraquara, aes(x=Semana, y=Casos)) +
  geom_point(aes(group = seq_along(Semana))) +
  geom_line() +
  ggtitle("Casos de dengue em Araraquara no ano 2018") +
  transition_reveal(Semana)

anim_save("Araraquara_dengue-aula.gif", last_animation())

ggplot(dados, aes(x=Semana, y=Casos, col=Regiao)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(type="qual") +
  theme_minimal()


##################
### Cartograma ###
##################

### Exemplo elaborado por Antônio Carlos 
# Herling Ribeiro Junior e Juliana Cobre

rm(list=ls(all=TRUE))

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

tm_shape(completo) + 
  tm_polygons("RED_HOMICI", 
              style = "jenks",  
              title = 'Variação do número de\n 
              homicídios em SP', textNA = 'NA') +
  tm_layout(frame = FALSE, 
            legend.position = c("left", "bottom"))

tmap_mode("view")
tmap_last()

mapa_sp <- tmap_last()
tmap_save(mapa_sp, "mapa_sp.html")

### Exemplo elaborado por Antônio Carlos 
# Herling Ribeiro Junior e Juliana Cobre

rm(list=ls(all=TRUE))

library(maptools)
library(cartogram)
library(tmap)

data(World)

tm_shape(World) +
  tm_polygons("economy",palette = "viridis",
              title = "Nível econômico \nde cada região") +
  tm_format("World")


### Exemplo elaborado por Antônio Carlos 
# Herling Ribeiro Junior,
# Flávio de Cassio Appolinario, Teh Led Red
# e Juliana Cobre.

rm(list=ls(all=TRUE))

library(rgdal)  
library(leaflet) 
library(dplyr)  
library(ggplot2)
library(sf)  
library(plotly)
library(plyr)

#Carregar base de dados
completo <- st_read(dsn = ".", layer = "35MIE250GC_SIR")

# Filtrar dados - aumentou
aumentou <- completo[completo$RED_HOMICI > 1, ]

# Filtrar dados - reduziu
reduziu <- completo[completo$RED_HOMICI < 1, ]

# Converter para valor absoluto
reduziuPosi <- abs(reduziu$RED_HOMICI)

# Converter para valor absoluto
aumentouPosi <- abs(aumentou$RED_HOMICI)

# Plotagem
plot(st_geometry(aumentou))

#Escolher azul para as cidades que reduziu
azul = colorNumeric("Blues", domain = reduziuPosi)

#Escolher vermelho para as cidades que aumentou
vrm = colorNumeric("Reds", domain = aumentouPosi)

#inicia leaflet 
leaflet() %>%
  #addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
  addProviderTiles(providers$Esri.WorldStreetMap)%>%
  #addProviderTiles(providers$OpenStreetMap.BlackAndWhite)%>%
  
  # poligonos para cidades onde nao existem dados
  addPolygons(data = completo,  weight = 1, fillColor = "LightGray" ,
              color = "black",
              fillOpacity = 0.9,
              label = (~completo$NM_MICRO),
              smoothFactor = 0.5,
              popup = paste0("N?o possui dados")
              
  )%>%  
  
  # poligonos para cidades onde reduziu
  addPolygons(data = reduziu,  weight = 1, fillColor = ~azul(reduziuPosi),
              color = "black",fillOpacity = 0.9,
              label = (~reduziu$NM_MICRO),
              smoothFactor = 0.5,
              popup = paste0(reduziu$NM_MICRO, " : ",
                             reduziuPosi, "%")
              
  )%>%
  
  # poligonos para cidades onde aumentou
  addPolygons(data = aumentou, weight = 1, fillColor = ~vrm(aumentouPosi),
              color = "black",fillOpacity = 0.9,
              label = aumentou,
              smoothFactor = 0.5,
              popup = paste0(aumentou$NM_MICRO, " : ",
                             aumentouPosi, "%")
  )%>%
  
  #barra de legenda lateral
  addLegend(data = reduziu, position = "bottomright", pal = azul, values = reduziuPosi, title = "Redução",
            labFormat = labelFormat(suffix = "%", prefix = "   "),
            na.label = "NA*",
            opacity = 0.9
  )%>%
  
  
  addLegend(data = aumentou, position = "topright", pal = vrm, values = aumentouPosi, title = "Aumento",
            labFormat = labelFormat(suffix = "%", prefix = "   "),
            na.label = "NA*",
            opacity = 0.9
  )

