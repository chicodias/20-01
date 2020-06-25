####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 7 (REFERENTE À PARTE 11 ) ##############
####################################################

### Fonte: https://rpubs.com/EstatBasica/Cap8

View(volcano)

# mapa de calor para dados volcano: paleta de cores blue-red
library(colorRamps)
filled.contour(x = 1:nrow(volcano), y = 1:ncol(volcano),
               z = volcano, color = blue2red, 
               plot.title = title(main = "Mapa Topográfico: Manga Whau",
                                  xlab = "Sul - Norte",
                                  ylab = "Leste - Oeste"),
               key.axis = axis(4, seq(90, 20, by = 10)))

# mapa de calor para dados volcano: paleta de cores viridis
library(viridis)
filled.contour(x = 1:nrow(volcano), y = 1:ncol(volcano),
               z = volcano, color = viridis, 
               plot.title = title(main = "Mapa Topográfico: Manga Whau",
                                  xlab = "Sul - Norte",
                                  ylab = "Leste - Oeste"),
               key.axis = axis(4, seq(90, 20, by = 10)))

# gráfico de perspectiva: blue-red
persp(volcano, phi = 40, theta = 60,
      col = heat.colors(ncol(volcano)*nrow(volcano), 0.8), 
      ticktype = "detailed", xlab = "Sul - Norte",
      ylab = "Leste - Oeste", zlab = "Altura(unidade)")

# gráfico de perspectiva: virids
persp(volcano, phi = 40, theta = 60,
      col = viridis(ncol(volcano)*nrow(volcano), 0.8), 
      ticktype = "simple", xlab = "Sul - Norte",
      ylab = "Leste - Oeste", zlab = "Altura(unidade)")

# curvas de nível
contour(x = 1:nrow(volcano), y = 1:ncol(volcano), volcano,
        col = "darkred", xlab = "Sul - Norte",  ylab = "Leste - Oeste")

### usando o pacote plotly
library(plotly)
plot_ly(z = ~volcano, type = "surface")

### o pacote plotly faz gráficos parecidos 
# com os do ggplot2 #ficadica

#################
### Wordcloud ###
#################

#Exemplo baseado em 
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

rm(list=ls(all=TRUE))
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Fonte dos dados: Kaagle
df = read.csv('hotels.csv')
#View(df)

# importar o arquivo texto 
text <- df$Description[1:100]

# processar os dados como um corpus
docs <- Corpus(VectorSource(text))

# se quiser, dar uma olhada no arquivo
inspect(docs)

# trocar os caracteres especiais por espaço
toSpace <- content_transformer(
  function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# converter o texto para letras minúsculas
docs <- tm_map(docs, content_transformer(tolower))

# remover pontuação
docs <- tm_map(docs, removePunctuation)

# remover espaços em branco extras
docs <- tm_map(docs, stripWhitespace)

# remover os números
docs <- tm_map(docs, removeNumbers)

# olhar o texto filtrado
inspect(docs)

# construir a matriz do documento: tabela contendo
# a frequência das palavras do texto
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

### primeira wordcloud
# apenas com esses "filtros", construir a primeira wordcloud.
set.seed(803)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# remover palavras comuns (stopwords) em inglês
docs <- tm_map(docs, removeWords, stopwords("en"))

# construir a matriz do documento
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# apenas com esses "filtros", construir a segunda wordcloud.
set.seed(803)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# remover palavras que devem ser desconsideradas 
docs <- tm_map(docs, removeWords, c("will", "hotel", "room", "rooms",
                                    "etc")) 

# construir a matriz do documento
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# apenas com esses "filtros", construir a terceira wordcloud.
set.seed(803)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# encontrar as palavras que ocorrem pelo menos trinta vezes
findFreqTerms(dtm, lowfreq = 30)

# encontrar quais as palavras do texto estão associadas com "best"
findAssocs(dtm, terms = "best", corlimit = 0.3)

# dar uma olhada na tabela de frequência do texto
head(d,10)

# considerar apenas o núcleo morfológico das palavras (em ingles)
docs <- tm_map(docs, stemDocument, "en")

# construir a matriz do documento
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# apenas com esses "filtros", construir a quinta wordcloud.
set.seed(803)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#construir a wordcloud com 50 palavras
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

