####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### ATIVIDADE EXTRA 2               ################
####################################################


#cj de dados iris

# qual a covariancia entre as variaveis sepal e petal lenght?

cov(iris$Sepal.Length,iris$Petal.Length)

# resposta: a covariância entre as variáveis é 1.274315. Portanto, uma 
# relaćão positiva de concordância

# há relaćão linear entre as variáveis?

cor(iris$Petal.Length,iris$Sepal.Length)
plot(iris$Petal.Length~iris$Sepal.Length)

#resposta: 

library(tidyverse)

  p <- iris %>% ggplot(aes(Petal.Length, Sepal.Length)) +
    geom_point(aes(colour = Species, size = Petal.Width)) +
    labs(colour = "Espécie", size = "Tamanho da pétala")
    xlab("Comprimento da sépala") + 
    ylab("Comprimento da pétala")
  
  p

  library(ggExtra)
  # histograma das marginais
  
  ggMarginal(p, type = "histogram")
  
  
  ggMarginal(p, type = "density")

  
  ggMarginal(p, type = "boxplot", groupColour = T)  
  
  
  pnew <- iris %>% filter(Species != "setosa")
  
  p <- iris %>% filter(Species != "setosa") %>% ggplot(aes(Petal.Length, Sepal.Length)) +
    geom_point(aes(colour = Species, size = Petal.Width)) +
    labs(colour = "Espécie", size = "Tamanho da pétala")
  xlab("Comprimento da sépala") + 
    ylab("Comprimento da pétala")
  
  p
  
  p <- iris %>% filter(Species == "setosa") %>% ggplot(aes(Petal.Length, Sepal.Length)) +
    geom_point(aes(colour = Species, size = Petal.Width)) +
    labs(colour = "Espécie", size = "Tamanho da pétala")
  xlab("Comprimento da sépala") + 
    ylab("Comprimento da pétala")
  p
  
  #de Forma geral, existe alguma relaćão linear entre as variaveis comprimento da sépala
  # e comprimento da pétala. no entanto, a relaćão linear é fraca dentro da espécie setosa
  
  
  
  ## comparar a distribuićão da variável comprimento da sépala
  
  p <- iris %>% ggplot(aes(x = Sepal.Length, y = Species)) +
    geom_violin() + labs(x = "Comprimento da sépala", y = "Espécies")
p

  