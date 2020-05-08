##################################################################
### ATIVIDADE AVALIATIVA DE VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS ###
# Data de entrega: 10/05/2020                                   ## 
# ALUNO:FRANCISCO ROSA DIAS DE MIRANDA            N.USP: 4402962 #
##################################################################

## Para ver este arquivo sem problema de acentuação use o encoding
# UTF-8. Salve com o mesmo encoding.

## Todos devem preencher o cabeçalho acima no arquivo .R ou 
# nos manuscritos.

## A atividade pode ser feita com ou sem o uso do R. Sem o 
# uso do R significa fazer "à mão".

## Quem optar por usar o R deve:
# - usar o conjunto de dados completo ou seja o Cars93 do pacote MASS 
# - entregar este arquivo .R mudando o nome do arquivo para o número USP.
# - escrever as interpretações, respostas ou comentários usando o # na 
# frente e em seguida ao comando utilizado para isso.

## Quem optar por fazer à mão deve:
# - usar o conjunto de dados parcial que está em pdf.
# - entregar as soluções em um único arquivo pdf ou jpg, sendo o nome 
# do arquivo o número USP.
# - escrever as interpretações, respostas ou comentários em cada item.

## Todos devem entregar o arquivo pelo escaninho da atividade no 
# e-disciplinas.

# lendo os dados
library(MASS)
dt <- Cars93

#1) Construa uma tabela de frequências para o número de passageiros.

tb <- table(dt$Passengers)
barplot(tb, xlab = "Numero de Passageiros",
            ylab = "Frequencia Absoluta")

#2) Quantos modelos de carros são considerados no conjunto de dados?

length(dt$Model)
# R: São considerados 93 modelos de carros neste conjunto de dados.

#3) Quantas montadoras (manufacturer) de carros há?

marcas <- unique(dt$Manufacturer)
length(marcas)
# R: existem 32 montadoras de carros neste conjunto de dados.

#4) Qual marca tem o preço médio dos seus carros maior?

medias <- numeric(length(marcas))
names(medias) <- marcas

i <- 1
for(i in 1:length(marcas))
  medias[i] <- mean(dt$Price[dt$Manufacturer == marcas[i]])

medias <- sort(medias, decreasing = T)
medias[1]
#R: a marca com o maior preco médio é Infiniti, cujos carros custam em média 47.9.

#5) Faça um gráfico de Pareto para a variável airbags.

library(qcc)
ab <- table(dt$AirBags)
pareto.chart(ab, main = "", las = 0,
             xlab = "Quantidade de airbags", 
             ylab = "Frequência",
             ylab2 = "Porcentagem acumulada")


#6) Faça um gráfico de setores para os tipos de carros (small, midsize, compact)
tam <- table(dt$Type)
pie(tam)

#7) Faça um histograma para o comprimento do carro. 

hist(dt$Length, breaks = "Sturges")

#8) Explore a distribuição dos preços dos carros. Para isso, use medidas estatísticas e gráficos.
#Interprete os valores obtidos e os gráficos (pelo menos 3 medidas e um gráfico).


