##################################################################
### ATIVIDADE AVALIATIVA DE VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS ###
# Data de entrega: 13/05/2020                                   ## 
# ALUNO: FRANCISCO ROSA DIAS DE MIRANDA          N.USP: 4402962 ##
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

library(tidyverse)
# lendo os dados
library(MASS)
dt <- Cars93 %>% as_tibble
#forcando cast em tibble para evitar de dar problema com o tidyverse


#1) Construa uma tabela de frequências para o número de passageiros.

#tabulando os dados e acrescentando as frequencias relativas e acumuladas
tb <- dt %>% count(Passengers, sort = T) %>%
  mutate(cumsum = cumsum(n)) %>% 
  mutate(freq_r = n / sum(n)) %>% 
  mutate(cumsum_r = cumsum(freq_r), cumsum_r = scales::percent(cumsum_r), freq_r = scales::percent(freq_r))

#nomeando as colunas
colnames(tb) <- c("N. de Passageiros","Freq Absoluta","Acumulada Absoluta","Freq Relativa", "Acumulada Relativa")
#resposta:
tb %>% View


#2) Quantos modelos de carros são considerados no conjunto de dados?

#como os modelos de carros são únicos no conjunto de dados, temos:
dt$Model %>% length

# R: São considerados 93 modelos de carros neste conjunto de dados.


#3) Quantas montadoras (manufacturer) de carros há?

#neste caso, como uma marca pode ter mais de um modelo, consideremos as observacoes distintas:
dt$Manufacturer %>% unique %>% length
# R: existem 32 montadoras de carros neste conjunto de dados.

#4) Qual marca tem o preço médio dos seus carros maior?

# Calculando a média de preco para cada marca e ordenando o vetor.
medias <- dt$Price %>%
          aggregate(by = list(dt$Manufacturer), FUN = mean) %>%
          arrange(desc(x))
#resposta:
medias[1,]

#R: A marca com o maior preco médio é Infiniti, cujos carros custam em média 47.9.
#   Contudo, note que esse valor foi influenciado por este ser o único carro comerci-
#   -alizado pela marca.
#   Pois a Mercedez-Benz, embora possua o carro com maior preco, comercializa 
#   outro modelo além desse, com preco médio muito menor, puxando a média para baixo.
#   É interessante ressaltar que se considerássemos, por exemplo, qual marca possui 
#   o maior preco máximo médio ou somente o carro mais caro, Mercedez-Benz seria a
#   escolhida.


#5) Faça um gráfico de Pareto para a variável airbags.

library(qcc)
# tabulando os dados
ab <- dt$AirBags %>% table
# gerando o gráfico
ab %>% pareto.chart(main = " ", las = 0,
             xlab = "Quantidade de airbags", 
             ylab = "Frequência Absoluta",
             ylab2 = "Porcentagem acumulada")


#6) Faça um gráfico de setores para os tipos de carros (small, midsize, compact)

#filtrando os dados e calculando a frequencia relativa
sec <- dt %>% select(Type) %>%
      filter(Type == "Small" | Type == "Midsize" | Type == "Compact") %>%
      count(Type, sort = T) %>%
      mutate(prop = n / sum(n), prop = scales::percent(prop))
#plotando o grafico de setores
sec$n %>% pie(labels = paste(sec$Type,sec$prop),
             main = "Tipos de carros")


#7) Faça um histograma para o comprimento do carro. 

dt$Length %>% hist(breaks = "Sturges",
                   col = "orange",
                   xlab = "Comprimento do carro",
                   ylab = "Frequência Relativa", main = " ", freq = F)

#8) Explore a distribuição dos preços dos carros. Para isso, use medidas estatísticas e gráficos.
#Interprete os valores obtidos e os gráficos (pelo menos 3 medidas e um gráfico).

# calculando a moda "na mão" 
# (retirada de https://tolstoy.newcastle.edu.au/R/help/01c/0770.html)

moda <- function(x) {
  z <- table(as.vector(x))
  names(z)[z == max(z)]
}

# plotando o gráfico
plot(density(dt$Price), type = "l", col = "green",xlim = c(0,max(dt$Price)), ylim = c(0,0.07),
     xlab = "Preco", ylab = "Densidade", main = "Densidade dos Precos Médios")
abline(v= median(dt$Price), col = "blue", lty= 2)
abline(v= mean(dt$Price), col = "red", lty= 1)
abline(v= moda(dt$Price), col = "black", lty= 3)
legend("topright", col = c("green", "red", "blue","black"),
       legend = c("Preco Médio", "Média", "Mediana", "Moda"),
       pch = c(15,NA,NA,NA), lty = c(0,1,2,3))

# Seja X a V.A. que representa a distribuicao dos precos.

print(paste("Moda: ", moda(dt$Price), "Mediana: ", median(dt$Price), "Média: ", round(mean(dt$Price),2)))

# X é assimétrica à direita, pois Moda(X) < Mediana(X) < Média(X).

# Medidas de Assimetria e Curtose
library(e1071)

(af <- skewness(dt$Price, type = 1))
#como af > 0, confirmamos que X possui assimetria positiva.

(k_m <- kurtosis(dt$Price, type = 1))

# Como k_m > 0, a distribuicao de X é leptocúrtica.


