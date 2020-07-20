####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 4 ######################################
####################################################

### Histograma

# conjunto de dados gerados: salário em uma empresa
dados = read.table("salario.R", header = T)

names(dados)
View(dados)

# histograma todos 
hist(dados$salario)

# conhecendo a função hist
hist_dados = hist(dados$salario)
names(hist_dados)

hist_dados$breaks
hist_dados$counts

# histograma todos barras coloridas
hist(dados$salario, col = "blue")

# nomear os eixos
hist(dados$salario, col = "blue",
     xlab = "Salário",
     ylab = "Frequência",
     main = " ")

# calcular o k pela regra de Sturges
n = length(dados$salario)
n


k_s = 1 + 3.3 * log10(n)
k_s


hist(dados$salario, col = "blue", breaks = round(k_s),
     xlab = "Salário",
     ylab = "Frequência",
     main = " ")


# calcular o k pela regra do quadrado
k_q = sqrt(n)
k_q

hist(dados$salario, col = "blue", breaks = round(k_q),
     xlab = "Salário",
     ylab = "Frequência",
     main = " ")



# vários gráficos na mesma figura
par(mfrow =c(2,2))
hist(dados$salario, col = "blue", breaks = 3,
     xlab= "Salário",
     ylab = "Frequência",
     main = "k = 3")
hist(dados$salario, col = "blue", breaks = round(k_s),
     xlab= "Salário",
     ylab = "Frequência",
     main = "k de Sturges")
hist(dados$salario, col = "blue", breaks = round(k_q),
     xlab= "Salário",
     ylab = "Frequência",
     main = "k regra do quadrado")
hist(dados$salario, col = "blue", breaks = 30,
     xlab= "Salário",
     ylab = "Frequência",
     main = "k = 30")


# Nota: o R usa como default a regra de sturges
# mas aproxima pela parte inteira do k aprox
# Exercício: troque a função round(k_s) por outra
# que faça o que o default do R considera.

par(mfrow =c(1,1))
# considerar k = 9
hist(dados$salario, col = "blue", breaks = 9,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")

# Conclusão: nem sempre o item breaks funciona.

# acrescentar a densidade aproximada
hist(dados$salario, col = "blue",
     freq = F,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")

lines(density(dados$salario), lty = 2)

# acrescentar os valores observados
rug(dados$salario)

# separar histograma por sexo
hist(dados$salario[dados$sexo == "F"], col = "orange",
     freq = T, 
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")
hist(dados$salario[dados$sexo == "M"], col = "light blue",
     freq = T, 
     xlab= "Salário",
     ylab = "Frequência",
     main = " ", add = T)

# precisamos aumentar o eixo x
min(dados$salario)
max(dados$salario)

#acrescentando as linhas de densidades aproximadas

hist(dados$salario[dados$sexo == "F"], col = "orange",
     freq = F, 
     xlab= "Salário",
     ylab = "Frequência",
     main = " ", xlim = c(30,70), ylim = c(0,0.15))
lines(density(dados$salario[dados$sexo == "F"]), lty = 2, col = "red")

hist(dados$salario[dados$sexo == "M"], col = "light blue",
     freq = F, 
     xlab= "Salário",
     ylab = "Frequência",
     main = " ", add = T)
lines(density(dados$salario[dados$sexo == "M"]), lty = 2, col = "blue")

### Histograma e polígono de frequências

hist_dados = hist(dados$salario, breaks = "Sturges",
                  xlab = "Salários",
                  ylab = "Frequencia",
                  main = " ")
lines(hist_dados$mids, hist_dados$counts, col = "blue")

par(mfrow = c(1,1))

# um pouco de supérfulo
hist_dados = hist(dados$salario, breaks = "Sturges",
                  col = "lightgrey",
                  border = "#A9A9A9",
                  las = 1,
                  xlab = "Salários",
                  ylab = "Frequencia",
                  main = " ")
lines(hist_dados$mids, hist_dados$counts, col = "blue")
### Polígono de frequências
plot(hist_dados$mids, hist_dados$counts, col = "blue")

### Polígono de frequências com supérfulos
plot(hist_dados$mids, hist_dados$counts, col = "blue", type = "l",
     las = 1,
     xlab = "Salários",
     ylab = "Frequencia",
     main = " ")


### Ogiva
library(fdth)
freq = hist_dados$counts
Freq = cumsum(freq)


plot(fdt(dados$salario), type = "cfh", col = "white",
     xlab = "Salário",
     ylab = "Frequencia Acumulada")
lines(hist_dados$breaks, c(0, Freq), col = "red")


# Polígono de frequências tipo escada

plot(hist_dados$breaks, c(0, Freq), type = "s",
     xlab = "Salário",
     ylab = "Frequencia Acumulada")

### Gráfico de pontos
stripchart(dados$salario, pch = 20, method = "stack", 
           xlab = "Salários",   
           main = "Sem arredondamento")
stripchart(round(dados$salario), pch = 20, 
           method = "stack", xlab = "Salários",   
           main = "Com arredondamento")

### Gráfico de linhas
library(ggplot2)
# dados economics
names(economics)
?economics

plot(economics$date, economics$psavert, type = "l",
     xlab = "Data",
     ylab = "% poupada", col = "blue", main = "% do salario poupado / total ganho - EUA"
     , las = 1)



### Gráfico de ramo-e-folhas
# dados gerados da massa corpórea (kg) de uma amostra de frequentadores da cantina
massa = c(81.2, 89.5, 67.0, 77.2, 66.8, 65.1, 69.9, 92.6, 49.7, 74.7, 51.9, 77.9, 75.4, 60.8, 84.5, 87.5, 69.4, 69.2, 73.9, 60.0, 94.8, 57.7, 72.8, 86.4, 77.6, 98.1, 81.1, 80.9, 75.1, 88.0, 68.5, 70.1, 106.6, 89.5, 85.9, 67.2, 78.9, 59.2, 75.4, 90.2, 86.3, 68.8, 87.8, 67.0, 84.6, 88.8, 76.9, 80.7, 60.4, 100.5, 82.1, 80.1, 60.5, 73.2, 62.5, 77.7, 70.6, 79.8, 70.2, 76.3, 81.9, 80.4, 75.9, 87.6, 70.0, 75.8, 89.2, 77.1, 75.1, 83.2, 93.0, 95.2, 73.5, 65.0, 74.0, 73.2, 75.1, 78.5, 76.9, 69.6)

stem(massa)

