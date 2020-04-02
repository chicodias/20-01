####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 4 ######################################
####################################################

### Histograma

# conjunto de dados gerados: salário em uma empresa
dados = read.table("salario.R", header = T)

names(dados)

# histograma todos 


# conhecendo a função hist
hist_dados = 



# histograma todos barras coloridas


# nomear os eixos


# calcular o k pela regra de Sturges
n = 



# calcular o k pela regra do quadrado




# vários gráficos na mesma figura
par(mfrow =c(2,2))
hist(dados$salario, col = "blue", breaks = ,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")
hist(dados$salario, col = "blue", breaks = ,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")
hist(dados$salario, col = "blue", breaks = ,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")
hist(dados$salario, col = "blue", breaks = ,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")


# Nota: o R usa como default ????
# mas aproxima pela ????
# Exercício: troque a função round(k_s) por outra
# que faça o que o default do R considera.

# considerar k = 
hist(dados$salario, col = "blue", breaks = ,
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")

# Conclusão: 

# acrescentar a densidade aproximada
hist(dados$salario, col = "blue",

     xlab= "Salário",
     ylab = "Frequência",
     main = " ")


# acrescentar os valores observados
rug(dados$salario)

# separar histograma por sexo
hist(          , col = " ",
     freq = T, 
     xlab= "Salário",
     ylab = "Frequência",
     main = " ")
hist(         , col = " ",
     freq = T, 
     xlab= "Salário",
     ylab = "Frequência",
     main = " ", )

# precisamos aumentar o eixo x
min(dados$salario)
max(dados$salario)


# acrescentando as linhas de densidades aproximadas



### Histograma e polígono de frequências
par(mfrow = c(1,1))

# um pouco de supérfulo

### Polígono de frequências

### Polígono de frequências com supérfulos


### Ogiva
library(fdth)
freq = hist_dados$counts
Freq = cumsum(freq)



# Polígono de frequências tipo escada



### Gráfico de pontos
stripchart(dados$salario, pch = 20, method = "stack", 
           xlab = "Salários",   
           main = "Sem arredondamento")
stripchart(round(dados$salario), pch = 20, 
           method = "stack", xlab = "Salários",   
           main = "Com arredondamento")

### Gráfico de linhas

# dados economics
names(economics)
?economics

plot(economics$date, economics$psavert)



### Gráfico de ramo-e-folhas
# dados gerados da massa corpórea (kg) de uma amostra de frequentadores da cantina
massa = c(81.2, 89.5, 67.0, 77.2, 66.8, 65.1, 69.9, 92.6, 49.7, 74.7, 51.9, 77.9, 75.4, 60.8, 84.5, 87.5, 69.4, 69.2, 73.9, 60.0, 94.8, 57.7, 72.8, 86.4, 77.6, 98.1, 81.1, 80.9, 75.1, 88.0, 68.5, 70.1, 106.6, 89.5, 85.9, 67.2, 78.9, 59.2, 75.4, 90.2, 86.3, 68.8, 87.8, 67.0, 84.6, 88.8, 76.9, 80.7, 60.4, 100.5, 82.1, 80.1, 60.5, 73.2, 62.5, 77.7, 70.6, 79.8, 70.2, 76.3, 81.9, 80.4, 75.9, 87.6, 70.0, 75.8, 89.2, 77.1, 75.1, 83.2, 93.0, 95.2, 73.5, 65.0, 74.0, 73.2, 75.1, 78.5, 76.9, 69.6)


