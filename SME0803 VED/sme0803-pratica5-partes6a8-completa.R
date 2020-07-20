####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 5 (REFERENTE ÀS PARTES 6 a 8 ) #########
####################################################

# Este comando limpa o histórico das variáveis
rm(list = ls(all=TRUE))

library(boot)

names(cane)

# valores de n com coluna block = A
cane$n[cane$block=="A"]

# valores de n com coluna r >= 20
cane$n[cane$r>= 20]

# valores de n com colunas block = A E r >= 20 
cane$n[cane$block=="A" & cane$r>=20]

# valores de n com colunas block = A OU r >= 20 
cane$n[cane$block=="A" | cane$r>=20]

# o que fazem estes comandos? extrair uma ou mais colunas do banco de dados
cane[-1,]
cane[,-c(1,2)]

# Dados
x = cane$r
  
# Ordenar os dados
sort(x)

# ? Resp. Posicao dos dados ordenados com 
# relacao ao conj. original
order(x)

# Quantidade de dados
n = length(x)
n

##########################
### Medidas de posição ###
##########################

### Média
(xb = mean(x))

### Curva de sensibilidade da média
cs = function(x) { x - xb }
plot(cs, 10, 30, lwd = 2, ylab = "CS(x)")
abline(h=0)
#segments(xb, cs(0), xb, 0, lty = 2)
#segments(xb, 0, 0, 0, lty = 2)

### Média aparada
(xa5 = mean(x, trim = 0.05))
mean(x, trim = 0.1)

# gráfico da média aparada para diferentes alfas
xa = function(alfa){
  mapply(function(a0) 
    mean(x, trim = a0), alfa)
}

plot(xa, 0.01, 0.5, 
     xlab =expression(alpha),
     ylab = expression(bar(x)[(alpha)]), 
     type = "p", pch = 20)

### Média geométrica
#install.packages("psych")
library(psych)

xg = geometric.mean(x)

### Média harmônica
xh = harmonic.mean(x)

### Mediana
Md = median(x)
Md

### Separatrizes ou quantis
summary(x)  
quantile(x)
alfa = seq(0.1, 0.9, by = 0.1)
decis = quantile(x, probs = alfa)
decisSAS = quantile(x, probs = alfa, type = 3)

############################
### Medidas de dispersão ###
############################

### Amplitude
A = max(x) - min(x)
A

# ou
extremos = range(x)
(A = (extremos[2] - extremos[1]))

# ou
# usando o diff
diff(x)

A = diff(range(x))
A

# ou
A = diff(c(min(x), max(x)))
A

### Amplitude interquartil
dq = quantile(x,0.75, names = F) - quantile(x,0.25, names = F)
dq

# usando o diff
diff(quantile(x,c(0.25,0.75), names = F))

### desvio médio
d = x - mean(x)
sum(d)

### desvio absoluto médio
dm = sum(abs(d))/n

# ou 
dm = mean(abs(d))

# ou
dm = mean(abs(x - xb))
dm

### desvio absoluto mediano
MAD = median(abs(x - Md))
MAD

### Variância
s2 = var(x)
s2

# ou: na "raça"
s2 = (sum((x - mean(x))^2))/(n-1)
s2

# desvio padrão
s = sd(x)
s

### O que acontece quando n aumenta?
nn = 5000 # 100 500 5000
media = 20
desviopadrao = 2
dados = rnorm(nn,media,desviopadrao) 
dados

# dividido por n-1
var(dados)

# dividido por n
sum((dados - mean(dados))^2)/n
###

### Propriedade: a variância de um conjunto de 
# valores constantes eh zero
valores = rep(23,30)
valores

var(valores)

### Propriedade: somar ou subtrair um valor 
# constante a cada elemento nao altera a variância
y = x + 6 

var(x)
var(y)

### Propriedade: multiplicar cada elemento por um mesmo valor b
# altera a variância em b^2
w = x*3

var(w)
var(x)

var(x)*(3^2)

### Pergunta: O que podemos concluir sobre outras medidas de 
# dispersão?
max(x) - min(x)
max(y) - min(y)

quantile(x,0.75, names = F) - quantile(x,0.25, names = F)
quantile(y,0.75, names = F) - quantile(y,0.25, names = F)

### coeficiente de variação
(cv = sd(x) / abs(mean(x)))

### amplitude studentizada
As = A/s

### ou
(As = (max(x) - min(x)) / sd(x))

### ou
extremos = range(x)
(As = (extremos[2] - extremos[1]) / sd(x))


### Gráfico de caixas (Box plot)
boxplot(cane$x)
boxplot(cane$x, pch = 20)
boxplot(cane$x, pch = "*", horizontal = TRUE, xlab = "Espessura (mm)")

bx = boxplot(cane$x, plot = FALSE)
names(bx)
bx$stats
class(bx$stats)

summary(cane$x)

boxplot(cane$x, pch = "*", horizontal = TRUE, xlab = "Espessura (mm)")
identify(bx$out, rep(1, length(bx$out)), match(bx$out, cane$x))

#######################################
### Medidas de assimetria e curtose ###
#######################################
install.packages("e1071")
#library(moments): não usaremos este pacote, não o carreguem
library(e1071)


### Segundo coeficiente de assimetria de Pearson
ap2 = 3*(mean(cane$x) - median(cane$x))/sd(cane$x)
ap2

# momento amostral central com o pacote e1071
moment(cane$x, order = 2, center = TRUE)
#n = length(x)
(n/(n-1))*moment(cane$x, order = 2, center = TRUE)
var(cane$x)

# momento amostral não central com o pacote e1071
moment(cane$x, order = 2, center = FALSE)

### Coeficiente de assimetria de Fisher-Pearson
# "na raça"
moment(cane$x, order = 3, center = TRUE)/(moment(cane$x, order = 2, center = TRUE)^(1.5))
# ou 
skewness(cane$x, type = 1)

# ainda podemos calcular
skewness(cane$x, type = 2)
skewness(cane$x, type = 3)
?skewness

### Coeficiente percentílico de curtose
Kp = (quantile(cane$x,0.75, names = F) - 
        quantile(cane$x,0.25, names = F))/
  (2*(quantile(cane$x,0.9, names = F) - 
       quantile(cane$x,0.1, names = F)))
Kp

### Coeficiente momento de curtose
# "na raça"
Km = moment(cane$x, order = 4, center = TRUE)/(moment(cane$x, order = 2, center = TRUE)^2) - 3
Km
# ou
kurtosis(cane$x, type = 1) 

# ainda podemos calcular
kurtosis(cane$x, type = 2) 
kurtosis(cane$x, type = 3) 
kurtosis(cane$x) 
?kurtosis



