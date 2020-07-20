####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 5 (REFERENTE ÀS PARTES 9 a 10 ) ########
####################################################

# Este comando limpa o histórico das variáveis
rm(list = ls(all=TRUE))

# pacote ineq
library(ineq)

# conjunto de dados fictício
dados =  c(18.9,  4.0,  6.1, 11.9,  2.0,  7.1,  7.3,  7.6,  
           8.1, 14.6,  2.1,  1.5,  3.7,  4.9,  8.9)

# resumo dos dados
summary(dados)

### Curva de Lorenz?
curva_lorenz = Lc(dados) 

# o que a função Lc calcula?
names(curva_lorenz)

# gráfico da Curva de Lorenz
plot(curva_lorenz)
plot(curva_lorenz, main = " ",  ylab ="q")

### ponto de discrepância máxima
jmax = which.max(curva_lorenz$p - curva_lorenz$L)
jmax

# discrepância máxima
Lmax = curva_lorenz$p[jmax] - curva_lorenz$L[jmax]
Lmax

# coordenadas do ponto de discrepância máxima
c(curva_lorenz$p[jmax], curva_lorenz$L[jmax])

# discrepância máxima no gráfico
# lembrar que a reta é a reta identidade
plot(curva_lorenz, main = " ",  ylab ="q")
segments(curva_lorenz$p[jmax], curva_lorenz$L[jmax],
         curva_lorenz$p[jmax], curva_lorenz$p[jmax],
         lty = 2)
text(curva_lorenz$p[jmax] + 0.07,
     curva_lorenz$L[jmax] + 0.15,
      label = expression(L[max]))
# colocando o texto "max" subescrito


### Índice de Gini
Gini(dados)

### Gráfico de dispersão

# leitura dos dados artificiais
x = c(154.3, 166.7, 154.8, 165.3, 160.9, 163.0, 171.5, 143.7, 161.4, 150.9,
      165.3, 171.8, 159.7, 150.7, 158.2, 160.2, 166.2, 169.2, 151.2, 161.7,
      149.3, 148.1, 157.5, 160.8, 160.1, 151.0, 163.1, 173.0, 164.4, 165.1,
      156.2, 160.6, 176.8, 166.5, 164.8, 154.9, 165.2, 169.4, 156.6, 160.3,
      154.0, 172.3, 159.6, 158.6, 168.0, 165.7, 157.7, 164.0, 161.8, 158.3,
      165.7, 162.9, 162.9, 156.8, 166.0, 162.7, 169.2, 165.4, 158.8, 147.0,
      161.3, 163.2, 153.0, 162.0, 147.2, 167.4, 141.0, 151.5, 169.6, 173.7,
      160.6, 162.2, 164.4, 156.2, 147.0, 177.6, 155.6, 167.5, 169.0, 153.1,
      155.8, 166.8, 157.3, 161.6, 155.9, 163.3, 154.9, 154.5, 161.5, 168.6,
      164.3, 159.9, 163.4, 152.8, 143.7, 154.2, 170.9, 174.6, 167.4, 167.2)

y = c(75.4,  75.4,  79.6,  70.4,  61.6,  83.6,  94.8,  70.5,  71.2,  65.5,
      88.3,  78.2,  80.1,  77.4,  84.6,  62.5,  75.7,  89.9,  79.3,  83.3,
      65.5,  74.1,  76.4,  52.9,  59.6,  71.5,  88.2,  87.2,  59.5,  93.9,
      84.1,  84.2,  73.7,  96.1,  82.6,  95.4,  80.2,  81.2,  87.5,  83.7,
      65.5,  77.3,  62.0,  84.7,  74.1,  91.3,  73.2,  65.5,  79.3,  96.5,
      63.2,  81.0,  81.2,  74.5,  80.6,  77.4,  81.3,  95.1,  73.6,  71.2,
      77.9, 102.4,  86.5,  97.6,  74.6,  90.6,  75.8,  77.4,  95.2,  93.9,
      95.4,  80.9,  91.4,  79.7,  85.8, 111.5,  60.6,  90.6,  77.5,  55.8,
      82.2,  76.3,  71.7,  82.0,  84.1,  89.8,  80.7,  75.0,  87.1,  84.5,
      72.1, 101.5,  72.8,  78.3,  87.5,  78.1, 105.4,  83.2,  83.1,  74.0)
      
# tamanho amostral
length(x)
      
### gráfico de dispersão
plot(x, y, pch = 20,
     xlab = "Altura(cm)",
     ylab = "Peso(kg)")
      
### covariância
# covariância entre x e y
cov(x,y)
      
# covariância entre y e x
cov(y,x)
      
# covariância entre x e x
cov(x,x)
      
# comparação com s_x^2
cov(x,x)-var(x)
      
### correlação entre x e y
cor(x,y)
      
# correlação de outra forma
cov(x,y)/(sd(x)*sd(y))
      
# transformação locação escala de x e y
a1 = 
b1 = 
a2 = 
b2 = 


      
# outro sinal para b
a1 = 
b1 = 
a2 = 
b2 = -2

### dados winter
library(texmex)
      
# dados winter do pacote texmex
?winter
      
names(winter)
      
View(winter)
      
rownames(winter)
      
summary(winter)
      
# por conveniência vamos usar somente os 150 primeiros dados
      
# painel de dispersão

      
# acertando a ordem e as legendas
ordem = c("O3", "NO2", "NO", "SO2", "PM10")
nomes = c("Ozônio",expression(NO[2]), "NO", expression(SO[2]),"PM10")

      
# matriz de covariância

      
# matriz de correlação


# construindo a função panel.cor
panel.cor = function(x,y, digits = 3)
{
 usr = par("usr")
 on.exit(par(usr))
 par(usr = c(0,1,0,1))
 r = cor(x,y)
 text(0.5,0.5, round(r, digits), cex=1.5)
}
      
# construindo a função panel.hist
panel.hist <- function(x, ...)
{
 usr <- par("usr"); on.exit(par(usr))
 par(usr = c(usr[1:2], 0, 1.5) )
 h <- hist(x, plot = FALSE)
 breaks <- h$breaks; nB <- length(breaks)
 y <- h$counts; y <- y/max(y)
 rect(breaks[-nB], 0, breaks[-1], y, col = "green", ...)
}
      
# acréscimo das correlações
pairs(winter[1:150,], upper.panel = panel.cor)
      
# sem a parte inferior do painel
pairs(winter[1:150,], labels = , lower.panel = )
      
# acréscimo de tendência
pairs(winter[1:150,], labels = , upper.panel = , 
      lower.panel = )
      
# com histograma
pairs(winter[1:150,], labels = , upper.panel = ,
      lower.panel = , diag.panel = )
      
panel.smooth2=function(x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1,  span = 2/3, iter = 3, ...)
      {
        points(x, y, pch = 20, col = "blue", bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
          lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
                col = "red", ...)
      }
      
# nova função panel.smoth2, não a default
pairs(winter[1:150,],  labels = , 
            upper.panel = ,  
            lower.panel = , diag.panel = )
      

### Vamos trabalhar com os dados Ilocos
# pacote necessário
library(ineq)

# informações sobre o conj. de dados Ilocos
?Ilocos


# sumáriodos dados


# apontando os "níves" da variável "urbanity"
# = c("Rural", "Urbana")

# construção da tabela com a v. "province" separada 
# nos níveis de "urbanity"
#tab1 = 

# marginais com relação a x


# marginais com relação a y


# ambas as marginais


# coloca em tabela a marginal de x


# coloca em tabela a marginal de y


# tabela de proporções


# tabela de proporções com relação à marginal x
#tab1rel = 

# marginais com relação a y
#addmargins(tab1rel, )

# marginais com relação a y em porcentagem com três dígitos

# tabela de tab1rel
#tab1relp = 

# gráfico de barras


# gráfico de barras separado por "province"

# na mesma barra "rural" e "urban"

### Gráfico mosaico

# apontando os níveis em "sex"

# tabela "province" e "sex"

# proporção "province" e "sex"

# "province" e "sex" em porcentagem

# marginais
#tab2marg =

# auxiliares
#k = 
#m = 

# soma total
#n = 

# multiplicação das frequências (supondo independência 
# entre province e sex)
#tab2ind = 

# nomeando linhas e colunas

# gráfico mosaico

# estatística de teste: qui quadrado de Pearson
#X2 = 

# diversos gráficos de caixa


# médias e variâncias do tamanho da família por província


# gráfico de médias e desvios padrões do tamanho 
# da família por província
#limy = c(0, 1.1 * max(tabmed + sqrt(tabvar)))
#gbarras = barplot(tabmed, xlab = "Província", 
#                  ylab = "Tamanho médio da família", 
#                  ylim = limy, col = "black", density = 10)
#arrows(gbarras, tabmed, gbarras, tabmed + sqrt(tabvar), 
#       angle = 90) 
#box()















      
      
      
      
      
      
      
      
      
      