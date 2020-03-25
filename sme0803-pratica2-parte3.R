####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 2 (REFERENTE À PARTE 3) ################
####################################################

#install.packages("tigerstats")
#library(tigerstats) # pacote para o conjunto de dados m111survey

#dados = m111survey

#?m111survey
dados = read.table("dadosm111survey.R",head = T)
names(dados)
colnames(dados) = c("altura","altura_ideal","horas_sono", "velocidade","sentimento_peso","amor_primeira","vida_et","local_sentar","media","dorme_suficiente","sexo","diferenca")

View(dados)

dados

dados$altura

attach(dados)

# tabela de contingência
table(sentimento_peso,sexo)

# tabela de contingência em uma variável
tab_sp_s = table(sentimento_peso,sexo)
tab_sp_s

# proporção pelo total geral
tab_sp_s/sum(tab_sp_s)

# marginal pelas linhas (sentimento_peso)
prop.table(tab_sp_s,1)

# marginal pelas colunas (sexo)
prop.table(tab_sp_s,2)



# arredondar
round(prop.table(tab_sp_s,2),2)

prop.table((tab_sp_s)*100,2)

# construir intervalos de classe para a variável altura
sort(altura)

# tamanho da amostra
n = length(altura) 
  
  # k de Sturges
  k_s = 1 + 3.3 * log10(n)
k_s

# k quadrado
k_q = sqrt(n)
k_q

# vamos escolher k = 8
k = 8
  
  # amplitude dos dados 
  A = max(altura) - min(altura) 
  
  # amplitude de cada classe
  A_c = A /k
  A_c #nao precisa arredondar 

  
  #aumentado uma amplitude para a ultima clases
  limites = seq(min(altura),max(altura) + A_c,A_c)
  limites

  #sem uma classe extra
  limites = seq(min(altura),max(altura),A_c)
  limites[length(limites)] = 80

# construir a tabela
  
  #frequencia absoluta
  freq = table(cut(altura, breaks = limites, right = FALSE))
  freq
  
  #frequencia relativa
  freq_r = freq/n
  freq_r

  #frequencia acumulada
  Freq = cumsum(freq)
  Freq

  #fac relativa
  Freq_r = Freq/n
  Freq_r
  
  # se quiser em %, multiplicar por 100
  freq_r_100 = freq_r *100
  round(freq_r_100,2)
  
  #freq ac em %
  Freq_r_100 = Freq_r *100
  round(Freq_r_100 = Freq_r *100)
  
# visualizar freq
  cbind(freq, Freq)

# Transformando as tabelas em um único data frame

# Primeiro selecionamos as classes usando a função levels
classes = levels(cut(altura, breaks = limites, right = FALSE))
classes

# Juntamos em uma única tabela e depois salvamos como data.frame
tabela = as.data.frame(cbind(classes, freq, round(freq_r,2), Freq, round(Freq_r,2)))

# Acrescentar nomes = números nas linhas
tabela = data.frame(tabela, row.names = c(1:length(classes)))
tabela

# Renomear as colunas
colnames(tabela) =c("Classes", "Freq.", "Freq. Rel.", "Freq. Acum.", "Freq. Acum. Rel.")

tabela

### alternativa usando o pacote fdth

install.packages("fdth")
library(fdth)


tabela2 = fdt(altura)
tabela2

tabela3 = fdt(altura, k = 8)
tabela3

tabela4 = fdt(altura, breaks = "Sturges" ) 
tabela4

tabela5 = fdt(altura, breaks = "FD") 
tabela5

tabela6 = fdt(altura, start = 51, end = 81, h = 5 ) 
tabela6

tabela6 = fdt(altura, start = 51, end = 82, h = 3.5 ) 
tabela6
# o que acontece se você colocar end < 81?
#come uma classe????

