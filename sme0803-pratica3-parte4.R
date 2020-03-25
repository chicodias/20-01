####################################################
### VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS - SME0803 ###
### PROFA. JULIANA COBRE - ICMC/SUP ################
### PRÁTICA 3 (REFERENTE À PARTE 4) ################
####################################################

# Este comando limpa o histórico das variáveis
rm(list = ls(all=TRUE)) 

# Vetor de dados
dados = c("Facebook",  "Indicação", "Facebook",  "Instagram", "Facebook",  "Instagram",
  "Instagram", "Instagram", "Instagram", "Instagram", "Indicação", "Instagram",
  "Instagram", "Instagram", "Facebook",  "Instagram", "Indicação", "Instagram",
  "Facebook",  "Indicação", "Instagram", "Instagram", "Indicação", "Facebook", 
  "Indicação", "Instagram", "Facebook",  "Facebook",  "Instagram", "Facebook", 
  "Instagram", "Indicação", "Indicação", "Outro",     "Indicação", "Instagram",
  "Instagram", "Outro",     "Facebook",  "Instagram", "Facebook",  "Instagram",
  "Indicação", "Instagram", "Instagram", "Indicação", "Instagram", "Facebook", 
  "Instagram", "Instagram", "Instagram", "Outro",     "Instagram", "Facebook", 
  "Instagram", "Outro",     "Facebook",  "Outro",     "Facebook",  "Indicação",
  "Instagram", "Instagram", "Instagram", "Instagram", "Facebook",  "Facebook", 
  "Instagram", "Instagram", "Instagram", "Outro",     "Instagram")

# Quantidade de respostas = comprimento do vetor de dados
n = length(dados)
n

### frequências (reveja sme0803-pratica2-parte3, se necessário)
# frequência absoluta
freq =  table(dados)

# frequência relativa (%)
freq_r =  freq / n * 100

### Gráfico de barras: precisa de um vetor de frequências
barplot(freq)

### Gráfico de barras ordenadas por freq. decrescente
# ordenar as frequências relativas (%)
freq_r = sort(freq_r, decreasing = T)
freq_r

# gráfico de barras
barras_ordenadas = barplot(freq_r,
                                  xlab = "Como conheceu o produto?", ylab = "Porcentagem")
  
# frequências relativas (%) dentro das barras
text(barras_ordenadas, freq_r/2, freq_r)

# arredondar frequências
barras_ordenadas = barplot(freq_r,
                           xlab = "Como conheceu o produto?", ylab = "Porcentagem")

text(barras_ordenadas, freq_r/2, round(freq_r,2))

# barras hachuradas
barras_hachuradas = barplot(freq_r,
                            xlab = "Como conheceu o produto?", ylab = "Porcentagem",
                            angle = 45, density = 15, col = "black")
text(barras_hachuradas, freq_r/2, round(freq_r,2), col = "red")

# colunas coloridas e 
colunas_coloridas = barplot(freq_r, horiz = TRUE,
                            xlab = "Como conheceu o produto?", ylab = "Porcentagem",
                            col = "orange")


# frequências relativas (%) dentro das colunas
text(freq_r/2, colunas_coloridas, round(freq_r,2))
# escolhendo as cores
cores = c("#00BFFF", "#7B68EE", "#F4A460", "#32CD32")

# barras com as cores escolhidas
barras_cores = barplot(freq_r, col = cores,
                       xlab = "Como conheceu o produto?", ylab = "Porcentagem")
  
# frequências relativas (%) dentro das barras
text(barras_cores, freq_r/2, round(freq_r,2))
  
### Gráfico de barras empilhadas
# frequências separadas por faixa de idade
dados2 = matrix(c(5,13,10, 5, 14, 28, 3,2), ncol = 4, nrow = 2,byrow=T)
colnames(dados2) = c("Facebook", "Instagram", "Indicação", "Outros")
rownames(dados2) = c("Menor de 25", "A partir")
dados2

 # gráfico de barras empilhadas
barplot(dados2, col = c("#00BFFF","#F4A460"), 
        main = "Pesquisa de Opinião",
        xlab = "Como conheceu o produto", 
        ylab = "Quantidade")

#acrescentar legenda
legend("topright", col = c("#00BFFF","#F4A460"),
       legend = c("Menos de 25 de anos","A partir de 25 anos"),
       pch = 15)

#help para pch
?points

# exercício: se os dados tivessem sido dados em matrix?
barplot(as.matrix(freq_r), col = cores, width = 0.3, xlim =c(0,1))
legend(0.5,100,   col = cores,legend = c("Facebook", "Instagram", "Indicação", "Outros"), pch = 15)

### Gráfico de Pareto
library(qcc)

pareto.chart(freq, main = "", las = 0,
             xlab = "Como conheceu o produto", 
             ylab = "Quantidade",
             ylab2 = "Porcentagem acumulada")

pareto.chart(freq_r, main = "Grafico de Pareto", las = 0,
             xlab = "Como conheceu o produto", 
             ylab = "Frequencia (%)",
             ylab2 = "Porcentagem acumulada",
             col = "lightgray")

pareto.chart(freq_r, main = "Grafico de Pareto", las = 0,
             xlab = "Como conheceu o produto", 
             ylab = "Frequencia (%)",
             ylab2 = "Porcentagem acumulada",
             col = cores)

### Gráfico de setores (pie chart)
pie(freq)

# escolhendo as cores
pie(freq_r, col =cores)

# colocando as etiquetas
pie(freq_r,col = cores,
    labels = paste(names(freq_r),freq_r))
# melhor arredondar os valores
pie(freq_r,col = cores,
    labels = paste(names(freq_r),round(freq_r,2)))

# somente os valores
pie(freq_r,col = cores,
    labels = paste(round(freq_r,2)))
legend("topleft", col = c("#00BFFF","#F4A460"),
       legend = c("Instagram","Facebook","Indicaçao","Outro"),
       pch = 15)


# escala de cores cinza
pie(freq_r, col = gray(seq(0.4,1,length = length(freq_r))),
    labels = paste(names(freq_r),round(freq_r,2)))
# barras empilhadas: melhor com o ggplot2
barplot(as.matrix(freq_r), col = cores, width = 0.5, xlim= c(0,1))
legend("topright", col = cores,
       legend = c("Instagram","Facebook","Indicaçao","Outro"),
       pch = 15)
