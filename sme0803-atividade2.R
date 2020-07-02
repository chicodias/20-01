##################################################################
### ATIVIDADE AVALIATIVA DE VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS ###
# Data de entrega: 01/07/2020                                  ###
# ALUNO: FRANCISCO ROSA DIAS DE MIRANDA         N.USP: 4402962 ###
##################################################################

### LEIA ATENTAMENTE AS INSTRUÇÕES A SEGUIR ###
### ALGUMAS REGRAS QUE NÃO FOREM RESPEITADAS TÊM UM CUSTO EM PONTOS ###

## Para ver este arquivo sem problema de acentuação use o encoding
# UTF-8. Salve com o mesmo encoding. (Não atender a este critério 
# custará 0,5 ponto a menos)

## Todos devem preencher o cabeçalho acima no arquivo .R ou 
# nos manuscritos. (Não atender a este critério custará 0,5 ponto
# ponto a menos)

## A atividade pode ser feita com ou sem o uso do R. Sem o 
# uso do R significa fazer "à mão".

## Quem optar por usar o R deve:
# - usar o conjunto de dados completo ou seja o Cars93 do pacote MASS 
# - entregar este arquivo .R mudando o nome do arquivo para o número USP.
# - escrever as interpretações, respostas ou comentários usando o # na 
# frente e em seguida ao comando utilizado para isso.
# - deixar os install.packages comentados com # ou não os deixar no 
# código (Não atender a este critério custará 0,2 ponto ponto a menos cada)

## Quem optar por fazer à mão deve:
# - usar o conjunto de dados parcial que está em pdf.
# - entregar as soluções em um único arquivo pdf ou jpg, sendo o nome 
# do arquivo o número USP.
# - escrever as interpretações, respostas ou comentários em cada item.

## Todos devem entregar o arquivo pelo escaninho da atividade no 
# e-disciplinas.

#install.packages(c("GGally","DescTools","devtools","gglorenz","hrbrthemes","ggpubr"))
#library(devtools)
#devtools::install_github("haleyjeppson/ggmosaic")

library(MASS) # dados e gráfico de coordenadas paralelas
library(tidyverse) # ggplot2, dplyr, etc
library(GGally) # Similar a funcão pairs(), só que para o ggplot2
library(DescTools) # Coeficiente de Contingencia
library(tigerstats) # Qui-quadrado
library(gglorenz) # curva de Lorenz no ggplot
library(ggmosaic) # grafico de mosaico no ggplot
library(ggpubr) # ggbarplot e grafico de pontos
library(ggExtra)
library(RColorBrewer)
library(plotly)
library(e1071)
library(hrbrthemes)

dt <- Cars93 %>% as_tibble

#1) Analise a relação das variáveis EngineSize, Fuel.tank.capacity,
# Horsepower e Weight duas a duas. Use gráficos e/ou medidas estatísticas, 
# o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

# Selecionando as variáveis
ex1 <- dt %>% select(EngineSize, Fuel.tank.capacity, Horsepower, Weight)

# Utilizando a ggpairs para gerar os scatter plots, observar a densidade
# e calcular o coeficiente de Pearson
# baseado em https://www.rdocumentation.org/packages/GGally/versions/1.5.0/topics/ggpairs
p1 <- ex1 %>% ggpairs(aes(alpha = 0.3)) 
p1
# Imediatamente ao visualizar os scatter plots podemos notar altas correlacões
# lineares positivas, duas a duas, entre as variáveis.

# Vejamos os boxplots

b1 <- reshape2::melt(ex1) %>% 
  ggplot(aes(value)) + 
  facet_wrap(~variable, scales = 'free_x') +
  geom_boxplot() + theme(legend.position = "none",
                        panel.grid.major = element_blank(),
                        axis.title = element_blank(),
                        axis.text.y = element_blank())
b1


# Vamos olhar os mínimos e máximos
# Mediana + 1.5 IQ para os outliers
# gráficos baseados em http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
ol1 <- dt %>% filter(Horsepower > median(dt$Horsepower) + 1.5*IQR(dt$Horsepower) | 
                Horsepower < 73) %>%
  ggbarplot(x = "Make", y = "Horsepower",
            fill = "Type",           
            color = "white",         
            palette = c("#E41A1C","#377EB8","#4DAF4A"),
            sort.val = "asc", 
            sort.by.groups = F,
            rotate = T,
            xlab = " ")
ol1
# Temos quatro outliers para a variável Horsepower:
# os Esportivos Chevrolet Corvette e Dodge Stealth,
# e os Sedãs Cadillac Seville	e Infiniti Q45.

ol2 <- dt %>% filter(EngineSize > median(dt$EngineSize) + 1.5*IQR(dt$EngineSize) | 
                EngineSize < 1.3) %>%
  ggbarplot(x = "Make", y = "EngineSize",
            fill = "Type",               
            color = "white",            
            palette = c("#984EA3", "#377EB8", "#4DAF4A"),
            sort.val = "asc",          
            sort.by.groups = F,      
            rotate = T, 
            xlab = " ")
ol2
# Temos agora dois outliers, com o motor do mesmo tamanho:
# o Chevrolet Corvette, aparecendo novamente, e o Buick Roadmaster,
# um grande representante da categoria Large.

ol3 <- dt %>% filter(Fuel.tank.capacity > 22.5 | 
                Fuel.tank.capacity <= 10.6) %>%
  ggbarplot(x = "Make", y = "Fuel.tank.capacity",
            fill = "Type",              
            color = "white",            
            palette = c("#984EA3", "#377EB8", "#FF7F00"),
            sort.val = "asc",           
            sort.by.groups = F,      
            rotate = T,
            xlab = " ")
ol3
# Aqui, o outlier é a Van Chevrolet Astro, seguido pelo
# já citado Buick Roadmaster, 

ol4 <- dt %>% filter(Weight > 4054 | 
                Weight < 1967) %>%
  ggbarplot(x = "Make", y = "Weight",
            fill = "Type",           
            color = "white",         
            palette = c("#984EA3", "#377EB8", "#FF7F00"),
            sort.val = "asc",        
            sort.by.groups = F,      
            rotate = T,
            xlab = " ")
ol4
# Não temos outliers para essa variável, mas
# o Roadmaster também aparece como veículo mais pesado.
# Vamos agora, avaliar o que acontece na outra ponta:

# Apesar de não haverem outliers a esquerda para guiar nossa análise,
# vemos neste caso os mínimos são sempre
# praticamente os mesmos modelos de carro em todos os atributos.

# Conclusão: 
# Valores altos se manifestam em no máximo duas características.
# Enquanto valores baixos afetam as quatro variáveis.

# Se queremos maximizar os valores, temos que escolher uma ou
# duas das variáveis para isso. Já minimizar afeta as quatro
# variáveis conjuntamente.

# Agora que discutimos os outliers, podemos entender melhor o
# Parallel Coordinates Plot:
# baseado em https://rdrr.io/cran/MASS/man/parcoord.html
palette <- brewer.pal(6, "Set1") 
# usando as mesmas cores para o Type que nos barplots
palette <- palette[c(6,5,1,2,3,4)]
my_colors <- palette[as.numeric(dt$Type)]
p1 <- ex1[,c(3,1,2,4)] %>% parcoord(col= my_colors) 
p1

# As quatro variáveis estão fortemente relacionadas entre si.
# De forma geral, existe uma relacão linear forte entre as variaveis Tamanho
# do motor, Horsepower, Capacidade do Tanque e Peso, se tomadas duas a duas.

# Com a ajuda do gráfico de retas paralelas, pudemos ver como o Type
# é capaz de condicionar as variáveis e agrupá-las.

# Outra variável que explica o comportamento bimodal das densidades é o Man.trans.avail:
ex1 %>% ggpairs(aes(colour = dt$Man.trans.avail,alpha = 0.3))
# Conclusão: Carros automáticos geralmente são maiores nas quatro variáveis.
# Porém, chama a atencão no gráfico de Fuel.tank.capacity x Horsepower
# que neste caso os carros somente automáticos tem um padrão pouco linear.



#2) Analise a relação das variáveis DriveTrain e Horsepower. Use gráficos 
# e/ou medidas estatísticas, o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

# Selecionando as variáveis
ex2 <- dt %>%  select(DriveTrain, Horsepower)

# Gráfico de Violino para a variável Horsepower de acordo com o 
# DriveTrain
p2 <- ex2 %>% ggplot(aes(Horsepower, y = DriveTrain, color = DriveTrain)) +
              geom_violin() + geom_boxplot(width = 0.1) +
              coord_flip() +
              scale_colour_manual(values = c("#A65628","#F781BF","#999999")) +
              theme(legend.position = "none")
p2
# O gráfico de violino nos mostra que a distribuicão para a variável Horsepower
# é muito similar para os valores de Tracão Frontal e 4x4. Vamos analisar
# agora o quanto os dados possuem desigualdade:

# curva e polígono de lorenz baseada em
# https://github.com/jjchern/gglorenz

l2 <- ex2 %>%  ggplot(aes(x = Horsepower, color = DriveTrain, fill = DriveTrain)) +
  stat_lorenz(geom = "polygon", alpha = 0.65) +
  geom_abline(linetype = "dashed") +
  coord_fixed()+
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  stat_lorenz(desc = TRUE) +
  labs(x = "Percentual Acumulativo - Horsepower",
       y = "Percentual Acumulativo Total") 
l2


#funcao que calcula o Coeficiente de Gini Máximo
maxGini <- function(x){
  return (1 - (1/length(x)))
}

#tabela com resultados observados
ex2$Horsepower %>% aggregate(by = list(dt$DriveTrain),
                  FUN = function(x) c(
                                      Mean = mean(x),
                                      Median = median(x),
                                      SD = sd(x),
                                      Gini = Gini(x),
                                      maxGini = maxGini(x),
                                      Skewness = skewness(x),
                                      Kurtosis = kurtosis(x)))
Gini(dt$Horsepower)
1 - (1/length(dt$Horsepower))  
# O coeficiente de Gini para o Horsepower pode variar de 0 a 0.99, e o observado foi de
# 0,2. Logo, os dados não são muito desiguais para o Horsepower.

# Pela curva e o polígono de Lorenz, vimos que a maior desigualdade do Horsepower está 
# nos veículos 4x4, o que pode ser explicado por essa classe ter dois ouliers, além da
# variância maior. A assimetria também é maior nessa classe.
# Além disso, os veículos 4x4 e com traćão frontal são Leptocúrticos (curtose > 0).
# O grupo Rear, apesar de possuir maior variância que o grupo Front,
# é o único platocúrdico (curtose < 0), que além disso  possui coeficiente
# de Gini ligeiramente menor.
# Portanto, no geral, não há muita desigualdade para a variável Horsepower, embora
# todos os grupos de DriveTrain sejam lijeiramente assimétricos e tenham a cauda
# direita mais pesada. (assimetria > 0)

# Vamos gerar um gráfico de densidade
# https://ggplot2.tidyverse.org/reference/geom_density.html
# para ilustrar a distribuicão conjunta condicionada das variáveis
# a linha preta é a densidade original da Variável Horsepower.

d2 <- dt %>% ggplot(aes(Horsepower)) +
  geom_density(aes(fill = DriveTrain),position = "stack") + ylab("Densidade") +
  geom_vline(xintercept = mean(dt$Horsepower) + sd(dt$Horsepower),linetype="dashed") +
  geom_vline(xintercept = mean(dt$Horsepower) - sd(dt$Horsepower),linetype="dashed") +
  geom_text(x=mean(dt$Horsepower), y=0.017, label= expression(bar(x) %+-% sigma^2))+
  geom_density()
d2
# Através da densidade, podemos notar que um dos poucos pontos (Horsepower = 250)
# onde obteríamos uma boa classificacão para X, tem uma densidade tão baixa
# que é improvável que observemos algo ali.

# Foi possível dessa análise notar que o comportamento da distribuicão 
# da variável Horsepower não pode ser muito bem explicado pela variável DriveTrain.

# Por exemplo, se escolhemos um veículo com Horsepower próximo à sua média, é muito
# difícil definir em qual DriveTrain ele se enquadraria, pois as médias para 
# cada DriveTrain estão muito próximas umas das outras.
# (todas elas estão a um intervalo mean +/- sd da outra)

# Portanto, há uma relacão de independência entre Horsepower e DriveTrain.



#3) Analise a relação das variáveis AirBags e DriveTrain. Use gráficos 
# e/ou medidas estatísticas, o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

ex3 <- dt %>% select(AirBags, DriveTrain)

# Cálculo de C e X²
tb3 <- table(ex3)
tb3 %>% ContCoef
table(ex3) %>% chisqtestGC

# o Coeficiente de Contingência C = 0,30 não nos fornece indícios de
# associacão entre as variáveis. Já o valor de X² = 9.8277 parece estar longe de 0,
# o que nos indica uma certa dependência.
# 
# (p-valor < 0.04 nos faz rejeitar a hipotese de independencia para alfa < 4%) 
# Assim, o teste de hipóteses é inconclusivo em apontar associacão entre AirBags e Tracão,
# pois o valor de x² está muito próximo da região crítica para alfa = 4%.

# Gráfico Mosaico
p3 <- ex3 %>% ggplot +
  geom_mosaic(aes(x = product(AirBags), fill=DriveTrain)) +
  labs(x="Airbags",y= "DriveTrain") + 
  scale_fill_manual(values = c("#A65628","#F781BF","#999999")) +
  theme(axis.text.y = element_blank())
p3  
# Através dele, graficamente notamos que as distribuicões condicionadas
# entre as duas variáveis se comportam como se houvesse uma leve dependência
# (seriam similares as distribuicões marginais se não o fossem).

# Portanto, os indícios amostrais nos fazem pensar que as variáveis DriveTrain
# e Airbags possuem uma Dependência Indireta (por exemplo com a variável Type).



#4) Analise o comportamento da variável tipo do carro (Type) dado
# o tipo de direção (Man.trans.avail). Use gráficos 
# e/ou medidas estatísticas, o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

ex4 <- dt %>% select(Type, Man.trans.avail)
# Ordenando os fatores
ex4$Type <- factor(ex4$Type, levels =
                     c("Small", "Sporty", "Compact", "Midsize", "Van", "Large"))
# Grafico de barras empilhadas
b4 <- ex4 %>% ggplot(aes(x= Type, fill = Man.trans.avail)) + geom_bar() +
  xlab("Tipo de Carro") + ylab("Freq. Absoluta")
  
b4  
# Cálculo do Coeficiente de Contingência e de X²
tb4 <- table(dt$Type, dt$Man.trans.avail)
tb4 %>% ContCoef
tb4 %>% chisqtestGC

# Os Altos Coeficiente de Contingência (0.60) e Qui-Quadrado (X² = 52,8)
# nos indicam associacão entre as variáveis Tipo de Carro e Tipo de Transmissão.
# O p-valor do teste de independência é quase 0, portanto rejeitamos a
# hipótese de independência.
# Além disso, podemos observar a dependência graficamente:

# Todos os carros pequenos (Small) e os esportivos
# (Sporty) possuem a opcão de câmbio manual, enquanto que todos os carros
# grandes (Large) só possuem versão automática, assim como a maior parte
# das Vans.
#
# O coeficiente de Contingência, contudo, é apenas num grau moderado,
# pelo falo dos carros do tipo Midsize, um dos grupos mais expressivos,
# possuirem quase a mesma proporcão de veículos
# com e sem transmissão manual.

# Note como o grafico de mosaico se comporta no caso de dependência
m4 <- ex4 %>% ggplot +
  geom_mosaic(aes(x = product(Type), fill=Man.trans.avail)) +
  theme(axis.text.y = element_blank())
m4

# Já havíamos concluido que, ao condicionar a transmissão manual
# na análise feita no exercício 1, o comportamento linear observado
# poderia vir a mudar.

# Vamos agora utilizar a ordenacão feita aos fatores para estudar
# como os atribuitos do Exercício 1 se comportam:
palette1 <- palette[c(4,5,1,3,6,2)]
a <- dt %>% select(EngineSize, Fuel.tank.capacity,Horsepower,Weight,Type)
a$Type <- factor(a$Type, levels =
                     c("Small", "Sporty", "Compact", "Midsize", "Van", "Large"))

b4 <- reshape2::melt(a) %>% 
  ggplot(aes(value, y = Type, color = Type)) + 
  facet_wrap(~variable, scales = "free_y") +
  geom_violin() +
  scale_color_manual(values = palette1) +
  coord_flip() +
  theme_pubr(legend = "none", x.text.angle = 90) +
  xlab(" ")
b4

# Da forma que ordenamos, os carros manuais estão
# à esquerda, e os somente automáticos a direita.

# Curiosidades:
# Inicialmente, eu imaginava que os menores carros seriam os Compact,
# mas na verdade são os Small.

# Carros Esportivos são praticamente sedãs manuais, pois eles possuem
# distribuicões bem similares, sendo a única diferenca que todos os
# Sporty têm a opcão de câmbio manual, e cerca de metade dos Sedãs também.

# Carros automáticos são mais pesados que os manuais,
# o site Notícias Automotivas diz o seguinte:
# "Sobre o consumo, o automático tende a ser maior em comparação 
# com o automatizado. Ele depende de energia do motor para que funcione,
# principalmente os antigos, além disso, ele é mais pesado."
# (https://www.noticiasautomotivas.com.br/diferenca-entre-cambio-automatico-e-automatizado/)


#tabela com resultados observados
dt$Horsepower %>% aggregate(by = list(dt$Type),
                             FUN = function(x) c(
                               mean = mean(x),
                               SD = sd(x)))
# Tirando o grupo Small, todos parecem ter uma média semelhante. Porém,
# o gráfico mostra muito bem a alta variabilidade entre os Sedãs e os
# Esportivos em todos os parâmetros.
# Podemos notar agora que várias das médias se distanciam o suficiente
# pra permitir que classifiquemos razoavelmente uma observacão dada o
# seu valor para uma das variáveis.
# O EngineSize quase que segue uma escala linear conforme o Tipo, só
# não é perfeita por conta da alta variabilidade dos Esportivos.
# O mesmo para Capacidade do Tanque, exceto que as Vans tem capacidade
# maior que os Large.
# No Horsepower observamos o mesmo que no EngineSize, exceto que agora os
# Sedãs também se destacam como campeões em Potência.
# Já o Peso parece ser a melhor variavel para classificar a amostra. 
# Note um padrão quase que de "escada", e que até mesmo os campeões
# de variabilidade possuem uma variância menor aqui.



#5) Analise as variáveis Horsepower, Weight, Man.trans.avail, Passengers
# e DriveTrain. Use gráficos e/ou medidas estatísticas, o que achar 
# adequado, além de palavras para dar sua resposta. Interprete os 
# resultados.


ex5 <- dt %>% select(Horsepower,Weight,Passengers)

t5 <- dt %>% select(Man.trans.avail, DriveTrain)
 table(t5) %>% chisqtestGC()
 table(t5) %>% ContCoef()
 
# As variáveis Man.trans avail e Drivetrain são independentes. 
# Temos valores bem baixos de C = 0,09 e X² = 0.77.
# Além disso, p-valor = 0.67 faz com que não rejeitemos a hipótese de 
# independência para um nível de significancia alfa < 67%.
 
# Observe o gráfico de mosaico ilustrando a independência, em como as
# distribuicões condicionadas são bem similares às marginais.

p5 <-  t5 %>% ggplot() +
   geom_mosaic(aes(x = product(Man.trans.avail), fill=DriveTrain)) +
   labs(x="Manual Transmission",y= "DriveTrain") + 
   theme(axis.text.y = element_blank())
p5
 
# Já analisamos a correlacão linear entre Weight e Horsepower. Vamos olhar como
# Man.tras.avail agrupa os dados:

q5 <- dt %>% ggplot(aes(x= Weight, y = Horsepower, color = Type)) +
  geom_point() +
  scale_color_manual(values = palette) + 
  facet_grid(~Man.trans.avail) +
  theme(axis.text.y = element_blank()) + theme_pubr()
q5
# confirmando o que observamos na 1), os carros somente automáticos
# gráfico de coordenadas paralelas
palette5 <- brewer.pal(2, "Set1") 
my_colors <- palette5[as.numeric(dt$Man.trans.avail)]
ex5[,c(3,1,2)] %>% parcoord(col= my_colors)
# Notamos que os veículos automáticos no geral comportam mais passageiros,
# são mais pesados, porém não necessariamente mais potentes.

# Grafico de pontos para a variável passengers
r5 <- ggdotchart(dt, x = "Passengers", y = "Horsepower",
           color = "Type",                                # Color by groups
           sorting = "ascending",                        # Sort value in descending order
           #add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr(),
           rotate = T
)
r5

# Grafico Violino para a distribuicão de horsepower
d5 <- dt %>% select(Horsepower, Weight, Man.trans.avail, Passengers, DriveTrain, Type)

d5 %>% ggplot(aes(x=DriveTrain, y=Horsepower, fill = Man.trans.avail)) +
  geom_violin()

p5 <- dt %>% ggplot(aes(Horsepower, Weight)) +
  geom_point(aes(colour = Man.trans.avail, shape = DriveTrain)) +
  labs(colour = "Manual Transmission")
p5 %>% ggMarginal(type = "density",groupColour = T)


# histograma para drive train
ex5 %>% ggplot(aes(Weight, color = DriveTrain)) + geom_boxplot()

# Algumas variáveis que podem motivar estudos no banco de dados
dt %>% ggpaired("Man.trans.avail","DriveTrain")


# Para finalizar, plot interativo:
fig <-  dt %>%  plot_ly(x = ~Horsepower , y = ~Weight,
                        color = ~Type , colors = palette,
                        text = ~paste(Make, '<br>Price:', Price,'K USD',
                                      '<br>Origin: ', Origin,
                                      '<br>AirBags: ', AirBags,
                                      '<br>DriveTrain: ',DriveTrain,
                                      '<br>Passengers: ',Passengers,
                                      '<br>Manual Transmission?', Man.trans.avail)
)
fig







p5 <- dt %>% ggboxplot(x = "Passengers", y = "Weight", color = "Type")
p5 %>% facet(facet.by = "Type")

q55 <- dt %>% ggboxplot(x = "Man.trans.avail", y = "Horsepower", color = "Type")
q55 %>% facet(facet.by = "Type")



ggdotchart(dt, x = "DriveTrain", y = "Weight",
           color = "Man.trans.avail",                                # Color by groups
           sorting = "ascending",                        # Sort value in descending order
           #add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr(),
           rotate = T
           
)

# outra forma de vermos fatos sobre os automáticos
palette <- brewer.pal(2, "Set1") 
my_colors <- palette[as.numeric(dt$Man.trans.avail)]
#grafico de coordenadas paralelas
parcoord(ex1[,c(3,1,2,4)] , col= my_colors)








ex5 %>% ggplot(aes(x = Passengers, fill = Man.trans.avail)) + geom_bar()
ex5 %>% ggplot(aes(x = Passengers, fill = DriveTrain)) + geom_bar()



ex5 %>% ggplot(aes(Horsepower, color = Man.trans.avail)) + geom_boxplot()


ex5 %>% ggplot(aes(x = Weight, y = Horsepower)) + geom_point(aes(color = DriveTrain, size = Passengers, shape = Man.trans.avail))



ex1 <- dt %>%  filter(Type == "Midsize") %>%
  select(EngineSize,Fuel.tank.capacity,Horsepower,Weight,Man.trans.avail,Type) 

   ex1 %>% ggpairs(aes(colour = Man.trans.avail,alpha = 0.3))




