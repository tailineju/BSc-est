### Roiii, aqui tem muitos codigos que você vai usar em ME2
## Vou deixando isso aqui mais bonitinho com o tempo e atualizo no drive.
# Qualquer duvida me chama: Matheus Martinez - 61 998223233

#Métodos 2 

####Teste Binomial----
?pbinom()
pbinom(5, 15, 0.6) #Conferir esse 5

2*(pbinom(5,15,0.6))
#binom.test(numero sucesoss, total, prob esperada)
binom.test(5,15,0.6)

#Exemplo 1 conover 
vec <- c(189,176,202,233,231,193,195,185,174,212,160,199,213,166,278)
summary(vec)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#160.0   180.5   195.0   200.4   212.5   278.0

pbinom(7, 15,0.75) #TOP!!!!!! <- melhor fazer pela tabela
    #7 
#Esse 7 é a coluna "y" da tabela. 

binom.test(7,15,0.75) #teste binomial saiu certo

####Lista 2 -----

###Questão 1---- 
#Letra A - Teste Binomial

#P-valor 
pbinom(5,15,0.6) # p valor certo (Unilateral)

#Letra B -
pbinom(5,15,0.6)

#Questão 2------
##Letra A ----
X <- c(142,134,98,119,131,103,154,122,93,137,86,119,161,144,158,165,81,117,128,103)
ordered(X)
media <- mean(X)
vari <- var(X)
sd <- sqrt(vari)
inf <- media - qt(0.95,19)*sd/sqrt(20) #o alpha nessa questão é 10. nós difidimos metade pro LI e metade pro LS (Métodos 1) <- 1-alpha/2
sup <- media + qt(0.95,19)*sd/sqrt(20) 

# Letra B - IC para mediana ----
# Olha no caderno

#Letra C: r* e s* (Aprox Normal) -------
pi = 0.5
alpha = 0.05
n = 20

r = 20*0.5 + qnorm(0.05/2)*sqrt(20*0.5*(1-0.5))
r
s = n*pi + qnorm(1-alpha/2)*sqrt(n*pi*(1-pi)) #14.38 

summary(X)
125-r
125+s


#e) -------
#caderno


####Questão 3 ------
pesos = c(8.3,9.5,9.6,8.75,8.4,9.1,9.25,9.80,10.05,8.15,10,9.6,9.8,9.2,9.3)
summary(pesos)

-qnorm(.95)

um = -qnorm(.95)*sqrt(pi*(1-pi)/15)
um +0.5 #já usei isso em métodos

#p chapeu phat: literalmente observamos
pesos = ordered(pesos)
pesos



####Questão 4 ----
altura <- c(184.4,177.8,181.1,179.1,179.1,179.8,193,198.1,184.2,180.6,179.3,182.6,184.9)


qnorm(.95)

qnorm(0.05)

#Pro IC: (alpha = 0.05)
#r
qnorm(0.025) #Z (alpha/2)
#ou
alpha = 0.05
qnorm(alpha/2)

#s
qnorm(1-alpha/2)

#####r*----
pi = 0.95
alpha = 0.05
n = 100
r = n*pi + qnorm(alpha/2)*sqrt(n*pi*(1-pi))
r

#####s*-----
s = n*pi + qnorm(1-alpha/2)*sqrt(n*pi*(1-pi))
s


#####Teste Qui quadrado de aderência-----

#Região Crítica alpha= 0.05 e gl = 3
qchisq(.95,3) 

#Pelo p- valr
pchisq(0.563, 3)

###Capítulo 14 - Problema 5 - 
qchisq(.95,5)

#Capítulo 3 - Exercício 16: Qual dist. os dados seguem?
a <- c(2,10,18,50,70,30,18,2)
mean(a)
sum(a)
shapiro.test(a)

X <- c(279.33, 260.27, 246.15 ,252.56, 235.99, 251.53 ,277.15, 263.98, 234.97, 257.02 ,260.80, 272.45, 250.34 ,271.34 ,227.99, 272.16 ,286.29 ,211.62 ,241.52 ,249.09)
a<- ordered(X)
a
View(a)

#### ...TESTES DE NORMALIDADE ----
###Kolmogorov Lilliefors....-----

#Esses testes podem ser encontrados no pacote
library(goftest)
??goftests
#Kolmogorov
#Lilliefors
#Shapiro Wilk
#Anderson Darlin

##Shapiro-Wilk ----
library(goftest)
library(nortest) #ad.test e lillie.test

shapiro.test(amostras100$NOTA_LP) 
lillie.test(amostras100$NOTA_LP)
ad.test(amostras100$NOTA_LP)

#Exemplo 
X <- c(279.33, 260.27, 246.15 ,252.56, 235.99, 251.53 ,277.15, 263.98, 234.97, 257.02 ,260.80, 272.45, 250.34 ,271.34 ,227.99, 272.16 ,286.29 ,211.62 ,241.52 ,249.09)
shapiro.test(X) #Igual no calculos <- p-valor alto <- aceita h0
#Note que o valor da correlação com a normal é forte
#Aceita H0 - X segue dist. normal 

lillie.test(X)

#Anderson- Darling-----
ad.test(X)


####Usando formulas da prof -----
library(EnvStats)


#Kormogorov                           #normal
#Tem que usar parâmetros!
gofTest(X, test = "ks", distribution = "norm", param.list = list(mean=250, sd=20))

'''
ks = 0,185544 <- Que nem fizemos na aula 
True cdf does not equal the\n <- não é igual a normal (rejeita h0)** conferir
'''

#Lillie Test (kormogorov sem parametros <- a formula estima)
gofTest(X, test = "lillie", distribuition = "norm") # p- valor alto <- aceita normalidade

#Shapiro Wilk
gofTest(X, test = "sw")
# p-valor: 0.9138121 <- alto <- é normal! 

#Shapiro - Francis
gofTest(X, test = "sf")
# p-valor: 0.8553913 <- alto <- é normal!

#Anderson Darling 
gofTest(X, test = "ad") #p-valor 0.9275552 <- alto <- é normal! 


### T de Student para duas pop normais ------
###Exemplo aula
#Primeiro temos que testar a igualdade das var

w = 10^2/15^2 #razão das variâncias
w # 0.4444444
# alpha = 0.05 
qf(0.025,14,11) 

qf(.975,14,11)
#Aceita H0

##Agora sim, teste t de student para variancias desc. e iguais

pt(-0.8295,25) #pt(valr observado, graus de liberdade)

'''
t.test(x,y,...)
'''

####Testes de Comparação Não parametricos e Independentes ------

#Independentes e Não-Paramétricas

####Mann Witney -----
?wilcox.test
wilcox.test(banco$NOTA_LP,banco$NOTA_MT)

a <- c(15,18,12,11,14,15)

b <- c(11,11,12,16,12,13,8,10,13)

t.test(a,b)
      #Porém n pode esquecer do:
t.test(a,b, var.equal = T, conf.level = .99)
            #Var iguais?    #NC


wilcox.test(a,b, conf.level = 0.99) #amostra pequena com mts empates <- usa aproximação, usa correção de continuidade
                        #Não trabalha com o que vimos na aula

####Teste de Kolmogorov p/ 2 amostras -----

ks.test(a,b) #0.55556 #p valor alto -> aceita h0

#Unilateral (conferir no help)
ks.test(a,b, alternativa = "less") #o alha agr é 0.4 de 1 dos lados
ks.test(a,b, alternativa = "upper")

#Cuidado com NA's!!! USE NA.RM = T


###Cramer von Mises p/ 2 amostras -----
#Não está implementado no R


####Exercício aula! -----
#Será que a NOTA_MT difere pra homens e mulheres? 2 grupos (homens X mulheres)
library(readr)
library(tidyverse)
setwd("C:/Users/mathe/OneDrive/Documentos")
dados  <- read_csv("amostra_190093471.csv")
#Grupo 1
homes <- dados %>%
  filter(SEXO == "A")
#Grupo 2
mulheres <- dados %>%
  filter(SEXO == "B")

#Fazendo testes (alpha = 0.95)
t.test(homes$NOTA_LP, homes$NOTA_LP) 
wilcox.test(homes$NOTA_LP, mulheres$NOTA_LP) 
ks.test(homes$NOTA_LP, mulheres$NOTA_LP) 

#Agora vamos pra uma amostra tamanho 30

#Grupo 1

amostra <- dados %>%
  sample_n(size = 30)%>%
  select(SEXO, NOTA_LP)

homes <- dados %>%
  filter(SEXO == "A")

mulheres <- dados %>%
  filter(SEXO == "B")

t.test(homes$NOTA_LP, homes$NOTA_LP) 
wilcox.test(homes$NOTA_LP, mulheres$NOTA_LP) 
ks.test(homes$NOTA_LP, mulheres$NOTA_LP) 
#Professora foi testando com amostras maiores pq fizeram o codigo de amostra errado...

#Amostra 500
dados  <- read_csv("amostra_190093471.csv")

amostra <- dados %>%
  sample_n(size = 500)%>%
  select(SEXO, NOTA_LP)

homes <- dados %>%
  filter(SEXO == "A")

mulheres <- dados %>%
  filter(SEXO == "B")
t.test(homes$NOTA_LP, homes$NOTA_LP) # p valor caiu (ta dando errado)
wilcox.test(homes$NOTA_LP, mulheres$NOTA_LP) 
ks.test(homes$NOTA_LP, mulheres$NOTA_LP) 

###obs----
#Amostras grandes <- tendo a rejeitar (+detalhado)
#Amostras pequenas <- tendo a aceitar (quando rejeita é pq ta mt diferente mesmo)


###
library(ggplot2)
#O B está mais deslocado para cima. 
#Será que as meninas se saem melhor em portugues que os menonos? <- Fazemos um teste de Hipóteses. 
ggplot(dados, aes(x= SEXO, y=`NOTA_LP`)) +
  geom_boxplot(fill=c("#25b19c"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Sexo", y="Nota_LP") +
  theme_bw() 
#São normais, como vimos no trab 3.2
#Então usamos o TESTE T
#Grupo 1

homes <- dados %>%
  filter(SEXO == "A")
#Grupo 2
mulheres <- dados %>%
  filter(SEXO == "B")

#Fazendo testes (alpha = 0.95)

t.test(homes$NOTA_LP, homes$NOTA_LP, conf.level = 0.95) #acho que deu errado


####Para amostras correlacionadas ------
#T de Student -----
#Normal e Correlacionada

?t.test 

'''
t.test(x,y, paired = T) #com paired = T (PAREADAS)
'''


qt(.05,5)

pt(-1.2753,5)

####Teste dos postos -----
#Teste top ( da pra fazer o código
#Não normal e correlacionada

##### Teste de postos de wilcox ----
sem <- c(23,35,29,33,43,32)
com <-c(28,38,29,37,42,30)

wilcox.test(sem, com, paired = T) #Conferir valores :s
#se paired = F <- amostras independentes (mann witney) 



###Várias pop: ANOVA um fator----

#Quantil de F

qf(.95, 2,77) #2 e 77 graus de liberdade
       #categorias - 1 e n - categorias. 
#ou 
1-pf(487.1,2,77) #PORQUE QUERO PROB A DIREITA E ELE CALCULA ACUMULADA (A ESQUERDA)
#Rejeita H0
            
#Função no R

#?aov

aov_res <- aov(y ~ fator)
summary (aov_res)

anova <- aov(data$NOTA_LP ~ data$RACA_COR)
summary(anova)

####Comparações multiplas de médias (ANOVA)-----
# Realizar após ANOVA rejeitar H0. Questão: Como sãs as diferenças de médias?

#pairwise.t.test(y,fator, p.adjust.method = "bonferroni")

pairwise.t.test(banco$NOTA_LP,banco$RACA_COR, p.adjust.method = "bonferroni") # Como 


####Comparação de Variâncias -----
#Utilizar antes da ANOVA

###Barlett (Dados/notas normais) -----
bartlett.test(banco$NOTA_LP ~ banco$RACA_COR) #Variâncias Iguais! 

###Levene (Não normais)-----
library(DescTools)

##Para média
#LeveneTest ( y, fator, center=mean) – para o teste de Levene

LeveneTest(banco$NOTA_LP, banco$RACA_COR, center = mean) #Variâncias Iguais! 

##Para mediada (Variante de Brown)
#LeveneTest ( y, fator, center=median) – para o teste de Levene na variante de Brown

LeveneTest(banco$NOTA_LP, banco$RACA_COR, center = median) #Variâncias Iguais! 

### Kruskal-Wallis (Anova Não Paramétrica) -----
kruskal.test(banco$NOTA_LP,banco$RACA_COR) #medias diferentes


#Rejeitou H0?
####Comparações multiplas de médias (Kruskall-Wallis)-----

##Conover 
#ConoverTest(y, fator, method = "none")
#pairwise.wilcox.test (y, fator, p.adjust.method = .....)
#DunnTest (y, fator, method = .......)
banco$RACA_COR <- as.factor(banco$RACA_COR)

ConoverTest(banco$NOTA_LP, banco$RACA_COR, method = "bonferroni")
pairwise.wilcox.test(banco$NOTA_LP, banco$RACA_COR, p.adjust.method = "bonferroni") #utilizando mann whitney
DunnTest(banco$NOTA_LP, banco$RACA_COR, p.adjust.method = "bonferroni") 


### Comparação de várias pop correlacionadas ------
# Teste de Friedmann ------

?friedman.test()
#friedman.test(y, fator, blocos)


# Teste de Quade ------
#Para 5 ou menos categorias 

?quade.test 
