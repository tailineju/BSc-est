#1----
pessoas <- c("Camila", "Carlos", "Khezia", "Gabriela", "Andre")
idades <- c(28, 81, 22, 30, 36)
numeros <- 1:100
pares <- seq(2, 100, by = 2)
tres <- seq(3, 1000, by = 3)


#2----
set.seed(123)
randomico <- rpois(100,5)
randomico[randomico > 5]
sum(randomico == 5)

#3----
vec <- c(27,36,38,31,
         31,35,25,22,
         26,25,23,27,
         31,22,26,31,
         33,28,34,26,
         38,34,27,42)
mat <- matrix(vec, nrow = 4)
apply(mat, 1, mean)
apply(mat, 2, mean)
sum((mat[1,] - mean(mat[1,]))^2)/(length(mat[1,])-1)

varpop <- function(x){
  sum((x - mean(x))^2)/length(x)
}

#4----
set.seed(111)
matriz2 <- matrix(rpois(1000,100),nrow = 10)
apply(matriz2, 2, mean)
apply(matriz2, 1, mean)


#5----
#dado tabular que permite dados de classes diferentes em cada vetor componente
df <- data.frame(
  pessoas,
  peso = c(45, 70, 40, 50, 90),
  altura = c(1.5, 1.75, 1.45, 1.5, 1.85),
  sexo = factor(c("f", "m","f","f","m"))
)

alturas <- aggregate(altura ~ sexo, data = df, mean)

plot(alturas$sexo, alturas$altura, pch = 19, frame = FALSE,
     xlab = "sexo", ylab = "altura média")

plot(peso ~ altura, data = df, frame = FALSE,
     col = sexo, pch = 19)
abline(v = alturas$altura , col = c("blue", "red"))

#6----
rm(list = ls())
gc()

data(mtcars)
summary(as.factor(mtcars$cyl))

mpg <- aggregate(mpg ~ as.factor(cyl), data = mtcars, mean)

set.seed(577)
dados <- data.frame( round(rnorm(2000,30),2),rpois(2000,100))
names(dados) <- c("valores", "grupo")
head(dados)

aggregate(valores ~ as.factor(grupo), data = dados, mean)





# simulacao ----

variancia <- 25
n <- 50
alfa <- 0.05

rc <- (qnorm(0.05)*(-1)*sqrt(variancia/n))+100
#de rc ate +Inf
set.seed(577)
medias <- c()
for(i in 1:1000){medias <- c(medias, mean(rnorm(50,100,25)))}

#361 de 1000 acima da RC
sum(medias>rc)

#erro tipo 2:
# nao rejeitar H0 quando h0 é falsa

beta <- pnorm((rc-102)/sqrt(variancia/n))
#0.118

medias2 <- c()
set.seed(577)
for(i in 1:1000){medias2 <- c(medias2, mean(rnorm(50,102,25)))}
sum(medias<rc)
#636 amostras geradas com media 102 nao estao contidas na RC


#-------------------------#
# REPLICACAO USANDO TIDY
library(dplyr)

set.seed(577)
k <- tibble(
  grupo = rep(1:1000, each = 50),
  valores = unlist(replicate(1000,rnorm(50,100,25), simplify = F))
  ) %>%
  group_by(grupo) %>%
  summarise(media = mean(valores))%>%
  pull(media)
#--------------------#


# relacao entre anova e teste t ----
rm(list = ls())

popA <- c(107, 113, 100, 104, 116, 92, 97,
          87, 110,
          81, 95,
          94,
          92, 88, 88)
popB <- c(91, 123,
          98, 102,
          97, 97, 109, 103, 95, 102, 85, 113, 101,89, 88)

#igualdade de variancias: nao rejeita igualdade (H0)
var(popA)/var(popB)
qf(.025, 14, 14, lower.tail = TRUE)
qf(.025, 14, 14, lower.tail = FALSE)

var.test(popA, popB) #teste do R corrobora

#teste t: calculamos se mi_d é igual a zero, avaliando se a diferenca
#entre as médias é zero

#sabemos que as variancias sao iguais, mas nao sabemos qual é a variancia

s_comb <- (14*var(popA) + 14*var(popB))/(30-2)
rc <- qt(0.025, 28) #apenas limite inferior. Espelhar para LS

#estatística de teste
t <- (mean(popA)-mean(popB))/(s_comb * sqrt(2/15))
# que esta fora da RC entao nao se rejeita H0

#estatística de teste deu diferente. Porque?
t.test(x = popA, y = popB, var.equal = TRUE) 


# anova ----
a <- 2 #numero de grupos
n <- 15 #numero de indivíduos em cada grupo
media.geral <- mean(c(popA, popB))

sqtot <- sum((c(popA, popB) - media.geral)^2)
# ou
sum(c(popA, popB)^2)-(a*n*media.geral^2)

sqres <- sum((popA-mean(popA))^2, (popB-mean(popB))^2)
# ou
sum(c(popA, popB)^2)-n*sum(mean(popA)^2, mean(popB)^2)

sqtrat <- sqtot-sqres
# ou - essa da diferente
sum((mean(popA)-media.geral)^2,(mean(popB)-media.geral)^2)*n
# ou
n*sum(mean(popA)^2, mean(popB)^2) - a*n*media.geral^2

# teste de variancias anterios ja confirma igualdade de variancias.
# [sqtrat/(a-1)]/[sqres/(an-a)] tem ~ F(a-1, an-a)
stat.F <- (sqtrat/(a-1))/(sqres/(a*n-a))
pval <- pf(stat.F, df1 = a-1, df2 = a*n-a, lower.tail = FALSE)
# teste unilateral com p-valor 0.39

qmtrat <- sqtrat/(a-1)
qmres <- sqres/(a*n-a)

## tabela anova ----
tab.anova <- data.frame(
  fonte = c("trat", "res", "total"),
  gl = c(a-1, a*n-a, a*n-1),
  SQ = c(sqtrat, sqres, sqtot),
  QM = c(qmtrat, qmres, NA),
  test.stat.F = c(qmtrat/qmres, NA, NA),
  p.valor = c(pval, NA, NA)
)
tab.anova

## usando a função aov
tabela <- data.frame(
  grupo = rep(c('popA', 'popB'), each = 15),
  valores = c(popA, popB)
)

anova <- aov(valores ~ grupo, data = tabela)

#comparando resultados
summary(anova)
tab.anova
