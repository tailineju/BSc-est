if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,car)

## 1.1 hipoteses
#h0 nao existem diferencas entre os tratamentos (potencias)
#h1 existem diferencas entre os trats

## 1.2 estatistica do teste


obs <- c(575, 542, 530, 539, 570,
        565, 593, 590, 579, 610,
        600, 651, 610, 637, 629,
        725, 700, 715, 685, 710)

pot<-c(160,180,200,220)
trat <- as.factor(rep(pot, each = 5))

df <- data.frame(obs,trat)

a<- length(pot)
n<- 5 #repeticoes

N <- a*n #ou length(obs)

ybarra <- sum(obs)/N
sst <- sum((obs-ybarra)^2)
yidot <- tapply(obs,trat,mean)
sstrat <- n*sum((yidot - ybarra)^2)
ssres <- sst - sstrat 
#ou sum(obs-rep(yidot,each=n))^2

qmtrat <- sstrat/(a-1)
qmres <- ssres/(N-a)
qmestab <-

#pvalor da estatistica F
pvalor <- pf(qmtrat/qmres,a-1,N-a,lower.tail = FALSE)
#regiao critica de rejeiao para F
rc <- qf(0.05,a-1,N-a,lower.tail = FALSE)

#tab anova
tab <- data.frame(1:2)
tab$sq <- c(sstrat, ssres)
tab$gl <- c(a-1,N-a)
tab$qm <- c(qmtrat, qmres)
tab$f <- c(qmtrat/qmres,".")
tab$pvalor <- pc(pvalor,".")
tab

#conferindo
anova <- aov(df$obs~df$trat)
summary(anova)

anova$residuals
df$eij

#residuo
yidot20 <- rep(yidot,each=5) #valores ajuestados
#eij = yij - yij estimado
#yij estimado = mi + taoi = yidot
df$eij <- df$obs - yidot20

sum(df$eij) #==0

#residuos padronizados
df$eijpad <- df$eij/sqrt(qmres)


# 1.3 pressupostos

boxplot(obs~trat)
qqline(df$eijpad)

#independencia
ggplot(df, aes(y = eij, x = obs)) +
  geom_point(size=5)+
  geom_hline(yintercept = 0, size=2)

#normalidade
hist(df$eijpad)
shapiro.test(df$eijpad) #nao rejeita h0

#homocedasticidade
plot(df$eij,anova$fitted.values)

## teste f
vartrat <- tapply(obs, trat, var)
testef <- max(vartrat)/min(vartrat)
pvalorf <- 2*pf(testef,n-1,n-1,lower.tail = FALSE)

## teste bartlett
bartlett.test(obs~trat)

## teste levene
medtrat <- tapply(obs, trat, median)
desvioy <- abs(obs-rep(medtrat,each=n))
summary(aov(desvioy~trat))

leveneTest(obs~trat)


# 1.4 conclusoes 
#pressupostos foram atendidos, dando qualidade ao modelo. 
#Tem-se evidencias para rejeitar h0 e concluir que existem diferencas entre os tratamentos


# 1.6 comparacao de medias
# teste de tukey
#h0: mi=mj
#h1: mi != mj


