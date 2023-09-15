if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

## 1.1 hipoteses


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

N <- a*n

ybarra <- sum(obs)/N
sst <- sum((obs-ybarra)^2)
yidot <- tapply(df$obs,df$trat,mean)
sstrat <- n*sum((yidot - ybarra)^2)
ssres <- sst - sstrat
sst

qmtrat <- sstrat/(a-1)
qmres <- ssres/(N-a)

#pvalor da estatistica F
pvalor <- pf(qmtrat/qmres,a-1,N-a,lower.tail = FALSE)

#tab anova
tab <- data.frame(1:2)
tab$sq <- c(sstrat, ssres)
tab$gl <- c(a-1,N-a)
tab$qm <- c(qmtrat, qmres)
tab$f <- c(qmtrat/qmres,".")
tab$pvalor <- pc(pvalor,".")
tab

#conferindo
tabcomand <- aov(df$obs~df$trat)
summary(tabcomand)

#residuo
yidot20 <- rep(yidot,each=5)
#eij = yij - yij estimado
#yij estimado = mi + taoi = yidot
df$eij <- df$obs - yidot20

sum(df$eij) #==0

ggplot(df, aes(y = eij, x = obs)) +
  geom_point(size=5)+
  geom_hline(yintercept = 0, size=2)

# 1.3 pressupostos

boxplot(df$obs)
boxplot(df$trat)

#normalidade
shapiro.test(obs)

#variancia 
