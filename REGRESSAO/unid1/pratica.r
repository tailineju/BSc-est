if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

massa <- c(62, 62.9, 36.1, 54.6, 48.5, 42, 47.4, 50.6, 42, 48.7, 40.3, 33.1, 51.9, 42.4, 34.5, 51.1, 41.2, 51.9, 46.9)
taxa <- c(1792, 1666, 995, 1425, 1396, 1418, 1362, 1502, 1256, 1614, 1189, 913, 1460, 1124, 1052, 1347,1204,1867,1439)

# aula pratica 01/09
df <- data.frame(massa,taxa)

summary(df$massa)
sd(df$massa)
cf_x <- sd(df$massa)/mean(df$massa)

summary(df$taxa)
mean(df$taxa)
sd(df$taxa)
cf_y <- sd(df$taxa)/mean(df$taxa)

boxplot(df$massa)
boxplot(df$taxa)

ggplot(df, aes(x=massa,y=taxa)) +
geom_point()
stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")


cor(df$massa,df$taxa)

#h0: p = 0
#h1: p != 0

cor.test(massa, taxa)

#h0: p = 0
#h1: p > 0

cor.test(massa, taxa, alternative="greater")


## #############################################
#aula pratica 15/09

df$xiyi<- massa*taxa
df$xi2<- massa^2
n<-nrow(df)
xbarra<- mean(massa)
ybarra<- mean(taxa)

beta1<- (sum(df$xiyi) - (n*xbarra*ybarra))/(sum(df$xi2)-(n*(xbarra)^2))
beta0<- ybarra - (beta1*xbarra)

df$estim <- beta0 + (beta1*df$massa) #media de taxa de metabolismo

df$residuo<- df$taxa - df$estim
df$residuo2 <- df$residuo^2

sum(df$residuo) #==0

sum(df$residuo2) #minimo (menor soma possivel)

sum(df$taxa) 
#==
sum(df$estim)

df$xiei <- df$massa * df$residuo

sum(df$xiei) #==0

df$yiei <- df$estim * df$residuo

sum(df$yiei) #==0

beta0 + (beta1*xbarra) #==ybarra

#estim variancia
sigma2 <- (sum(df$residuo2))/(n-2)

#denom variancias
denom <- sum(df$xi2) - (n*(xbarra^2))

s2beta0 <- sigma2 * (sum(df$xi2)/(n*denom))
s2beta1 <- sigma2/denom

sbeta1 <- sqrt(s2beta1)
sbeta0 <- sqrt(s2beta0)

cf.var.b1 <- sbeta1/beta1
cf.var.b0 <- sbeta0/beta0


covb1b0 <- -xbarra*s2beta1 #negativo= inversamente relacionado

cf.corr <- covb1b0/(sbeta1*sbeta0) #se 1 for pequeno a tendencia eh que o outro seja maior


################################### aula 1809
# exer. calc ic(0,95)
sup <- beta1 + qt(1-0.025,n-2)*sbeta1
inf <- beta1 - qt(1-0.025,n-2)*sbeta1

beta1
ic<- c(inf,sup)
ic

#teste de ausencia de regressao
#h0: beta1 =0
#h1: beta1 != 0
Tcalc <- beta1/sbeta1
qt(1-0.025,n-2) #regiao critica

#>< que rc, rejeita-se h0



SSTO <- sum(df$taxa^2)- n*(ybarra^2)


SSR <- beta1^2 *(sum(df$xi2) - n*(xbarra^2))

SSE<- SSTO-SSR


MSR <- SSR/1
MSE <- SSE/(n-2)

F<- MSR/MSE

var <- c('Reg','Erro')
Ftab<- c(F,".")
gl<- c(1, n-2)
MS <- c(MSR, MSE)
SS <-  c(SSR,SSE)   


tabelaReg <- data.frame(var,SS,gl,MS,Ftab)

#teste F

pvalor<- pf(F, 1, n-2, lower.tail = FALSE)
rc<- qf(0.05, 1, n-2, lower.tail = FALSE)
rc

#teste T

tcalc <- beta1/sqrt(sigma2/(sum(df$xi2)-n*(xbarra^2)))
tcalc^2 #== F


R2<-SSR/SSTO #quantos % da variacao na var Y eu consegui explicar?

sqrt(R2)


#aula 1810


#b= t de 1-alplha/4
alpha<-0.05
sup <- beta1 + qt(1-(alpha/4),n-2)*sbeta1
inf <- beta1 - qt(1-(alpha/4),n-2)*sbeta1

beta1
ic<- c(inf,sup)
ic

#problema - taxa de metabolismo para 30, 42 e 50kg
#media estimada
xh<-c(30,42,50)
yestim<- beta0+(beta1*xh)

denom<- sum(df$massa^2) - n*(xbarra^2)
num <- (xh-xbarra)^2

syestim<- sqrt(MSE) * sqrt((1/n) +(num/denom))
#syestim<- c(70.26,35.39,32.92) 

#working-hotelling
W2<-2*qf(1-alpha,2,n-2)
W<-sqrt(W2)
supW<- yestim + W*syestim
infW<- yestim - W*syestim
infW
supW

#bonferroni
g<- length(xh)
B<-qt(1-(alpha/(2*g)),n-2)
supB<- yestim + B*syestim
infB<- yestim - B*syestim
infB
supB


#novas observacoes
#scheffe
Sch2 <- g*qf(1-alpha,g,n-2)
Sch<-sqrt(Sch2)
spred <- sqrt(MSE) * sqrt(1 + (1/n) +(num/denom))
supSch<- yestim + Sch*spred
infSch<- yestim - Sch*spred
infSch
supSch

#bonferroni
supBn<- yestim + B*spred
infBn<- yestim - B*spred
infBn
supBn

#regiao de confianca
#working-hotelling
infW
supW