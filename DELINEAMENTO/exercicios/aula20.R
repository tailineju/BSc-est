if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, car)


# qual o estudo?
# verificar somente um efeito (a)
# controlar outros efeitos ( b e c) 

obs <- c(83,77,80,83,85,
         80,85,85,81,79,
         82,97,76,84,76,
         81,93,81,91,83,
         74,87,89,88,72)

a <- as.factor(c(3,1,4,5,2,
                 1,5,2,4,3,
                 2,3,5,1,4,
                 4,2,1,3,5,
                 5,4,3,2,1))

b <- as.factor(rep(c(1:5),times=5))
c <- as.factor(rep(c(1:5),each=5))

df <- data.frame(obs,a,b,c)

# exploratoria
boxplot(obs~a)
boxplot(obs~b)
boxplot(obs~c)

# hipoteses
#h0: tao1 = ... = taoA = 0
#h1: taoi != 0

# anova
anova<- aov(obs ~ a + b + c)
summary(anova)

# conclusoes
# Livros tem diferença entre si, musica tambem

# pressupostos

# normalidade
qqnorm(anova$residuals)
qqline(anova$residuals)
shapiro.test(anova$residuals)

#homocedasticidade
plot(x=anova$residuals,y=anova$fitted.values)
leveneTest(obs~a)
leveneTest(obs~b)
leveneTest(obs~c)

#independencia de residuos
plot(anova$residuals)
plot(anova$residuals/summary(anova)[[1]][4,3]) #residuos padronizados

#aditividade ???
modlct <- lm(obs~a+b+c)
adlct <- (predict(modlct))^2
modalct <-lm(obs~a+b+c|adlct)

anova(modlct,modalct) #teste de vero #h0 Beta ad = 0 #h1= Beta ad !=0

# estim parametros
#mi
mi <- mean(obs)

#alpha
yi_ <- tapply(obs,c,mean)
yi_v <- rep(yi_, each=5)
alpha <- yi_v - mean(obs)

#taoj
yj_ <- tapply(obs,a,mean)
yj_v <- rep(yj_, each=5)
taoj <- yj_v - mean(obs)

#betak
yk_ <- tapply(obs,b,mean)
yk_v <- rep(yk_, each=5)
betak <- yk_v - mean(obs)

mi
alpha
taoj
betak

# comp de medias
TukeyHSD(anova, which='a')
