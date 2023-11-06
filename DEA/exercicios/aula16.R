if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, car, asbio)

# spinneret
# 30/10

obs <- c(19.56,23.16,29.72,
         22.94,27.51,23.71,
         25.06,17.70,22.32,
         23.34,23.54,18.75,
         16.28,22.29,28.09,
         18.53,19.89,20.42,
         23.98,20.46,19.28,
         15.33,23.02,24.97,
         24.41,22.44,19.23,
         16.65,22.69,24.94,
         18.96,24.19,21.95,
         21.49,15.78,24.65)

trat <- as.factor(
        c('A','B','C',
          'B','C','A',
          'C','A','B',
          'B','C','A',
          'A','B','C',
          'C','A','B',
          'C','A','B',
          'A','B','C',
          'B','C','A',
          'A','B','C',
          'B','C','A',
          'C','A','B'))

linhas <- as.factor(rep(c(1:12),each=3))
cols <- as.factor(rep(c(1,2,3), times=12))

df<- data.frame(obs,trat,linhas,cols)

# exploratoria ---
boxplot(obs~trat)
boxplot(obs~linhas)
boxplot(obs~cols)


# modelo ---

# hipoteses ---
#h0: tao1 = ... = taoA = 0
#h1: taoi != 0

# analise de variancias ---
anova <- aov(obs~trat+linhas+cols)
summary(anova)

# pressupostos ---

#normalidade
qqnorm(anova$residuals)
qqline(anova$residuals)
shapiro.test(anova$residuals)

#homocedasticidade
plot(x=anova$residuals,y=anova$fitted.values)
leveneTest(obs~trat)
leveneTest(obs~linhas)
leveneTest(obs~cols)

#independencia de residuos
plot(anova$residuals)
plot(anova$residuals/summary(anova)[[1]][4,3])

# estimação dos parametros ---

#mi
mi <- mean(obs)

#alpha
yi_ <- tapply(obs,linhas,mean)
yi_v <- rep(yi_, each=5)
alpha <- yi_v - mean(obs)

#taoj
yj_ <- tapply(obs,trat,mean)
yj_v <- rep(yj_, each=5)
taoj <- yj_v - mean(obs)

#betak
yk_ <- tapply(obs,cols,mean)
yk_v <- rep(yk_, each=5)
betak <- yk_v - mean(obs)

mi
alpha
taoj
betak

# teste de aditividade ---


# proporção da variância explicada ---
r2 <- summary(anova)[[1]][4,3]/sum(summary(anova)[[1]][,3])
paste0(round(r2*100,2), '%')

# comparação de médias ---
TukeyHSD(anova, which='trat')
