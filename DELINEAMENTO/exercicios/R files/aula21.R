if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, car, dae)

# analise fatorial 
# +2 fatores

obs <- c(27,30,31,26,30,34,
        26,28,32,27,36,36,
        28,26,29,28,35,36,
        31,34,33,32,34,29,
        30,28,32,30,33,30,
        28,30,32,33,34,31,
        28,34,26,26,36,28,
        26,35,27,29,36,26,
        27,34,28,28,34,26)

ciclo <- as.factor(rep(c(40,50,60), each=18))

oper <- as.factor(rep(c(1:3),times=18))

temp <- as.factor(rep(c(300,350), each=3, times=9))

df <- data.frame(obs,ciclo,oper,temp)

# exploratoria
boxplot(obs~ciclo)
boxplot(obs~oper)
boxplot(obs~temp)
boxplot(obs~ciclo*oper*temp)


#1.1) hipoteses 

#h0: Os fatores não apresentam efeitos no tecido
#h1: Os fatores apresentam efeitos no tecido

#modelo analise fatorial
#h0: tao1 = ... = taoA = 0
#h1: taoi != 0

#h0: beta1 = ... = betaA = 0
#h1: betai != 0

#h0: teta1 = ... = tetaA = 0
#h1: tetai != 0

#h0: tao*beta1 = ... = tao*betaA = 0
#h1: tao*betai != 0

#h0: tao*teta1 = ... = tao*tetaA = 0
#h1: tao*tetai != 0

#h0: teta*beta1 = ... = teta*betaA = 0
#h1: teta*betai != 0

#h0: tao*beta1*teta = ... = tao*betaA*teta = 0
#h1: tao*betai*teta != 0


#1.2) análise

anova <- aov(obs~ciclo*oper*temp)
summary(anova)

# Conclusões
# Com alpha=5%, todas as H0 do modelo são rejeitadas,
# indicando que existem diferenças.

#1.3) pressupostos

# normalidade
qqnorm(anova$residuals)
qqline(anova$residuals)
shapiro.test(anova$residuals)

# independencia 
plot(anova$residuals)
plot(anova$residuals/summary(anova)[[1]][4,3])

# homocedasticidade
plot(anova$residuals, anova$fitted.values)
leveneTest(obs~ciclo)
leveneTest(obs~oper)
leveneTest(obs~temp) #h0 rejeitada

#1.4) H0 rejeitada e aí
# Como a pontuação média varia de acordo com os fatores?

TukeyHSD(anova)
interaction.ABC.plot(obs,ciclo, oper, temp, data=df)


