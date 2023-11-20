library("pacman")
pacman::p_load(tidyverse,car,dae)

obs <- c(158,197,176,224,
         189,187,211,190,
         184,210,211,147,
         209,163,165,198)

trat <- as.factor(
    c(4,1,2,3,
    1,4,3,2,
    2,3,1,4,
    3,2,4,1))

est <- as.factor(rep(c(1:4),each=4))

vac <- as.factor(rep(c(1:4),times=4))

df <- data.frame(obs,trat,est,vac)

# expl
boxplot(obs~trat)
boxplot(obs~est)
boxplot(obs~vac)

# anova
anova <- aov(obs~trat+est+vac)
summary(anova)

sum(summary(anova)[[1]][,2])
sum(summary(anova)[[1]][,3])

#normalidade
qqnorm(anova$residuals)
qqline(anova$residuals)

shapiro.test(anova$residuals)

#indep
plot(anova$residuals)

#var iguais
leveneTest(obs~trat)
leveneTest(obs~est)
leveneTest(obs~vac)

bartlett.test(obs,trat)
bartlett.test(obs,est)
bartlett.test(obs,vac)

#adit
mod1<- lm(obs~trat+est+vac)
adv<- predict(mod1)^2
mod2<- lm(obs~trat+est+vac|adv)
anova(mod1,mod2)

#resi
df$res <- anova$residuals

#comp medias
TukeyHSD(anova, which = 'trat')


### -------------------------------------------------

obs <- c(53.3,52.1,54,55.2,56.8,57.3,
         54.5,55.4,57.3,59.9,59,60.3,
         59.1,61.7,60.2,62.2,61.3,63.7,
         57.1,60.5,59.8,58.1,62.5,61.8)

temp <- as.factor(
    rep(c(250,260,270),each=2,times=4)
)

pres <- as.factor(
    rep(c(120,130,140,150),each=6)
)

df <- data.frame(obs,temp,pres)
df
#exp

boxplot(obs~temp*pres)
boxplot(obs~temp)
boxplot(obs~pres)

# anova
anova<- aov(obs~temp*pres)
summary(anova)

sum(summary(anova)[[1]][,2])
sum(summary(anova)[[1]][,3])


#normalidade
qqnorm(anova$residuals)
qqline(anova$residuals)

shapiro.test(anova$residuals)

#indep
plot(anova$residuals)

#var iguais
leveneTest(obs~temp)
leveneTest(obs~pres)

bartlett.test(obs,temp)
bartlett.test(obs,pres)

TukeyHSD(anova)
interaction.plot(temp,pres,obs)
interaction.plot(pres,temp,obs)

df2 <- df %>% filter(temp == "270")

anova2 <- aov(df2$obs~df2$pres)
summary(anova2)

TukeyHSD(anova2)

boxplot(df2$obs~df2$pres)
