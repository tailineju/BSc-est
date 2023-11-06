library(tidyverse)
library(car)

obs1<- c(5,14,7,16,
        4,9,8,12,
        4,10,5,11,
        6,12,9,14,
        3,13,10,17)

obs <- c(5,4,4,6,3,
        14,9,10,12,13,
        7,8,5,9,10,
        16,12,11,14,17)

esp<- as.factor(c('a','b','c','d'))
espv<- rep(esp, each=5)

anova<-aov(obs~espv)
summary(anova)

pvalor<- 1-pf(22.6,3,16)

#pressupostos

#normalidade
shapiro.test(anova$residuals)
#homocedasticidade
bartlett.test(obs~espv)
leveneTest(obs~espv)
#independencia dos res
ggplot(mapping=aes(x=anova$residuals,y=obs)) +
    geom_point(size=5)



df<- data.frame(espv,obs)
yidot<- tapply(obs,espv,mean)
yitdot<- rep(yidot,each=5)

df$residuals<- ((df$obs-yitdot)^2)/3.95


#ex 2

reg<- as.factor(c('N','NE','CO','SE','S'))
regv<- rep(reg,each=5)
espc<- as.factor(1,2,3,4)
espcv<- rep(espc,length.out=20)
obs2<- c(105.17,88.42,100.78,102.09,
        102.21,89.36,99.26,99.45,
        99.43,90.16,96.77,102.63,
        107.74,92.3,102.5,107.63,
        106.2,91.5,104.1,105.9)

#c(105.17,102.21,99.43,107.74,106.2,
      #  88.42,89.36,90.16,92.3,91.5,
       # 100.78,99.26,96.77,102.50,104.1,
       # 102.09,99.45,102.63,107.63,105.9)

df2 <- data.frame(espcv,regv,obs2)
anova2<-aov(obs2~regv+espcv)

summary(anova2)
TukeyHSD(anova2)

#pressupostos

#normalidade
shapiro.test(anova2$residuals)
#homocedasticidade
bartlett.test(obs2~espcv)
bartlett.test(obs2~regv)
leveneTest(obs2~espcv)
#independencia dos res
ggplot(mapping=aes(x=anova2$residuals,y=obs2)) +
    geom_point(size=5)

yidot2<- tapply(obs2,espcv,mean)
