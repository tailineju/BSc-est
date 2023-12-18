---
title: 'p3'
author: tailine
output:html
---
pacman::p_load(tidyverse,car,easyanova,asbio)

obs<-c(107,117,122,111,90,116,
        89,101,98,101,95,90,
        116,136,130,122,117,114,
        101,110,104,91,100,94,
        90,112,99,105,110,114,
        96,89,92,78,90,93)
fatA<-factor(rep(c('A','B','C'),each=12))
fatB<-factor(rep(c('M','P'),each=6,times=3))
bloco<-factor(rep(c(1:6),times=6))

df<-data.frame(fatA,bloco,fatB,obs)

anova<-aov(obs~fatA*fatB+Error(bloco/fatA))
summary(anova)

res<-aov(obs~fatA*fatB+bloco:fatA)
summary(res)

anova2<- ea2(df,design = 5)
ea2
