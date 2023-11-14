if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, car)

install.packages('tinytex')
tinytex::install_tinytex()
# to uninstall TinyTeX, run tinytex::uninstall_tinytex() 

# writeLines(c(
#   '\\documentclass{article}',
#   '\\begin{document}', 'Hello world!', '\\end{document}'
# ), 'test.tex')
tinytex::pdflatex('test.tex')


# analise fatorial
# 2 fatores

obs <- c(130,155,34,40,20,70,
         74,180,80,75,82,58,
         150,188,136,122,25,70,
         159,126,106,115,58,45,
         138,110,174,120,96,104,
         168,160,150,139,82,60)

temp <- as.factor(rep(c(15,15,70,70,125,125),times=6))

material <- as.factor(rep(c(1:3), each=12))

df <- data.frame(obs,temp,material)

#0 exploratoria
boxplot(obs~material)
boxplot(obs~temp)
boxplot(obs~material*temp)

#0 hipoteses modelo analise materialial
#h0: tao1 = ... = taoA = 0
#h1: taoi != 0

#h0: beta1 = ... = betaA = 0
#h1: betai != 0

#h0: tao*beta1 = ... = tao*betaA = 0
#h1: tao*betai != 0

#1 anova
anova <- aov(obs~temp*material)
summary(anova)

#2 pressupostos
# normalidade
qqnorm(anova$residuals)
qqline(anova$residuals)
shapiro.test(anova$residuals)

#homocedasticidade
plot(x=anova$residuals,y=anova$fitted.values)
leveneTest(obs~temp)
leveneTest(obs~material)

#independencia de residuos
plot(anova$residuals)
plot(anova$residuals/summary(anova)[[1]][4,3]) #residuos padronizados

#3 comparacao de medias
TukeyHSD(anova)

#4 responder perguntas do pesquisador

interaction.plot(material,temp,obs)
interaction.plot(temp,material,obs)

#1. Quais efeitos o tipo de material e a 
# temperatura têm na vida útil da bateria?

#Quanto maior a temperatura, menor a vida útil
#Temp 15 apresenta a melhor vida útil independente do material

#Material 1 diferencia do 2 e do 3 e apresenta menor temperatura


#2. Existe uma escolha de material que proporcione 
# uma vida uniformemente longa, independentemente da temperatura?

# sim, o material 3