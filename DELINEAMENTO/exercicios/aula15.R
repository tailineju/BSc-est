if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, car)

#quadrados latinos cana de açucar
# 27/10

#por linha
obs <- c(432,518,458,583,331,
         724,478,524,550,400,
         489,384,556,297,420,
         494,500,313,486,501,
         515,660,438,394,318)

linha <- as.factor(c(1:5))
linhas <- rep(linha, each=5)

col <- as.factor(c(1:5))
cols <- rep(col, times=5)

trat <- c('D','A','B','C','E',
          'C','E','A','B','D',
          'E','B','C','D','A',
          'B','D','E','A','C',
          'A','C','D','E','B')

df <- data.frame(obs, trat, linhas, cols)

##### modelo - quadrado latino

##### hipoteses
#h0: tao1 = ... = taoA = 0
#h1: taoi != 0

##### anova
anova<- aov(obs ~ cols + linhas + trat)
summary(anova)

#####conclusoes 
#alpha = 5%
# existe diferença entre os trat e entre as colunas, 
# não há evidências para dizer que há diferenças nas linhas

#### pressupostos
#normalidade
boxplot(obs~trat)
boxplot(obs~linhas)
boxplot(obs~cols)
eijpad <- anova$residuals/summary(anova)[[1]][4,3] #meansq
shapiro.test(eijpad)

#homocedasticidade
leveneTest(obs~trat)
leveneTest(obs~linhas)
leveneTest(obs~cols)

#independencia
ggplot(mapping=aes(x=anova$residuals,y=anova$fitted.values)) +
    geom_point(size=5)

#aditividade

#### estimação dos parametros

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

#teste de aditividade
#yijk = m +taoj + bk + ad + eijk
#ho: ad=0
#h1: ad!=0
modlct <- lm(obs~linhas+cols+trat)
adlct <- predict(modlct)^2
modalct <- lm(obs~linhas+cols+trat+adlct)
anova(modlct,modalct)


##### prop explicada
r2 <- summary(anova)[[1]][4,3]/sum(summary(anova)[[1]][,3])

##### comparação de médias
TukeyHSD(anova, which='trat')
