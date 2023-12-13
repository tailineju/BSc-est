# exercício aula 26
# EXPERIMENTO EM PARCELAS SUBDIVIDIDAS (SPLIT-PLOT)----
# ração = variedade
# Mineral = semente

obs <- c(42.9,41.6,28.9,30.8,
         53.8,58.5,43.9,46.3,
         49.5,53.8,40.7,39.4,
         44.4,41.8,28.3,34.7,
         53.3,69.6,45.4,35.1,
         57.6,69.6,42.4,51.9,
         59.8,65.8,41.4,45.4,
         64.1,57.4,44.1,51.6,
         62.3,58.5,44.6,50.3,
         63.4,50.4,45.0,46.7,
         64.5,46.1,62.6,50.3,
         63.6,56.1,52.7,51.8,
         75.4,65.6,54.0,52.7,
         70.3,67.3,57.6,58.5,
         68.8,65.3,45.6,51.0,
         71.6,69.4,56.6,47.4)

fatA <- factor(c(rep(c("A1","A2","A3","A4"),each=16)))
fatB <- factor(rep(rep(c("B1","B2","B3","B4"), each=4),4))
bloco <- factor(c(rep(c("1","2","3","4"),16)))
fatA
fatB

length(obs)
length(fatA)
length(fatB)
length(bloco)

data.frame(obs,bloco,fatA,fatB)


# Modelo----
anovas <- aov(obs~fatA*fatB+ Error(bloco/fatA))
summary(anovas)
names(anovas) # ñ consegue pegar os resíduos

# pressupostos----
# fazer outro modelo para poder selecionar residuos
mod1 <- aov(obs~ fatA*fatB + bloco:fatA)
summary(mod1)
QMres <- summary(mod1)[[1]][5,3]
QMres

residuo <- mod1$ residuals
resid_pad <- residuo/sqrt(QMres)

# normalidade----
qqnorm(residuo)
qqline(residuo)

plot(mod1,2)

shapiro.test(residuo)

# independência ----
plot(residuo)
plot(resid_pad)

# homogeneidade----
plot(mod1,1)

require(car)
leveneTest(obs ~ bloco)
leveneTest(obs ~ fatA)
leveneTest(obs ~ fatB)
leveneTest(obs ~ c(fatA:fatB))


bartlett.test(obs ~ bloco)
bartlett.test(obs ~ fatA)
bartlett.test(obs ~ fatB)
bartlett.test(obs ~ c(fatA:fatB))

# comparação de médias----

interaction.plot(fatA,fatB,obs)

require(easyanova)

compara = easyanova::ea2(data.frame(fatA,bloco,fatB,obs),design = 5)
compara
