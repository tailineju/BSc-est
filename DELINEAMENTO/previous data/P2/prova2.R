dados <- c(159,196,178,225,
           189,178,215,189,
           184,216,221,147,
           208,137,166,198)
tratamento <- c("T4","T1","T2","T3",
                "T1","T4","T3","T2",
                "T2","T3","T1","T4",
                "T3","T2","T4","T1")
colunas <- rep(c("V1","V2","V3","V4"),4)
linhas <- rep(c("E1","E2","E3","E4"),each=4)
df <- data.frame(dados,tratamento,colunas,linhas)

anova <- aov(dados~tratamento+linhas+colunas)
summary(anova)

# independencia
plot(anova$residuals)
plot(anova$residuals~anova$fitted.values)

# Normalidade
shapiro.test(anova$residuals)

# Homocedasticidade
pacman::p_load(car)
leveneTest(y=dados,group=tratamento)
bartlett.test(dados~tratamento)

leveneTest(y=dados,group=linhas)
leveneTest(y=dados,group=colunas)

# Aditividade
fit <- lm(dados~tratamento+linhas+colunas)
adj <- predict(fit)^2
model <- lm(dados~tratamento+linhas+colunas+adj)

anova2 <- anova(fit,model)
anova2

TukeyHSD(anova)
boxplot(dados~tratamento)

# ---------------------------------------------------------------------------- #

dados <- c(53.3,52.1,54,55.2,56.8,57.3,
           54.5,55.4,57.3,59.9,59,60.3,
           59.1,61.7,60.2,62.2,61.3,63.7)
potassio <- rep(c("Baixa","Baixa","Média","Média","Alta","Alta"),3)
nitrogenio <- rep(c("Baixa","Média","Alta"),each=6)
df <- data.frame(dados,potassio,nitrogenio)

anova <- aov(dados~potassio*nitrogenio)
summary(anova)

# independencia
plot(anova$residuals)
plot(anova$residuals~anova$fitted.values)

# Normalidade
shapiro.test(anova$residuals)

# Homocedasticidade
pacman::p_load(car)
leveneTest(y=dados,group=potassio)
leveneTest(y=dados,group=nitrogenio)

leveneTest(y=dados,group=colunas)

# Comparação
TukeyHSD(anova)
plot(TukeyHSD(anova))
boxplot(dados~potassio)
boxplot(dados~nitrogenio)
boxplot(dados~potassio*nitrogenio)
