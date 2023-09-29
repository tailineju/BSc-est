
# AULA 2 - R


#############################################################################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,readxl, psych,lavaan,semPlot,semTools,corrplot)


# Analise Fatorial Explorataria e Alfa de Cronbach
# Importando arquivo exemploAF.xlsx

dados=read_excel("psico/semuni/exemploAF.xlsx")

str(dados)

table(dados$X1)


# Correlacoes

# Escala autoeficacia

correlacoes=cor(dados, method="pearson")
correlacoes=cor(dados, method="spearman")
correlacoes=cor(dados, method="kendall") #ordenacao e var nao normal

# Grafico de correlacoes
cor.plot(correlacoes,numbers=TRUE,main="Autoeficacia")


# Identificacao do numero de fatores

# Extracaoo por componentes principais

fit<-fa(dados,fm="pa")

summary(fit)

fit$e.values

# Scree plot 1

plot(fit$e.values,type="o",  ylim = c(0, 6),
     xlab="Componente", ylab="Autovalor")


# Analise paralela

paralela=fa.parallel(dados,nfactors=4, fa="pc", error.bars=TRUE,
                     main="Analise Paralela", n.iter=20,
                     ylabel=NULL,show.legend=TRUE,
                     sim=TRUE,quant=.95,cor="poly", 
                     #dados ordinais polypolica(?)
                     use="pairwise",plot=TRUE,correct=.5)

dados=conjunto1

# Numero de variaveis
nvar=dim(dados)[2]
x=c(1:nvar)

# Armazenando os resultados
observado=paralela$pc.values
reamostrado=paralela$pc.simr
simulado=paralela$pc.sim

resultado=cbind(x,observado,reamostrado, simulado)

resultado


# Analise fatorial

AF1 <- fa(dados, nfactors=3, rotate="Promax", cor=TRUE)

summary(AF1)

print(AF1,sort=TRUE)
#h2: comunalidade
#u2: especificidade (1-h2)

# Extraindo as informacoes para c�lculo da % explicada
# Numero de variaveis 
nvar=dim(dados)[2]

# Autovalores
auto=AF1$e.values

# % da variabilidade explicada
var_explicada=auto/nvar
var_explicada

# % da variabilidade explicada acumulada
var_explicada_a=var_explicada
for(i in 2:nvar) {
  var_explicada_a[i] <- var_explicada_a[i-1]+var_explicada[i]
}

# Tabela com as informacoes
x=c(1:nvar)
tabela_var=data.frame(cbind(x,auto,var_explicada,var_explicada_a))
tabela_var


# Scree plot 2
ggplot(tabela_var, aes(x=x, y=auto)) +
  scale_x_continuous(n.breaks=nvar)+
  labs(x="Fator ou componente", y="Autovalor")+
  geom_line()+
  geom_point(aes(x=x, y=auto))

# Cargas fatoriais
AF1$loadings

# Comunalidade e especificidade
comunalidade=AF1$communalities
especificidade=1-comunalidade

resultado2=cbind(comunalidade, especificidade)

resultado2

# Gerando os escores dos fatores
escores=data.frame(AF1$scores)

summary(escores)

hist(escores$MR1)

# Sao propostos entao 3 fatores:

# Fator 1 (autoeficacia para escola): 1 a 9

# Fator 2 (autoeficacia autoassertiva): 10 a 13

# Fator 3 (autoeficacia para suporte): 14 a 16



# Medidas de fidedignidade

# Escala para fator 1

fat1=dados[,1:9]

a=alpha(fat1,check.keys = TRUE)

summary(a)
a

# Escala para fator 2

fat2=dados[,10:13]

a=alpha(fat2)
summary(a)
a

# Escala para fator 3

fat3=dados[,14:16]

a=alpha(fat3, check.keys = TRUE)
summary(a)
a

######################################################################

# Analise Fatorial Confirmatoria


# Covariancia

covariancia=cov(dados)

# Grafico de correlacoes
cor.plot(covariancia,numbers=TRUE,main="Autoeficacia")

# Formulacao do modelo

I1=indProd(dados,var1=1:9,var2=14:15)

mod1 <- "Escola =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9
         Assertiva =~ X10 + X11 + X12 + X13
         Suporte =~ X14 + X15 + X16"

# Ajuste do modelo
fit1 <- cfa(mod1, data=dados, estimator = "WLSMV")

# Resultados
summary(fit1, fit.measures=TRUE, standardized = TRUE)


# Grafico
semPaths(fit1, what = 'par', layout = "tree2", sizeInt=0.5, std=F,
         edge.label.cex = 0.8, residuals = T, sizeLat=10,
         curve = 2.5, fade = F, rotation = 2, sizeMan=5,
         label.cex = 1.2, nCharNodes = 0, label.norm = "OOOOO",
         label.color = "gray10", border.color = "gray10",
         posCol = c("#024A5F"), negCol = c("#E46A4D"),
         edge.width=0.5, nCharEdges = 1)


# Indices de modificacao

mi1 <- modificationIndices(fit1)
mi1


