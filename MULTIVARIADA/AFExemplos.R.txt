
###
# ANALISE MULTIVARIADA 1
#### ANALISE FATORIAL - EXEMPLOS ####
# PROF. GEORGE VON BORRIES;

#### . ####

### J&W - Exemplo Exemplo 9.3 ####
# PAG. 491
# Solucao utilizando Componentes Principais

# Taste, Money (good by for money), Flavor
# Snack (suitable for snack), 
# Energy (provides lots of energy)

library(MCMCpack) # facil de gerar a matriz correlacoes

(concor <- xpnd(c(1,0.02,0.96,0.42,0.01,
                  1,0.13,0.71,0.85,
                  1,0.50,0.11,
                  1,0.79,
                  1),5))

dimnames(concor) <- list(c("Taste","Money","Flavor","Snack","Energy"),
                         c("Taste","Money","Flavor","Snack","Energy"))
concor

(sol <- principal(concor, nfactors = 2, 
                  rotate = 'none', covar = FALSE))

load <- sol$loadings[,1:2]
plot(load,type="n")
text(load,labels=c("Taste","Money","Flavor",
                   "Snack","Energy"),cex=1.2)


#### J&W - Exemplo Exemplo 9.4 ####
# PAG. 493

library(data.table)
library(psych)

stock <- read.delim("c:/001-DADOS/cursos/T8-4.dat",header=F)         # windows
# stock <- read.delim("~/datasets/T8-4.DAT",header=F) # linux
nomes <- c("JPMorgan", "Citibank", "WellsFargo", 
           "Shell", "Exxon")
setnames(stock,nomes)
head(stock)

cor(stock)

#### ..... PC, m=1 ####

(af1stock <- principal(stock, nfactors = 1, 
                      rotate = 'none',
                      covar = FALSE))

(resaf1stock <- (round(cor(stock) 
              - af1stock$loadings %*% t(af1stock$loadings)
              - diag(af1stock$uniquenesses),3)))


#### ..... PC, m=2 ####

(af2stock <- principal(stock, nfactors = 2, 
                       rotate = 'none',
                       covar = FALSE))

(resaf2stock <- (round(cor(stock) 
                - af2stock$loadings %*% t(af2stock$loadings)
                - diag(af2stock$uniquenesses),3)))

(load <- af2stock$loadings[,1:2])
plot(load,type="n",xlim=c(0.5,0.9),ylim=c(-0.5,0.9))
text(load,labels=c("Morgan","Citibank","WF",
                   "Shell","Exxon"),cex=1.2)

#### J&W - Exemplo Exemplo 9.5 ####
# PAG. 497

#### ..... MV, m=2 ####

                               # cov ou cor
af2stockemv <- factanal(covmat = cor(stock),
                        factors = 2, rotation = "none")
af2stockemv$uniquenesses
af2stockemv$loadings
print(af2stockemv, digits=3, cutoff=.0003)

(resaf2stockemv <- (round(cor(stock)
                         - (af2stockemv$loadings[,1:2] 
                         %*% t(af2stockemv$loadings[,1:2])) 
                         - diag(af2stockemv$uniquenesses),3)))

(load2 <- af2stockemv$loadings[,1:2])
plot(load2,type="n",xlim=c(0,1),ylim=c(0,1))
text(load2,labels=c("Morgan","Citibank","WF",
                   "Shell","Exxon"),cex=1.2)

sum(abs(resaf1stock))
sum(abs(resaf2stock))
sum(abs(resaf2stockemv))


#### J&W - Exemplo Exemplo 9.7 ####
# PAG. 503

#### .... Teste MV, m = 2 

n <- nrow(stock)
p <- ncol(stock)
m <- 2

af2stockemv$uniquenesses
L <- af2stockemv$loadings[,1:2]

psi <- diag(af2stockemv$uniquenesses,p)

detratio <- det(L %*% t(L) + psi)/det(cor(stock))
est <- (n - 1- (2*p + 4*m + 5)/6) * log(detratio)
gl <- ((p-m)^2-p-m)/2

1 - pchisq(est,gl)  # p-valor

# Utilizando funcao fa do pacote psych
#### Caso Heywood #### 

teste <- fa(cor(stock), nfactors = 2, rotate = 'none', fm='ml',
            n.obs = n) # indicar num. de obs
teste$STATISTIC
teste$PVAL


#### .....J&W - Exercicio 9.8 ####

(covM <- xpnd(c(1,0.4,0.9,
                1,0.7,
                1),3))


(hs0 <- factanal(covM, factors=1, rotation="none"))

(hs1 <- principal(covM, nfactors = 1, 
                  rotate = 'none',
                  covar = T))
(rhs1 <- (round(covM 
                - hs1$loadings %*% t(hs1$loadings)
                - diag(hs1$uniquenesses),3)))

(hs2 <- fa(covM, nfactors = 1, 
           rotate = "none", fm = "ml"))
(rhs2 <- (round(covM 
                - hs2$loadings %*% t(hs2$loadings)
                - diag(hs2$uniquenesses),3)))

abs(rhs1) - abs(rhs2)


#### J&W - Exemplo 9.8 ####
# PAG. 505
# Rotacao de Fatores

(R <- xpnd(c(1, 0.439, 0.410, 0.288, 0.329, 0.248, 
             1    , 0.351, 0.354, 0.320, 0.329,
             1    , 0.164, 0.190, 0.181,
             1    , 0.595, 0.470,
             1    , 0.464,
             1    ), 6))

dimnames(R) <- list(c("Gaelic","English","History",
                      "Arithmetic","Algebra","Geometry"),
                    c("Gaelic","English","History",
                      "Arithmetic","Algebra","Geometry"))
R

af2exams <- factanal(covmat = R,
                     factors = 2, rotation = "none")
af2exams$uniquenesses  # Variância específica
round(1 - af2exams$uniquenesses,3) # Comunalidades hi^2
(L <- af2exams$loadings[,1:2])

plot(L,type="n",xlim=c(-1,1),ylim=c(-1,1))
text(L,labels=c("Ga","En","Hi",
                "Ar","Al","Ge"), cex=1.2)
abline(h=0, v=0, col = "blue")

# Rotacao 

# install.packages("REdaS")
library(REdaS)

phi <- deg2rad(20) 
rad2deg(phi)       

# sentido horario

T1 <- matrix(c(cos(phi), sin(phi), 
               -sin(phi), cos(phi)), 2, 2, byrow = T)

LT1 <- L %*% T1

plot(LT1,type="n",xlim=c(-1,1),ylim=c(-1,1))
text(LT1,labels=c("Ga","En","Hi",
                  "Ar","Al","Ge"), cex=1.2)
abline(h=0, v=0, col = "blue")

# sentido anti-horario

T2 <- matrix(c(cos(phi), -sin(phi), 
               sin(phi), cos(phi)), 2, 2, byrow = T)

LT2 <- L %*% T2

plot(LT2,type="n",xlim=c(-1,1),ylim=c(-1,1))
text(LT2,labels=c("Ga","En","Hi",
                  "Ar","Al","Ge"), cex=1.2)
abline(h=0, v=0, col = "blue")

# Rotacao varimax

af2examsr <- factanal(covmat = R,
                      factors = 2, rotation = "varimax")

(Lr <- af2examsr$loadings[,1:2])

plot(Lr,type="n",xlim=c(-1,1),ylim=c(-1,1))
text(Lr,labels=c("Ga","En","Hi",
                 "Ar","Al","Ge"), cex=1.2)
abline(h=0, v=0, col = "blue")

#### J&W - Exemplo 9.12 ####
# PAG. 517
# Factor Scores

af2stockemvr <- factanal(covmat = cor(stock),
                         factors = 2, rotation = "varimax")
psi <- diag(af2stockemvr$uniquenesses,5)
L <- af2stockemvr$loadings[,1:2]

z <- as.vector(c(0.50, -1.40, -0.20, -0.70, 1.40))

# weighted least squares
fwls <- solve(t(L) %*% solve(psi) %*% L) %*% t(L) %*% solve(psi) %*% z

# regression
freg <- t(L) %*% solve(cor(stock)) %*% z


#### J&W - Exemplo 9.14 ####
# PAG. 520

mcor <- xpnd(c(1, 0.505, 0.569, 0.602, 0.621, 0.603,
               1, 0.422, 0.467, 0.482, 0.450,
               1, 0.926, 0.877, 0.878,
               1, 0.874, 0.894,
               1, 0.937,
               1),6)

afpc3 <- principal(mcor, nfactors=3, rotate = 'none')
afpc3

afpc3r <- principal(mcor, nfactors=3, 
                    rotate = 'varimax')
afpc3r

avml3 <- factanal(mcor, factors = 3, rotation = 'none')
avml3 <- fa(mcor, nfactors = 3, rotate = 'none', 
            fm = 'ml')
avm13
avm13r <- fa(mcor, nfactors = 3, rotate = 'varimax', 
             fm = 'ml')

L <- avm13$loadings[,1:3]
psi <- diag(avm13$uniquenesses,6)

round(mcor - L %*% t(L) - psi,3)




