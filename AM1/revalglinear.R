
### APRENDIZADO ESTATISTICO
### REVISAO DE ALGEBRA DE MATRIZES
### PROF. GEORGE VON BORRIES

# NOTA: 
#       INFORMACOES SOBRE OPERADORES PODEM SER OBTIDAS EM
#       http://www.statmethods.net/advstats/matrix.html.

### EXEMPLOS:

library(Matrix)

# CRIANDO MATRIZES

(X <- matrix(c(1,5,-2,2,3,4),2,3))
rankMatrix(X)[1]  # Rank de uma matriz

(Y <- matrix(c(1,5,1,-2,2,2,3,4,1),3,3))
rankMatrix(Y)[1]

(Y <- matrix(c(1,5,1,-2,2,2,3,4,1),3,3, byrow=T))
rankMatrix(Y)[1]

(Y <- matrix(c(1,5,6,-2,2,0,3,4,7),3,3, byrow=T))
rankMatrix(Y)[1]

# Operacoes

c <- rbind(14,-11,-12)
dim(X) 
dim(c)
X*c           # Incorreto
X%*%c         # Correto   
is.matrix(X)
is.matrix(c)

# AB = CB, mas A diferente de C

(A <- matrix(c(1,2,3,0,2,-1),2,3))
(B <- matrix(c(1,0,1,2,1,0),3,2))
(C <- matrix(c(2,5,1,-6,1,-4),2,3))

A*B      # Incorreto
A%*%B    
C%*%B


# Matriz Triangular superior e inferior
# Obs. comandos do pacote base.

(m <- matrix(1:25,5,5))
lower.tri(m)
m[lower.tri(m)] <- 0 
m
(m <- matrix(1:25,5,5))
upper.tri(m)
m[upper.tri(m)] <- 0
m

# Matriz positiva definida?

S1 <- matrix(c(2,-1,-1,3),2,2)
solve(S1)

S2 <- matrix(c(13,-2,-3,-2,10,-6,-3,-6,5),3,3)
solve(S2)

# Inversa de uma matriz

(Y <- matrix(c(1,5,1,-2,2,2,3,4,1),3,3, byrow=T))
(YI <- solve(Y))
Y%*%YI
round(Y%*%YI, digits=0)
round(Y%*%YI, digits=10)
round(Y%*%YI, digits=15)
round(Y%*%YI, digits=16)

# Decomposicao de Cholesky

(D1 <- matrix(c(4,12,-16,12,37,-43,-16,-43,98),3,3))
(M <- chol(D1))
t(M)%*%M

# Matriz com vetores ortogonais

(C <- matrix(c(1,1,1,1,1,-2,1,-1,0),3,3))

t(C[,1])%*%C[,2]  # Colunas mutuamente ortogonais
t(C[,1])%*%C[,3]
t(C[,2])%*%C[,3]

# Matriz ortonormal 
#     Dividindo cada coluna pelo respectivo comprimento.

C[,1] <- as.vector(C[,1])
C[,2] <- as.vector(C[,2])
C[,3] <- as.vector(C[,3])

CON <- C  # MATRIZ ORTONORMAL

CON[,1] <- C[,1]%*%solve(sqrt(t(C[,1])%*%C[,1]))
CON[,2] <- C[,2]%*%solve(sqrt(t(C[,2])%*%C[,2]))
CON[,3] <- C[,3]%*%solve(sqrt(t(C[,3])%*%C[,3]))
CON

sqrt(t(C[,1])%*%C[,1])      # COMPRIMENTO DE C[,1]
sqrt(t(CON[,1])%*%CON[,1])  # COMPRIMENTO DE CON[,1]

round(CON%*%t(CON),2)
round(t(CON)%*%CON,2)

# Autovalores e Autovetores

(A = matrix(c(1, 0, 1, 3),2,2,byrow = T))
(autoA <- eigen(A))

autoA$values
autoA$vectors

A%*%autoA$vectors[,1]
autoA$values[1]*autoA$vectors[,1]
A%*%autoA$vectors[,2]
autoA$values[2]*autoA$vectors[,2]

autoA$vectors[,2]/min(autoA$vectors[,2])

(I <- diag(c(1,1)))
I+A
eigen(A)
eigen(I + A)

# Processo de Ortogonalizacao de Gram-Schmidt

# install.packages("pracma")
library(pracma)

(A = matrix(c(4,0,0,2,3,1,0,-1),4,2,byrow=TRUE))

(gs <- gramSchmidt(A))

round(t(gs$Q)%*%gs$Q,2)
round(gs$Q%*%gs$R,2)  

# Inversa condicional (generalizada)

library(MASS)

(A <- matrix(c(1,0,-1,2,0,1,1,-1,-1,-1,1,2),4,3))

solve(A)         # Problema: Matriz nao quadrada! 
(igA <- ginv(A))


round(A%*%igA,digits=7)
round(igA%*%A,digits=7)
round(A%*%igA%*%A,digits=7)
round(igA%*%A%*%igA,digits=7)


(Y <- matrix(c(1,5,1,-2,2,2,3,4,1),3,3))

(iY <- solve(Y))  # matriz quadrada e positiva definida
(igY <- ginv(Y))  

round(Y%*%igY,digits=7)
round(igY%*%Y,digits=7)
round(Y%*%igY%*%Y,digits=7)
round(igY%*%Y%*%igY,digits=7)

# Decomposicao espectral

# Matriz S com n=64 e p=4 testes psicologicos
# (n = 32 homens e 32 mulheres)
# (Ver Rencher, pag. 138)

psych <- read.table("c:/001-DADOS/cursos/T5_1_PSYCH.dat")
head(psych)
library(data.table)
nomes <- c("id", "y1", "y2", "y3", "y4")
setnames(psych,nomes)
head(psych)
require(plyr)
psych$id <- mapvalues(psych$id, 
                          from=c(1,2), 
                          to = c("mas","fem"))
head(psych)
tail(psych)

(psychmatrix <- as.matrix(psych[,2:5]))
(covpsych <- cov(psychmatrix))

eigpsm <- eigen(covpsych)   # AP = DP
(P <- eigpsm$vectors)
(D <- diag(eigpsm$values))

covpsych%*%P                # AP
eigpsm$values[1]%*%P[,1]    # D[,1]P
eigpsm$values[2]%*%P[,2]    # D[,2]P
eigpsm$values[3]%*%P[,3]    # D[,3]P
eigpsm$values[4]%*%P[,4]    # D[,4]P

covpsych
P%*%D%*%t(P)

solve(covpsych)
(icovpsych <- P%*%solve(D)%*%t(P))

# Traco de uma matriz

# install.packages("psych")
library(psych) 
tr(covpsych)



