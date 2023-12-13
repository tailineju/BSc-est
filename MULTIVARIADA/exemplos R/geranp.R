
# ANALISE MULTIVARIADA 1
# PROF. GEORGE VON BORRIES
# PROGRAMA PARA GERAR NORMAL BIVARIADA

#### Parametros da N2 ####

mu <- c(0, 0)
Sigma <- matrix(c(1, .9, .9, 1), 
                nrow = 2, ncol = 2)

#### Gerando N2 ####

#### ....Decomposicao Espectral #### 

rmvn.eigen <-
  function(n, mu, Sigma) {
    p <- length(mu)
    ev <- eigen(Sigma, symmetric = TRUE)
    lambda <- ev$values
    V <- ev$vectors
    R <- V %*% diag(sqrt(lambda)) %*% t(V)
    Z <- matrix(rnorm(n*p), nrow = n, ncol = p)
    X <- Z %*% R + matrix(mu, n, p, byrow = TRUE)
    X
  }


#### .... Decomposicao em Valores Singulares ####

rmvn.svd <-
  function(n, mu, Sigma) {
    p <- length(mu)
    S <- svd(Sigma)
    R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v)
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)
    X <- Z %*% R + matrix(mu, n, p, byrow=TRUE)
    X
  }

#### .... Decomposicao de Cholesky ####

rmvn.cholesky <-
  function(n, mu, Sigma) {
    p <- length(mu)
    Q <- chol(Sigma)
    Z <- matrix(rnorm(n*p), nrow=n, ncol=p)
    X <- Z %*% Q + matrix(mu, n, p, byrow=TRUE)
    X
  }


#### Gerando as Amostras ####

a1 <- rmvn.eigen(1000, mu, Sigma)
plot(a1, xlab = "x", ylab = "y", pch = 20)
print(colMeans(a1))
print(cor(a1))
  
a2 <- rmvn.svd(1000, mu, Sigma)
plot(a2, xlab = "x", ylab = "y", pch = 20)
print(colMeans(a2))
print(cor(a2))
  
a3 <- rmvn.cholesky(1000, mu, Sigma)
plot(a3, xlab = "x", ylab = "y", pch = 20)
print(colMeans(a3))
print(cor(a3))
  
#### Checando Desvio de Normalidade ####  

####....Elipse de Contorno#### 
library(car)

dataEllipse(a1,pch=20, 
            main = "Decomposicao Espectral")
  
dataEllipse(a2,pch=20, 
            main= "Decomposicao em Valores Singulares")

dataEllipse(a3,pch=20, 
            main= "Decomposicao de Cholesky")


####.... Q-Q Plot - Rencher (2012)     ####   

ndc <- function(dados){
  S <- var(dados)  
  dadosc <- dados
  dadosc[,1] <- dados[,1] - mean(dados[,1])
  dadosc[,2] <- dados[,2] - mean(dados[,2])  
  
  n <- nrow(dadosc); p <- ncol(dadosc)
  D2 <- rep(0,n)
  
  for(i in 1:n){ 
    D2[i] <- t(as.vector(dadosc[i,])) %*% solve(S) %*% as.vector(dadosc[i,])}
  
    ui  <- sort(n * D2/(n - 1)^2)
    alpha <- 0.5 * (p - 2) / p
    beta  <- 0.5 * (n - p - 3) / (n-p-1) 
    i <- c(1:n)
    q.vi <- (i-alpha)/(n-alpha-beta+1) 
    a <- 0.5
    b <- 0.5 * (n-p-1)
    vi <- qbeta(q.vi,a,b)
    plot(ui,vi)
    model <- lm(vi ~ui)
    abline(model, col=4, lwd=2)  
}
  
ndc(a1) 
ndc(a2)
ndc(a3)

#### Geracao N2 com pacotes do R ####
  
library(MASS)
library(mvtnorm)

r1 <- mvrnorm(1000, mu = mu, Sigma=Sigma)    
print(colMeans(r1))
print(cor(r1))

dataEllipse(r1,pch=20, 
            main= "Funcao mvrnorm do Pacote Mass")

ndc(r1)

r2 <- rmvnorm(1000, mean = mu, sigma=Sigma, 
              method="eigen")
print(colMeans(r2))
print(cor(r2))
    
dataEllipse(r2,pch=20, 
            main= "Funcao rmvnorm do Pacote mvtnorm - eigen")

ndc(r2)


r3 <- rmvnorm(1000, mean = mu, sigma=Sigma, 
              method="svd")
print(colMeans(r3))
print(cor(r3))
    
dataEllipse(r3,pch=20, 
            main= "Funcao rmvnorm do Pacote mvtnorm - svd")
ndc(r3)

    
r4 <- rmvnorm(1000, mean = mu, sigma=Sigma, 
              method="chol")
print(colMeans(r4))
print(cor(r4))
  
dataEllipse(r4,pch=20, 
            main= "Funcao rmvnorm do Pacote mvtnorm - chol")
ndc(r4)
    
#### Comparando tempo de geracao #### 

library(microbenchmark)

ben <- microbenchmark(
  rmvn.eigen(10000, mu, Sigma),
  rmvn.svd(10000, mu, Sigma),
  rmvn.cholesky(10000, mu, Sigma),
  mvrnorm(10000, mu = mu, Sigma=Sigma), 
  rmvnorm(10000, mean = mu, sigma=Sigma, method="eigen"),
  rmvnorm(10000, mean = mu, sigma=Sigma, method="svd"),
  rmvnorm(10000, mean = mu, sigma=Sigma, method="chol"),
  times = 1000 # altere times 
)
  
ben
  
autoplot(ben)

  
  