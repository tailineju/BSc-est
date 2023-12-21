## Estatística Computacional
## Donald Pianto
## Monte Carlo Estimation of Trimmed Means

## E |X1 - X2| = 2/sqrt(pi) = 1.128379
m <- 1000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}
est <- mean(g)
est

## Qual o erro padrão da estimativa Monte Carlo?
## Var(|X1-X2|) = 2-4/pi

## voltar aos slides

## Então o erro padrão = sqrt((2-4/pi)/m) = 0.02695850
## Nossa estimativa do erro padrão é
sqrt(sum((g - mean(g))^2)) / m

## voltar ao slides

## MSE de uma média truncada
n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
  x <- sort(rnorm(n))
  tmean[i] <- sum(x[2:(n-1)]) / (n-2)
}
mse <- mean(tmean^2)
mse
sqrt(sum((tmean - mean(tmean))^2)) / m #se

## O EQM da média amostral é Var(X)/n = 1/20 = 0.05

## Como isso compara com a mediana?

n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
  x <- sort(rnorm(n))
  tmean[i] <- median(x)
}
mse <- mean(tmean^2)
mse
sqrt(sum((tmean - mean(tmean))^2)) / m #se

## Volta aos slides

n <- 20
K <- n/2 - 1
m <- 1000
mse <- matrix(0, n/2, 6)

trimmed.mse <- function(n, m, k, p) {
  #MC est of mse for k-level trimmed mean of
  #contaminated normal pN(0,1) + (1-p)N(0,100)
  tmean <- numeric(m)
  for (i in 1:m) {
    sigma <- sample(c(1, 10), size = n,
                    replace = TRUE, prob = c(p, 1-p))
    x <- sort(rnorm(n, 0, sigma))
    tmean[i] <- sum(x[(k+1):(n-k)]) / (n-2*k)
  }
  mse.est <- mean(tmean^2)
  se.mse <- sqrt(mean((tmean-mean(tmean))^2)) / sqrt(m)
  return(c(mse.est, se.mse))
}

for (k in 0:K) {
  mse[k+1, 1:2] <- trimmed.mse(n=n, m=m, k=k, p=1.0)
  mse[k+1, 3:4] <- trimmed.mse(n=n, m=m, k=k, p=.95)
  mse[k+1, 5:6] <- trimmed.mse(n=n, m=m, k=k, p=.9)
}

mse

## Terminou
## Volta aos slides
