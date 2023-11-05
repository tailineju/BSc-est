x <- matrix(c(1,1,1,1,1,8,4,0,-4,-8),5,2)
x
y <- matrix(c(7.8,9,10.2,11, 11.7),5,1)
y

#a
yty <- t(y)%*%y
yty

xtx <- t(x)%*%x
xtx

xty <- t(x)%*%y
xty

#b
xtxi <- solve(xtx)
xtxi

#c
beta <- xtxi %*% xty
beta

#d
yest <- x %*% beta
yest

ei <- y - yest
ei

#e
SSE <- yty - (t(beta) %*% t(x) %*% y)
SSE

um <- matrix(rep(1,5),5,1)
umumt <- um%*%t(um)

n <- 5

SSTO <- yty - ((1/n)%*% t(y) %*% umumt %*% y)
SSTO

SSR <- SSTO - SSE
SSR

#f

sigma2 <- SSE/(n-2)
sigma2

var <- as.numeric(sigma2) * xtxi
var