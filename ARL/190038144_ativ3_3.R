x <- matrix(c(1,1,1,1,1,8,4,0,-4,-8),5,2)
y <- matrix(c(7.8,9,10.2,11, 11.7),5,1)

#a
yty <- t(y)%*%y

xtx <- t(x)%*%x

xty <- t(x)%*%y

#b
xtxi <- solve(xtx)

#c
beta <- xtxi %*% xty

#d
yest <- x %*% beta

ei <- y - yest

#e
SSE <- yty - (t(beta) %*% t(x) %*% y)

um <- matrix(rep(1,5),5,1)
umumt <- um%*%t(um)

n <- 5

SSTO <- yty - ((1/n)%*% t(y) %*% umumt %*% y)

SSR <- SSTO - SSE


#f

sigma2 <- SSE/(n-2)

var <- as.numeric(sigma2) * xtxi