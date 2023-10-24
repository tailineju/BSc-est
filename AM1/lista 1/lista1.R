if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,tinytex)
#####################################
#ex 4 - livro 1.1
x1 <- c(3,4,2,6,8,2,5)
x2 <- c(5,5.5,4,7,10,5,7.5)

plot(x=x1,y=x2)

#means
mean(x1)
mean(x2)

#vars
sd(x1)^2
sd(x2)^2

#cov
cov(x1,x2)

#cov by hand
n<- length(x1)
covv <- sum((x1-mean(x1))*(x2-mean(x2)))/(n-1)


#####################################
#ex 5 - livro 1.2
y1<-c(1,2,3,3,4,5,6,8,9,11)
y2<-c(18.95,19,17.95,15.54,14,12.95,8.94,7.49,6,3.99)

#a
plot1<-plot(x=y1,y=y2)
ggMarginal(plot1, type="histogram")
