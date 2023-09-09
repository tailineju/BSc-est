if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

phi.mc <- function(x, n){
    x.abs=abs(x)
    i=(1/n)*(x.abs/sqrt(2*pi))*sum(exp(((-runif(n,max=x.abs)^2)/2)))
    if(x>=0){
        result=0.5+i
    }
    if(x<0){
        result=0.5-i
    }
    return(result)
}

phi.mc(2,1000)


#comparing
x.seq<-seq(-3,3,0.5)
phi.mc.seq<-map(x.seq,phi.mc(2,1000))
pnorm.seq<-pnorm(x.seq)
cbind(x.seq,phi.mc.seq,pnorm.seq)


#calcule uma estimativa da var da sua estimativa MC de I(O)
r=100
phi.mc.2=function()
phi.mc(2,1000)
est.phi2=map(r,phi.mc.2)
quantile(est.phi2,c(0.25,0.975))