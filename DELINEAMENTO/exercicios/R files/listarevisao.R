################
#ex. 1 
M=100
X=85
n=16
sd=12
alpha=0.1


#h0: M=100
#h1: M<100

ttab = qt(alpha,n-1) # var populacional desconhecida e n pequeno - t de student

Xc = (ttab * (sd/sqrt(n))+M)#regiao critica

tcalc =(X-M)/(sd/sqrt(n)) #T calculado

pvalor = pt(tcalc,n-1) #pvalor

tbeta=(Xc-90)/(sd/sqrt(n))

beta = pt(tbeta,n-1,lower.tail = FALSE)

poder = 1 - beta

ttab
Xc
tcalc
pvalor 
beta
tbeta
poder

####################
#ex. 2
antes<-c(13.6,13.6,14.7,12.1,12.3,13.2,11,12.4)
depois<-c(11.4,12.5,14.6,13,11.7,10.3,9.8,10.4)

df<- data.frame(antes,depois)
df$dif<- antes-depois
d<- mean(df$dif)
sd2<- sd(df$dif)

n=nrow(df)
M2=0 
alpha=.05
tcalc2 <- (d-M2)/(sd2/sqrt(n))
pvalor <- pt(tcalc2, n-1, lower.tail = FALSE)

ICsup= d + qt(1-alpha/2,n-1)*(sd2/sqrt(n))
ICinf= d - qt(1-alpha/2,n-1)*(sd2/sqrt(n))

IC<-c(ICinf,ICsup)
IC
t.test(antes,depois, paired= TRUE, alternative="greater")


###########################
# ex. 3

varA<-c(1.3,1.4,1.1,1.4,1.5)
varB<- c(1.8,1.6,1.9,1.9,1.8)

xa <-mean(varA)
xb<- mean(varB)

sa<-sd(varA)^2
sb<-sd(varB)^2

df3<- data.frame(varA,varB)

#Normalidade
#h0: normais
#h1: nao normais
shapiro.test(df3$varA) #normal
shapiro.test(df3$varB) #normal

#Homocedasticidade
#h0: iguais
#h1: diferentes
var.test(df3$varA,df3$varB) #iguais
n=nrow(df3)


# Teste t
t.test(varA, varB, var.equal = TRUE)
