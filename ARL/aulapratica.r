if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

massa <- c(62, 62.9, 36.1, 54.6, 48.5, 42, 47.4, 50.6, 42, 48.7, 40.3, 33.1, 51.9, 42.4, 34.5, 51.1, 41.2, 51.9, 46.9)
taxa <- c(1792, 1666, 995, 1425, 1396, 1418, 1362, 1502, 1256, 1614, 1189, 913, 1460, 1124, 1052, 1347,1204,1867,1439)

# aula pratica 01/09
df <- data.frame(massa,taxa)

summary(df$massa)
sd(df$massa)
cf_x <- sd(df$massa)/mean(df$massa)

summary(df$taxa)
mean(df$taxa)
sd(df$taxa)
cf_y <- sd(df$taxa)/mean(df$taxa)

boxplot(df$massa)
boxplot(df$taxa)

ggplot(df, aes(x=massa,y=taxa)) +
geom_point()
stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")


cor(df$massa,df$taxa)

#h0: p = 0
#h1: p != 0

cor.test(massa, taxa)

#h0: p = 0
#h1: p > 0

cor.test(massa, taxa, alternative="greater")


## #############################################
#aula pratica 15/09

df$xiyi<- massa*taxa
df$xi2<- massa^2
n<-nrow(df)
xbarra<- mean(massa)
ybarra<- mean(taxa)

beta1<- (sum(df$xiyi) - (n*xbarra*ybarra))/(sum(df$xi2)-(n*(xbarra)^2))
beta0<- ybarra - (beta1*xbarra)

df$estim <- beta0 + (beta1*df$massa) #media de taxa de metabolismo

df$residuo<- df$taxa - df$estim
df$residuo2 <- df$residuo^2

sum(df$residuo) #==0

sum(df$residuo2) #minimo (menor soma possivel)

sum(df$taxa) 
#==
sum(df$estim)

df$xiei <- df$massa * df$residuo

sum(df$xiei) #==0

df$yiei <- df$estim * df$residuo

sum(df$yiei) #==0

beta0 + (beta1*xbarra) #==ybarra

#estim variancia
sigma2 <- (sum(df$residuo2))/(n-2)

#denom variancias
denom <- sum(df$xi2) - (n*(xbarra^2))

s2beta0 <- sigma2 * (sum(df$xi2)/(n*denom))
s2beta1 <- sigma2/denom

sbeta1 <- sqrt(s2beta1)
sbeta0 <- sqrt(s2beta0)
