
###
# ANALISE MULTIVARIADA
# Prof. George von Borries
#### Exemplos Notas de Aula ####


#### (1) XYdata ####

require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
cl1 <- kmeans(x, 2)
cl2 <- kmeans(x, 3)

par(mfrow=c(1,1))

plot(x, pch=19,cex=1)

par(mfrow=c(1,2))

plot(x, col = cl1$cluster)
points(cl1$centers, col = 1:2, pch = 19, cex=2)

cl2c <- cl2$cluster-1
cl2c[cl2c==0] <- 3

plot(x, col = cl2c)
points(cl2$centers, col = c(3,1,2), pch = 19, cex=2)


#### (2) Analise Grafica ####

#### (2.1) Andrews Plot ####

install.packages("andrews")
require(andrews)

data(iris)
head(iris)

par(mfrow=c(2, 2))

# 1: f(t)=x1/(2^0.5)+x2*sin(t)+x3*cos(t)+x4*sin(2*t)+x5*cos(2*t)+...
# 2: f(t)=x1*sin(t)+x2*cos(t)+x3*sin(2*t)+x4*cos(2*t)+...
# 3: f(t)=x1*cos(t)+x2*cos((2*t)^0.5)+x3*cos((3*t)^0.5)+...
# 4: f(t)=1/(2^0.5)*(x1+x2*(sin(t)+cos(t))+x3*(sin(t)-cos(t))+
#                    x4*(sin(2*t)+cos(2*t))+x5*(sin(2*t)-cos(2*t))+...)

andrews(iris,type=1,step=500,clr=5,ymax=3,main='Andrews Plot 1')
andrews(iris,type=2,step=500,clr=5,ymax=3,main='Andrews Plot 2')
andrews(iris,type=3,step=500,clr=5,ymax=3,main='Andrews Plot 3')
andrews(iris,type=4,step=500,clr=5,ymax=3,main='Andrews Plot 4')

par(mfrow=c(1, 1))

# Para saber a ordem das cores basta pedir o comando
# pie(rep(1,4),col = rainbow(4))

#### (2.2) Grafico de Perfis ou Parallel Plot #### 

iris[1:3,]
library(data.table)
nomes <- c("Sep.C", "Sep.L", "Pet.C", "Pet.L", 
           "Especie")
setnames(iris,nomes)
iris[1:3,]


library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Especie, data = iris,
      xlab=" ",
      panel = panel.superpose,
      key = list(title = "Variedades de Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(c("Setosa", "Versicolor", "Virginica"))))


parallelplot(~iris[1:4] | Especie, iris) 
parallelplot(~iris[1:4], iris, groups = Especie,
             horizontal.axis = FALSE, scales = list(x = list(rot = 90)))

#### (2.3) Face Plots ####

install.packages("aplpack")
library(aplpack)

sampiris <- rbind(iris[ 1,],iris[ 5,],iris[6,],
                  iris[51,],iris[55,],iris[56,],
                  iris[101,],iris[105,],iris[106,])
sampiris
faces(sampiris[,1:4],face.type=0)
faces(sampiris[,1:4],face.type=1)


#### (2.4) Heatmaps #### 

library("foreign")   # to use function read.xport
install.packages("gplots")
library("gplots")    # to use heatmaps
install.packages("heatmap.plus")
library("heatmap.plus")  # to use heatplus

# Instalando o Pacote heatplus do Bioconductor
# Informacoes sobre o pacote:  
# http://www.bioconductor.org/packages/2.12/bioc/html/Heatplus.html

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Heatplus")

library("Heatplus")

irism <- as.matrix(iris[,1:4])

hm1 <- regHeatmap(t(irism)) #, legend = 2, col=heat.colors, breaks=-3:3)
plot(hm1)

hm2 <- regHeatmap(t(irism), legend = 2, col=heat.colors,
                  breaks = -4:4)
plot(hm2)

hm3 <- regHeatmap(t(as.matrix(sampiris[,1:4])),
                  dendrogram = list(status="hide"))
plot(hm3)


#### (3) Matriz de Distancias #### 

require(graphics)

m <- c(0,9,3,6,11,9,0,7,5,10,3,7,0,9,2,6,5,9,0,8,11,10,2,8,0)
(m <- matrix(m,5,5))

(d <- as.dist(m))

(d1 <- dist(sampiris[,1:4],method="euclidean",diag=F))
(d2 <- dist(sampiris[,1:4],method="manhattan",diag=F))
(d3 <- dist(sampiris[,1:4],method="minkowski",diag=F,p=2))  # = d1
(d4 <- dist(sampiris[,1:4],method="minkowski",diag=F,p=1))


#### (3.1) Medidas de Similiaridade #### 

library(SimilarityMeasures)

(point1 <- sampiris[1,1:4])
(point2 <- sampiris[2,1:4])

DistanceSq(point1,point2,4)
# ou
sum((point1-point2)^2)

#### (3.2) Medidas de Concordancia #### 

library("proxy")

summary(pr_DB) # lista de medidas
pr_DB$get_entry("Jaccard") # Descricao da medida
pr_DB$get_entry("Dice")


(x <- matrix(sample(c(FALSE, TRUE), 16, 
                   rep = TRUE), ncol = 4))

dist(x,method="Jaccard",by_rows=T)


animais <- matrix(c(1,1,1,1,1,0,0,0,0,1,0,0,0,0,0,
                    0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,
                    1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,
                    0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,
                    0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
                    0,0,0,0,0,0,0,0,1,0,0,1,1,1,0,
                    1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
                    0,0,0,1,1,1,1,0,1,1,1,1,0,0,0,
                    0,0,0,0,0,0,0,1,1,0,1,1,1,1,0,
                    1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
                    0,1,1,0,0,0,0,0,0,0,0,0,0,0,0),13,15,byrow=T)

colnames(animais) <- c("pomba", "pato",
                       "ganso", "coruja", "falcao",
                       "aguia", "raposa","cachorro",
                       "lobo", "gato","tigre","leao",
                       "cavalo","zebra","vaca")

rownames(animais) <- c("pequeno","medio","grande",
                       "2patas","4patas","pelos",
                       "cascos","juba","penas",
                       "caça","corre","voa","nada")

animais

dist(animais,method="Jaccard",by_rows=T)
simil(animais,method="Jaccard",by_rows=T)

dist(animais,method="Dice",by_rows=T)
simil(animais,method="Dice",by_rows=T)

dist(animais,method="Jaccard",by_rows=F)
simil(animais,method="Jaccard",by_rows=F)


#### (4) ALGORITMOS ####

#### (4.1) ALGORITMOS HIERARQUICOS ####

m <- c(0,9,3,6,11,9,0,7,5,10,3,7,0,9,2,6,5,9,0,8,11,10,2,8,0)
(m <- matrix(m,5,5))
(d <- as.dist(m))

par(mfrow=c(1, 1))

hcs <- hclust(d, "single")
plot(hcs)
plot(hcs, hang = -1, cex.axis=1.2, cex.lab = 1.2, 
     xlab="Ligação Simples", main="Dendograma",
     ylab="Distância")

hcc <- hclust(d, "complete")
plot(hcc)
plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Ligação Completa", main="Dendograma")

hcc <- hclust(d, "average")
plot(hcc)
plot(hcc, hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Ligação Média", main="Dendograma")


hcsa <- hclust(dist(animais,method="Jaccard",by_rows=T)
               ,"single")
plot(hcsa,hang=-1)

hcca <- hclust(dist(t(animais),method="Jaccard",by_rows=T),
               "complete")
plot(hcca,hang=-1)

hcaa <- hclust(dist(t(animais),method="Jaccard",by_rows=T),
               "average")
plot(hcaa,hang=-1)

# Numero de fusoes possiveis

nf <- function(N){
  nh <- 0 
  for(i in 2:N) nh <- nh + choose(i,2) 
  nh
}

nf(1000)

# Numero de divisoes possiveis

nd <- function(N) {2^(N-1) - 1}

nd(1000)

#### (4.2) ALGORITMOS NAO-HIERARQUICOS ####

library(cluster)

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")

# CLARA

clarax.2 <- clara(x, 2, samples = 20, metric = "euclidean")
clarax.2$clusinfo
clarax.2$clustering

par(mfrow=c(1, 1))
plot(x, col = clarax.2$clustering)
points(clarax.2$medoids, col = 1:2, pch = 19, cex=2)

# PAM

pamx.2 <- pam(x, 2)
summary(pamx.2)
pamx.2$clusinfo
pamx.2$clustering

par(mfrow=c(1, 1))
plot(x, col = pamx.2$clustering)
points(pamx.2$medoids, col = 1:2, pch = 19, cex=2)

# AGNES 

# votes.repub: percentual de votos dados a candidatos
#              republicanos nas eleicoes presidenciais
#              de 1856 a 1976. Linhas = 50 estados, 
#              colunas = 31 eleicoes.

data(votes.repub)
head(votes.repub)                                # standardization   
agn1 <- agnes(votes.repub, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)
              # pares de dissimilaridades (distancias)
agn2 <- agnes(daisy(votes.repub), diss = TRUE, 
              method = "complete")
plot(agn2)

#### (5) k-Means ####

# Agrupamento k-Means
# Dados geyser: arquivo do livro 
#               Modern Multvariate Statistical Techniques (Izenman)
# Reproduzindo Exemplo de Izenman, pag. 409-410
# Prof. George von Borries.


load("MULTIVARIADA/exemplos R/geyser.rda")
geyser

(meang <- apply(geyser,2,mean))
(varg <- apply(geyser,2,var))
(stdg <- sqrt(varg))

geyser.sc <- scale(geyser,center=meang,scale=stdg)

apply(geyser.sc,2,mean)
apply(geyser.sc,2,var)

geyser.sc.d <- dist(geyser.sc)

par(mfrow=c(2, 2))

geyser.km2 <- kmeans(geyser.sc,2, iter.max = 1000)
geyser.km3 <- kmeans(geyser.sc,3, iter.max = 1000)
geyser.k <- cbind(geyser,geyser.km2$cluster,geyser.km3$cluster)
geyser.k[1:5,]
plot(geyser.k$X1,geyser.k$X2,col=geyser.km2$cluster,pch=16,
     main="K-means, G=2",xlab="X1",ylab="X2")
plot(geyser.k$X1,geyser.k$X2,col=geyser.km3$cluster,pch=16,
     main="K-means, G=3",xlab="X1",ylab="X2")

geyser.km2$withinss / geyser.km2$totss 
geyser.km2$betweenss / geyser.km2$totss  
geyser.km3$withinss / geyser.km3$totss
geyser.km3$betweenss / geyser.km3$totss

geyser.km2$totss - geyser.km2$tot.withinss
geyser.km2$betweenss

n <- nrow(geyser)
(r2g2 <- geyser.km2$betweenss / geyser.km2$totss)  
(r2g3 <- geyser.km3$betweenss / geyser.km3$totss)

g <- 2
(pseudoF2 <- (n-g)/(g-1) * r2g2/(1-r2g2)) # maior Pseudo-F => melhor agrupamento
g <- 3
(pseudoF3 <- (n-g)/(g-1) * r2g3/(1-r2g3)) 

hcsg <- hclust(geyser.sc.d, "single")
groups.2 <- cutree(hcsg,2)
table(groups.2)
groups.3 <- cutree(hcsg,3)
table(groups.3)
geyser.g <- cbind(geyser,groups.2,groups.3)
geyser.g[1:5,]
plot(geyser.g$X1,geyser.g$X2,col=geyser.g$groups.2,pch=16,
     main="Ligação Simples, G=2",xlab="X1",ylab="X2")
plot(geyser.g$X1,geyser.g$X2,col=geyser.g$groups.3,pch=16,
     main="Ligação Simples, G=3",xlab="X1",ylab="X2")


#### (6) ARI ####

library(mclust)
adjustedRandIndex(geyser.km2$cluster,geyser.g$groups.2)
adjustedRandIndex(geyser.km3$cluster,geyser.g$groups.3)


par(mfrow=c(1, 1))


#### (7) MCLUST ####

library(mclust)
n <- length(geyser[,1])
geyser.mclust2den <- densityMclust(geyser, model="VVV", G = 2)

# simulando amostra da densidade
sim.results <- simVVV(geyser.mclust2den$parameters, n, seed = 0)
ysim <- sim.results[,c(2,3)]
gsim <- sim.results[,"group"]
ysim1 <- ysim[gsim==1, ]
ysim2 <- ysim[gsim==2, ]

plot(geyser.mclust2den, col="black", 
     xlab = "Dimensao 1", ylab = "Dimensao 2",
     xlim = c(min(ysim[,1]), max(ysim[,1])), 
     ylim = c(min(ysim[,2]), max(ysim[,2])),
     levels = round(quantile(geyser.mclust2den$density,
                             probs = c(0.05, 0.25, 0.5, 0.75,
                                       0.95, 0.99)), 3))
points(ysim1, col="red", pch=19)
points(ysim2, col="blue", pch=19)
legend(x=1.5, y = 95, legend = c("Contornos densidade",
                                  "Componente 1",
                                  "Componente 2"),
       col = c("black", "red", "blue"), lty = c(1,0,0),
       lwd = c(2,0,0), pch = c(NA, 19, 19))