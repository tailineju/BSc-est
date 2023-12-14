
###
# ANALISE MULTIVARIADA 1
# PROVA 3
# TAILINE J. S. NONATO
# 190038144
###

pacman::p_load(tidyverse,knitr,factoextra,mvnTest)
#remotes::install_github("r-hyperspec/hyperSpec")
#library("hyperSpec")

# Exercício 12.11 ----

x1 <- c(5,1,-1,3)
x2 <- c(4,-2,1,1)
item <- c("A","B","C","D")
df <- data.frame(item,x1,x2)

c.ab <- df %>%
  filter(item == c("A","B")) %>%
  select(!item) %>%
  summarise_all(list(mean))

c.cd <- df %>%
  filter(item == c("C","D")) %>%
  select(!item) %>%
  summarise_all(list(mean))

df.c<- as.matrix(rbind(c.ab,c.cd))
df.c

km <- kmeans(df[,2:3],centers=df.c)
km$cluster

km$centers

fviz_cluster(km, data=df[,2:3],
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

# Exercício 12.12 ----

c.ac <- df %>%
  filter(item == c("A","C")) %>%
  select(!item) %>%
  summarise_all(list(mean))

c.bd <- df %>%
  filter(item == c("B","D")) %>%
  select(!item) %>%
  summarise_all(list(mean))

df.c2<- as.matrix(rbind(c.ac,c.bd))
df.c2

km2 <- kmeans(df[,2:3],centers=df.c2)
km2$cluster

km2$centers
fviz_cluster(km2, data=df[,2:3],
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())


# Exercíco 12.13 ----

x11 <- rev(c(5,-1,1,-3))
x21 <- rev(c(3,1,-2,-2))
df2 <- data.frame(item,x11,x21)

c.ab2 <- df2 %>%
  filter(item == c("A","B")) %>%
  select(!item) %>%
  summarise_all(list(mean))

c.cd2 <- df2 %>%
  filter(item == c("C","D")) %>%
  select(!item) %>%
  summarise_all(list(mean))

df.c3<- as.matrix(rbind(c.ab2,c.cd2))
df.c3

km3 <- kmeans(df2[,2:3],centers=df.c3)
km3$cluster

km3$centers
fviz_cluster(km2, data=df2[,2:3],
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

# Exercício 12.14 ----

# Item A
df14<- read.delim('Brands-of-Cereal-T11-9.dat.txt',sep=';',header = FALSE)
d14 <- dist(df14[,3:10],method="euclidean",diag=F)
d14

# Item B
simples14 <- hclust(as.dist(d14), method = "single")
plot(as.dendrogram(simples14), hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Ligação Simples")
completa14 <- hclust(as.dist(d14), method = "complete")
plot(as.dendrogram(completa14), hang = -1, cex.axis=1.2, cex.lab = 1.5, 
     xlab="Ligação Completa")

# Exercício 12.15 ----

c32.1 <- df32 %>%
  filter(V1 == c("1")) %>%
  select(!V1) %>%
  summarise_all(list(mean))

c32.2 <- df32 %>%
  filter(V1 == c("2")) %>%
  select(!V1) %>%
  summarise_all(list(mean))

df.c32<- as.matrix(rbind(c32.1,c32.2))
df.c32

km15_2 <- kmeans(df32[,2:3],2, iter.max = 1000)
km15_3 <- kmeans(df32[,2:3],3, iter.max = 1000)
km15_4 <- kmeans(df32[,2:3],4, iter.max = 1000)


df32.k <- cbind(df32,km15_2$cluster,km15_3$cluster,km15_4$cluster)
df32.k[1:6,]
plot(df32.k$V1,df32.k$V2,col=km15_2$cluster,pch=16,
     main="K-means, G=2",xlab="X1",ylab="X2")
plot(df32.k$V1,df32.k$V2,col=km15_3$cluster,pch=16,
     main="K-means, G=3",xlab="X1",ylab="X2")
plot(df32.k$V1,df32.k$V2,col=km15_4$cluster,pch=16,
     main="K-means, G=4",xlab="X1",ylab="X2")


# Exercício 11.32 ----

df32<- read.delim('file11_32.txt',sep=';',header = FALSE)
df32$V1 <- factor(df32$V1)


cov_pooled <- function(x, groups, ..., regularize = NULL) {
  assert_hyperSpec(x)
  validObject(x)

  if (!is.factor(groups)) {
    stop("groups must be a factor")
  }

  x <- x[!is.na(groups)]
  groups <- groups[!is.na(groups)]

  means <- aggregate(x, groups, "mean") # TODO: speed up?

  cov_p <- cov(x@data$spc - means@data$spc[as.numeric(groups), , drop = FALSE])

  # regularization
  if (is.null(regularize)) regularize <- 1e-5 * max(abs(cov_p))
  cov_p <- cov_p + diag(regularize, nrow(cov_p))

  # Return:
  list(COV = cov_p, means = means)}

## Item A

df32 %>% 
  ggplot(aes(x=V2, y=V3, color=V1)) +
    geom_point()

## Item B

means1 <- as.matrix(df32 %>%
  filter(V1 == 1) %>%
  select(V2,V3) %>%
  summarise_all(mean))

means2 <- as.matrix(df32 %>%
  filter(V1==2) %>%
  select(V2,V3) %>%
  summarise_all(mean))

xb1 <- means1
xb2 <- means2

group1 <- df32  %>%
  filter(V1==1) %>%
  select(V2,V3)

group2 <- df32  %>%
  filter(V1==2) %>%
  select(V2,V3)

al <- t(t(means1-means2)) %*% solve(cov_pooled(group1,group2))

m <- .5*(al%*%t(means1)+al%*%t(means2))

pop1 <- group1 %>%
  rowwise() %>%
  mutate(M = al %*% c(V2, V3)) %>%
  mutate(pop = ifelse(M > m,"p1","p2")) %>%
  pull()
pop1 <- factor(pop1)

pop2 <- group2 %>%
  rowwise() %>%
  mutate(M = al %*% c(V2, V3)) %>%
  mutate(pop = ifelse(M > m,"p1","p2")) %>%
  pull()
pop2 <- factor(pop2)

mc <- t(matrix(c(27,3,8,37),2,2))

lda <- lda(V1~V2+V3, data = df32,prior =c(.5,.5))

gldap <- predict(lda)
glctable <- table(df32$V1, gldap$class)
p <- (diag(prop.table(glctable,1)))
ptotal <- (sum(diag(prop.table(glctable))))

set.seed(M)
split <- sample.split(df32$V1, SplitRatio = 0.3) 
train <- subset(df32, split==T)
test <- subset(df32, split==F)

lda1 <- lda(V1~V2+V3, data = train,prior =c(.5,.5))

PT <- predict(lda1, newdata = test, type = "response")
glctable <- table(test$V1, PT$x >= 0.5)

## Item C

pop3 <- df32 %>%
  rowwise() %>%
  mutate(M = al %*% c(V2, V3)) %>%
  mutate(pop = ifelse(M > m,"p1","p2")) %>%
  pull()
pop3 <- factor(pop3)
pop3

## Item D

LDA <- lda(V1~., data = df32,prior=c(.75,.25))

LDAp1 <- predict(LDA)
LDAtable1 <- table(df32$V1, LDAp1$class)
p <- (diag(prop.table(LDAtable1,1)))
ptotal <- (sum(diag(prop.table(LDAtable1)))) 

partimat(V1~V2+V3, data=df32, method="lda", 
         plot.matrix = F, imageplot = T,prec=100)

set.seed(M)
split <- sample.split(df32$V1, SplitRatio = 0.3) 
train <- subset(df32, split==T)
test <- subset(df32, split==F)

lda1 <- lda(V1~V2+V3, data = train,prior =c(.75,.25))

PT <- predict(lda1, newdata = test, type = "response")
glctable <- table(test$V1, PT$x >= 0.5)
pred <- LDA %>%
  predict(df)
