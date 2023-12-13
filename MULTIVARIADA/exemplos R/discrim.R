
###
# ANALISE MULTIVARIADA
# Analise de Discriminantes e Classificacao
# Prof. George von Borries
#### Exemplos Notas de Aula ####

library(ggplot2)
library(MASS)
setwd("C:\\001-dados\\cursos")

#### (1) Salmao - J&W - Exemplo 11.8 ####

salmon <- read.table("Salmao-T11-2.dat",
                     header = F)
colnames(salmon) <- c("Origin", "Gender", "Freshwater", "Marine")
head(salmon)
dim(salmon)

salmon$Origin <- factor(salmon$Origin, level=c(1,2),
                        labels=c("Alaskan","Canadian"))
salmon$Gender <- factor(salmon$Gender, level=c(1,2),
                        labels=c("Female","Male"))
head(salmon)
table(salmon$Origin,salmon$Gender)

salaskan <- salmon[which(salmon[,1] == 'Alaskan'),]
scanadian <- salmon[which(salmon[,1] == 'Canadian'),]

c(mean(salaskan[,3]),mean(salaskan[,4]))
c(mean(scanadian[,3]),mean(scanadian[,4]))

(s1 <- cov(salaskan[,3:4]))
(s2 <-cov(scanadian[,3:4]))

(s.pon <- ((nrow(salaskan)-1)*s1 + (nrow(scanadian)-1)*s2)/(nrow(salmon)-2))

ggplot(salmon, aes(x=Freshwater,y=Marine, 
                   color=as.character(Origin))) +
  geom_point(shape = 16, size=2) +
  labs(title = "Salmon", xlab = "Freshwater",
       ylab = "Marine") +
  scale_color_manual(values = c("#EF6F6A","#1170AA")) +
  guides(colour = guide_legend(title = "Grupo")) +
  theme_classic() + 
  theme(
    axis.line = element_blank(),
    axis.title.x = element_text(face=2,
                                colour = "black",
                                size=12),
    axis.title.y = element_text(face=2,
                                colour = "black",
                                size=12),
    axis.text.y = element_text(face=2,
                               colour = "black",
                               size=10),
    axis.text.x = element_text(face=2,
                               colour = "black",
                               size=10),
    axis.ticks = element_blank(),
    legend.title = element_text(face=2, size = 12),
    legend.text = element_text(face=2),
    plot.title = element_text(hjust = 0.5, 
                              color = "black", face=2, size = 15),
    panel.background = element_rect(
      fill = "azure", color = "grey1"))

slda <- lda(Origin ~ Freshwater + Marine, 
            data=salmon, na.action = "na.omit", CV=F)

slda$prior
slda$means
slda$svd
slda$scaling
(mediapond <- slda$prior %*% slda$means)
(constante <- mediapond %*% slda$scaling) 
slda$scaling*sqrt(slda$svd) 
constante*sqrt(slda$svd)

salmonp <- predict(slda)
(ctable <- table(salmon$Origin, salmonp$class))
(diag(prop.table(ctable,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(ctable)))) # prop total de classf. correta 


salmon.new <- data.frame(cbind(c(70,120),c(420,380)))
colnames(salmon.new) <- c("Freshwater","Marine")
head(salmon.new)

psalmon.new <- predict(object=slda,newdata=as.list(salmon.new))
psalmon.new$class

plot(slda, dimen = 1)

library(klaR)
partimat(Origin ~ Marine + Freshwater, data=salmon,
         method="lda", main="Gráfico de Partição")


#### (2) Graduate Admission - J&W - Exemplo 11.11 ####

graduate <- read.table("C:/001-dados/cursos/Graduate-T11-6.dat",
                     header = F)
colnames(graduate) <- c("GPA", "GMAT", "Group")
head(graduate)
dim(graduate)

graduate$Group <- factor(graduate$Group, level=c(1,2,3),
                        labels=c("admit","no_admit","borderline"))
head(graduate)
table(graduate$Group)

(gqda <- qda(Group  ~ GPA + GMAT, 
             data=graduate, na.action = "na.omit"))

(gqdap <- predict(gqda))
(gqctable <- table(graduate$Group, gqdap$class))
(diag(prop.table(gqctable,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable)))) # prop total de classf. correta 

plot(graduate$GPA,graduate$GMAT,
     col = graduate$Group, pch=20, cex=1.5,
     xlim = c(2,4), ylim = c(300,700),
     main = "Graduate Admission",
     xlab = "GPA",
     ylab = "GMAT")

legend(x=2.1, y = 700, legend = c("Admit",
                                "No_admit",
                                "Borderline"),
       col = c("black", "red", "green"), 
       lty = c(0,0,0), lwd = c(1,1,1), pch = c(19, 19, 19))

graduate.new <- data.frame(GPA = 3.21, GMAT = 497)
(pgraduate.new <- predict(object=gqda,newdata=graduate.new))

partimat(Group ~ GPA + GMAT, data=graduate, method="qda")
