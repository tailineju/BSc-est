require(broom)

tipo <- factor(rep(c("I", "II", "III"), 5))
tempo <- c(19,20,16,
           22,21,15,
           20,33,18,
           18,27,26,
           25,40,17)

#funcao implementada
pairwise.t.test(tempo, tipo, p.adjust.method ="none") %>% broom::tidy()

#calculos manuais
medias <- tapply(tempo, tipo, mean)

qmres <- summary(aov(tempo ~ tipo))[[1]]$`Mean Sq`[2]

#ni = nj = 5

t12 <- abs(medias[1] - medias[2])/sqrt(2*qmres/5)
t13 <- abs(medias[1] - medias[3])/sqrt(2*qmres/5)
t23 <- abs(medias[2] - medias[3])/sqrt(2*qmres/5)

#pvalores - DIFERENTES DO PAIRWISE.T.TEST
pt(t12, df = 12, lower.tail = FALSE)
pt(t13, df = 12, lower.tail = FALSE)
pt(t23, df = 12, lower.tail = FALSE)

