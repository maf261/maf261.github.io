rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 7.1
alpha <- 0.05

###########################################################
rm(list=ls())
library(ExpDes.pt)
library(readr)
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 7.1
da <- read_table2("Exer_7_1.txt")
#View(da)
str(da)
da$Col <- as.factor(da$Col)
is.factor(da$Col)
da$Linha <- as.factor(da$Linha)
is.factor(da$Linha)
da$Trat <- as.factor(da$Trat)
is.factor(da$Trat)

#Explorando os dados

names(da)
summary(da)

attach(da)

plot(Resp ~ Col + Linha + Trat)

da.mt <- tapply(Resp, Trat, mean)
da.mt
da.ml <- tapply(Resp, Linha, mean)
da.ml
da.mc <- tapply(Resp, Col, mean)
da.mc

plot.default(Trat, Resp)
points(da.mt, pch="x", col=2, cex=1.5)

#Homocedasticidade, Normalidade e Independência

da.aov <- aov(Resp~Col+Linha+Trat)
residuos <- (da.aov$residuals)

par(mfrow=c(2,2))

plot(da$Trat,residuos)
title("Resíduos vs Trat \n Homocedasticidade")

preditos <- (da.aov$fitted.values)

plot(residuos,preditos)
title("Resíduos vs Preditos \n Independência")

qqnorm(residuos,ylab="Residuos", main=NULL)
qqline(residuos)
title("Grafico Normal de \n Probabilidade dos Resíduos")

par(mfrow=c(2,1))

respad <- (residuos/sqrt(anova(da.aov)$"Mean Sq"[4]))
boxplot(respad)
title("Resíduos Padronizados - outliers")

outlier<-c(max(respad),min(respad))
outlier

#Teste para Normalidade dos Resíduos
shapiro.test(residuos)

a <- dql(da$Trat,
  da$Linha,da$Col,da$Resp,
  quali = TRUE,
  mcomp = "tukey",
  sigT = 0.05,
  sigF = 0.05)

plotres(a)

