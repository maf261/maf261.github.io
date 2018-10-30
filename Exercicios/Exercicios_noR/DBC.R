rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 4.1
alpha <- 0.05
###########################################################
rm(list=ls())
library(ExpDes.pt)
library(readr)
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 6.1
da <- read_table2("Exer_6_1.txt")
#View(da)
str(da)
da$Bloco <- as.factor(da$Bloco)
is.factor(da$Bloco)
da$Trat <- as.factor(da$Trat)
is.factor(da$Trat)
library(lattice)
#Analise grafica para avaliar normalidade e homocedasticidade dentro dos blocos
xyplot(Resp~Bloco, data=da, jitter.x=TRUE)
xyplot(Resp~reorder(Bloco,Resp), data=da, jitter.x=TRUE)
dotplot(Resp~Bloco, data=da, jitter.x=TRUE)
dotplot(Resp~reorder(Bloco,Resp), data=da, jitter.x=TRUE)
bwplot(Resp~Bloco, data=da, pch="|")
bwplot(Resp~reorder(Bloco,Resp), data=da, pch="|")

#Analise grafica para avaliar normalidade e homocedasticidade
xyplot(Resp~Trat, data=da, jitter.x=TRUE)
xyplot(Resp~reorder(Trat,Resp), data=da, jitter.x=TRUE)
dotplot(Resp~Trat, data=da, jitter.x=TRUE)
dotplot(Resp~reorder(Trat,Resp), data=da, jitter.x=TRUE)
bwplot(Resp~Trat, data=da, pch="|")
bwplot(Resp~reorder(Trat,Resp), data=da, pch="|")
attach(da)
plot(Resp~Bloco+Trat)
m0 <- lm(Resp~Trat+Bloco, data=da)
anova(m0)
summary(m0)

par(mfrow=c(2,2))
plot(m0)
layout(1)

##Plot 1 - Residuos vs Valores Ajustados

# O gráfico dos resíduos versus valores ajustados (valores preditos) é uma 
# das principais técnicas utilizadas para verificar as suposições dos 
# resíduos. Além da detecção de heteroscedasticidade, esse gráfico pode 
# indicar que não existe uma relação linear entra as variáveis explicativas 
# com a variável resposta por meio de alguma tendência nos pontos. Por 
# exemplo, se os pontos do gráfico formam uma parábola, é indicativo que 
# termos de segundo grau sejam necessários.
# 
# Para o diagnóstico de heteroscedasticidade, tentamos encontrar alguma 
# tendência no gráfico. Por isso, se os pontos estão aleatoriamente 
# distribuídos em torno do 0, sem nenhum comportamento ou tendência, temos 
# indícios de que a variância dos resíduos é homoscedástica. Já a presença 
# de "funil" é um indicativo da presença de heteroscedasticidade.

#Plot 2 - Locação - Escala

#Para obtenção dos resíduos padronizados fazemos uma mudança de escala 
#dos resíduos, tend uma linha horizontal temos que a variabilidade é
#constante

#Plot 3 - Normal Q-Q plot
#Pontos ao redor da linha significa normalidade

#Plot 4 - Mesma avaliação do plot 1, mais utilizado quando 
#utilizamos regressão linear

a <- dbc(da$Trat,
  da$Bloco,da$Resp,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar='oneillmathews',
  sigT = 0.05,
  sigF = 0.05)

plotres(a)

##---------------------------------------------------------------------
m1 <- aov(Resp~Trat+Bloco,data = da)
summary(m1)
TukeyHSD(m1)
library(agricolae)
tukey <- HSD.test(m1, "Trat",alpha=0.05)
tukey
plot(m1)
library(gplots)
if(!is.null(dev.list())) dev.off()
cat("\014")
plotmeans(Resp~Trat, xlab = "tratamentos", ylab = "Resp")
