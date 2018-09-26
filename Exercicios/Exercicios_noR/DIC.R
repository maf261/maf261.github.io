rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 4.1
alpha <- 0.05
#Variedades: A, B, C e D
t1 <- c(25,26,20,23,21)
t2 <- c(31,25,28,27,24)
t3 <- c(22,26,28,25,29)
t4 <- c(33,29,31,34,28)
tab <- matrix(c(t1,t2,t3,t4),5,4)
(n <- length(tab))
(I <- dim(tab)[2])
(J <- dim(tab)[1])
(bart1 <- mean(t1))
(bart2 <- mean(t2))
(bart3 <- mean(t3))
(bart4 <- mean(t4))
(Tott1 <- sum(t1))
(Tott2 <- sum(t2))
(Tott3 <- sum(t3))
(Tott4 <- sum(t4))
(mu <- mean(tab))
####################################
difTot <- matrix(NA,dim(tab)[1],dim(tab)[2])
for (j in 1:dim(tab)[2]) {
  for (i in 1:dim(tab)[1]) {
    difTot[i,j] <- ((tab[i,j]-mu)^2)
  }
  
  }
(SQTot <- sum(difTot))
mean(tab[,1])
####################################
difTrat <- matrix(NA,1,I)
for (i in 1:I) {
  difTrat[i] <- ((mean(tab[,i])-mu)^2)
}
(SQTrat <- 5*sum(difTrat))
####################################
difRes <- matrix(NA,dim(tab)[1],dim(tab)[2])
for (i in 1:dim(tab)[2]) {
  for (j in 1:dim(tab)[1]) {
    difRes[j,i] <- (tab[j,i]-mean(tab[,i]))^2
     }
}
(SQRes <- sum(difRes))
####################################
FV <- cbind("Trat","Res","Total")
GL <- cbind(I-1,I*(J-1),I*J-1)
SQ <- cbind(SQTrat,SQRes,SQTot)
QM <- cbind(SQTrat/GL[1],SQRes/GL[2],SQTot/GL[3])
(Fcal <- QM[1,1]/QM[1,2])
(Ftab <- qf(alpha,GL[1],GL[2],lower.tail = FALSE))
(pvalor <- pf(Fcal,GL[1],GL[2],lower.tail = FALSE))
RR <- "Rejeita-se H0 ao nível alpha de significancia"
RNR <- "Não rejeita-se H0 ao nível alpha de significancia"
dif <- "Pelo menos uma média difere das demais"
ndif <- "Todas as médias são iguais"
ifelse(Fcal>Ftab,RR,RNR)
ifelse(Fcal>Ftab,dif,ndif)
###########################################################
rm(list=ls())
library(ExpDes.pt)
library(readr)
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 4.1
## Dados de experimento em DIC. Fator Variedade de milho aplicada à 5 parcelas
## experimentais. A resposta é a produtividade ao final do experimento.

da <- read_delim("E:/Documentos/GitHub/MAF261/maf261.github.io/Exercicios/Exercicios_noR/Exer_4_1.txt", 
                 "\t")
#View(da)
str(da)
da$Var <- as.factor(da$Var)
is.factor(da$Var)
library(lattice)
#Analise grafica para avaliar normalidade e homocedasticidade
xyplot(Prod~Var, data=da, jitter.x=TRUE)
xyplot(Prod~reorder(Var,Prod), data=da, jitter.x=TRUE)
dotplot(Prod~Var, data=da, jitter.x=TRUE)
dotplot(Prod~reorder(Var,Prod), data=da, jitter.x=TRUE)
bwplot(Prod~Var, data=da, pch="|")
bwplot(Prod~reorder(Var,Prod), data=da, pch="|")

m0 <- lm(Prod~Var, data=da)
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

dic(da$Var,
  da$Prod,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = 'bartlett',
  sigT = 0.05,
  sigF = 0.05)

##---------------------------------------------------------------------
## Pacotes.
pkg <- c("lattice", "latticeExtra", "plyr", "reshape","doBy", "multcomp")
library(readr)
sapply(pkg, require, character.only=TRUE)
rm(list=ls())
if(!is.null(dev.list())) dev.off()
cat("\014")
#Exercicio 4.1
## Dados de experimento em DIC. Fator Variedade de milho aplicada à 5 parcelas
## experimentais. A resposta é a produtividade ao final do experimento.

da <- read_delim("E:/Documentos/GitHub/MAF261/maf261.github.io/Exercicios/Exercicios_noR/Exer_4_1.txt", 
                 "\t")
#View(da)
str(da)
da$Var <- as.factor(da$Var)
is.factor(da$Var)
xyplot(Prod~Var, data=da, jitter.x=TRUE)
## dotplot(Prod~Var, data=da, jitter.x=TRUE)
## bwplot(Prod~Var, data=da, pch="|")

X <- model.matrix(~Var, data=da)
## (X <- model.matrix(~0+Var, data=da))
## (X <- model.matrix(~-1+Var, data=da))
## (X <- model.matrix(~Var, data=da, contrasts=list(Var=contr.SAS)))
## (X <- model.matrix(~Var, data=da, contrasts=list(Var=contr.sum)))
## (X <- model.matrix(~Var, data=da, contrasts=list(Var=contr.helmert)))
X
## beta = (X'X)^{-1} X'y
XlX <- crossprod(X)
XlX
Xly <- crossprod(X, da$Prod)
Xly
beta <- solve(XlX, Xly)
beta
##---------------------------------------------------------------------
## Matriz de contrastes aplicados e as estimativas de médias, \mu_i =
## \mu+\tau_i.
Z <- cbind(1, contrasts(C(da$Var, contr="contr.treatment"))); Z
Z%*%beta

##---------------------------------------------------------------------

## Obtendo o quadro de anova matricialmente.
X <- model.matrix(~Var, data=da)
## Modelo: \mu+\alpha_i
Pmu <- X[,1]%*%solve(crossprod(X[,1]))%*%t(X[,1])
Pmualpha <- X%*%solve(crossprod(X))%*%t(X)
In <- diag(length(da$Var))
y <- da$Prod
## Somas de quadrados de Variedades e resídual.
t(y)%*%(Pmualpha-Pmu)%*%y
t(y)%*%(In-Pmualpha)%*%y
## Graus de liberdade de Variedades e resídual.
sum(diag(Pmualpha-Pmu))
sum(diag(In-Pmualpha))
## Usando a lm().
m0 <- lm(Prod~Var, data=da)
coef(m0)
anova(m0)
