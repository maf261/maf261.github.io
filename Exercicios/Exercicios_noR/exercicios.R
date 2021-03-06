library("ExpDes")
#Experimento Fatorial - Exercício 8.1 da Apostila 
#Fator 1 - Irrigação
Irrig<-gl(2,6,label=c(paste("A",0:1,sep="")))
#Fator 2 - Calagem
Cal<-rep(gl(2,3,label=c(paste("B",0:1,sep=""))),2)
#Variável Resposta - Dados
dados<-c(25,32,27,
         35,28,33,
         41,35,38,
         60,67,59)
#Tabela com os tratamentos e os dados
tab<-data.frame(Irrig,Cal,dados)
#Comando para rodar a Anova
fat2.crd(Irrig, Cal, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)


#Experimento Fatorial - Exercício 8.2 da Apostila 
#Fator 1 - Nitrogênio
Nit<-gl(2,10,label=c(paste("N",0:1,sep="")))
#Fator 2 - Fósforo
Fos<-rep(gl(2,5,label=c(paste("P",0:1,sep=""))),2)
dados<-c(10.5,11,9.8,11.2,9.9,
         11.2,11,10.4,13.1,10.6,
         11.5,12.4,10.2,12.7,10.4,
         14,14.1,13.8,13.5,14.2)
#Tabela com os tratamentos e os dados
tab<-data.frame(Nit,Fos,dados)
#Comando para rodar a Anova
fat2.crd(Nit, Fos, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)


#Experimento Fatorial - Exercício 8.3 da Apostila 
#Fator 1 - Ração
racao<-gl(2,12,label=c(paste("R",0:1,sep="")))
#Fator 2 - Ambiente à noite
luz<-rep(gl(2,6,label=c(paste("L",0:1,sep=""))),2)
#Variável Resposta - Dados
dados<-c(50,52,48,54,52,50,
         49,52,50,48,46,45,
         42,44,46,43,44,45,
         40,40,38,39,41,43)
#Tabela com os tratamentos e os dados
tab<-data.frame(racao,luz,dados)
#Comando para rodar a Anova
fat2.crd(racao, luz, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)


#Experimento Fatorial - Exercício 8.10 da Apostila 
#Fator 1 - B
b<-gl(3,12,label=c(paste("B",1:3,sep="")))
#Fator 2 - A
a<-rep(gl(4,3,label=c(paste("A",1:4,sep=""))),3)
#Variável Resposta - Dados
dados<-c(12,14,16,
         15,17,18,20,21,23,23,24,26,
         18,17,20,22,23,23,25,26,28,29,30,32,
         22,21,20,30,31,32,29,32,32,34,35,37)
#Tabela com os tratamentos e os dados
tab<-data.frame(b,a,dados)
#Comando para rodar a Anova
fat2.crd(b, a, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)


#Experimento Fatorial - Exercício 8.17 da Apostila 
#Fator 1 - A
a<-gl(2,9,label=c(paste("A",1:2,sep="")))
#Fator 2 - B
b<-rep(gl(3,3,label=c(paste("B",1:3,sep=""))),2)
#Variável resposta - dados
dados<-c(12,14,16,15,17,18,12,11,13,
         14,13,16,11,12,11,12,12,13)
#Comando para rodar a Anova
fat2.crd(a, b, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)


#Experimento Fatorial - Exercício 8.19 da Apostila 
#Fator 1 - Tipos de Colhetadeira
t<-gl(2,15,label=c(paste("T",1:2,sep="")))
#Fator 2 - Horários de Colheita
h<-rep(gl(3,5,label=c(paste("H",1:3,sep=""))),2)
#Variável Resposta - dados
dados<-c(35,40,45,49,39,
         43,41,47,38,48,
         52,57,58,56,59,
         54,58,56,61,59,
         67,59,62,65,64,
         71,73,74,77,75)
#Tabela com os niveis dos fatores e os dados
tab<-data.frame(t,h,dados)
#Comando para rodar a Anova
fat2.crd(t, h, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)



#Experimento Fatorial - Exercício 8.23 da Apostila 
#Fator 1 - Suplementos Minerais
a<-gl(3,8,label=c(paste("A",1:3,sep="")))
#Fator 2 - Suplementos Vegetais
b<-rep(gl(2,4,label=c(paste("B",1:2,sep=""))),3)
#Variável Resposta - dados
dados<-c(35.2,36,35,35.4,
         32.8,34.6,36.7,35.2,
         34.7,36.3,35.1,36.4,
         28.6,31.1,29,28.6,
         33.8,29.4,28.8,29.2,
         30.8,31.4,32.8,31.3)
#Tabela com os niveis dos fatores e os dados
tab<-data.frame(a,b,dados)
#Comando para rodar a Anova
fat2.crd(a, b, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)



#Experimento Fatorial - Exercício 8.25 da Apostila 
#Fator 1 - Recipiente
rec<-gl(3,8,label=c(paste("R",1:3,sep="")))
#Fator 2 - Espécie
esp<-rep(gl(2,4,label=c(paste("E",1:2))),3)
#Variável Resposta - dados
dados<-c(26.2,26,25,25.4,
         24.8,24.6,26.7,25.2,
         25.7,26.3,25.1,26.4,
         19.6,21.1,19,18.6,
         22.8,19.4,18.8,19.2,
         19.8,21.4,22.8,21.3)
#Tabela com os niveis dos fatores e os dados
tab<-data.frame(rec,esp,dados)
#Comando para rodar a Anova
fat2.crd(rec, esp, dados, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("F1", "F2"), sigT = 0.05, sigF = 0.05)

#################################

#-----------------------------------------------------------------
# Dados referentes a um experimento no Delineamento Inteiramente -
# Casualizado, no esquema fatorial 3 x 2, para testar os efeitos -
# de três 3 Recipientes para produção de dados e 2 espécies de   -
# eucaliptos, quanto ao desenvolvimento das dados. Esse exemplo  -
# é apresentado livro de Banzatto & Kronka (1995), página 132.   -
#-----------------------------------------------------------------
rm(list=ls())

(croqui = expand.grid(rep=1:4, Espec=c("E1","E2"), Recip=c("R1","R2",'R3')))

alturas = c(26.2, 26.0, 25.0, 25.4, 
            24.8, 24.6, 26.7, 25.2,
            25.7, 26.3, 25.1, 26.4, 
            19.6, 21.1, 19.0, 18.6,
            22.8, 19.4, 18.8, 19.2, 
            19.8, 21.4, 22.8, 21.3)

(dados = data.frame(croqui, resp=alturas))

(dados$trat = factor(rep(c('T1','T2','T3','T4','T5','T6'), each=4)))
# ou
(dados$Trat = with(dados, interaction(Espec, Recip)))

head(dados)
tail(dados)
str(dados)

#---------------------------
# Estatísticas descritivas -
#---------------------------
summary(dados)

(médias.trat = with(dados, tapply(resp, trat, mean)))
(médias.rec = with(dados, tapply(resp, Recip, mean)))
(médias.esp = with(dados, tapply(resp, Espec, mean)))

(médias = with(dados, tapply(resp, list(Recip, Espec), mean)))
(variâncias = with(dados, tapply(resp, list(Recip, Espec), var)))
(desvios = with(dados, tapply(resp, list(Recip, Espec), sd)))

#-----------------------
# Gráficos descritivos -
#-----------------------
par(mai=c(1, 1.1, .2, .2))
with(dados, plot.default(resp ~ trat, las=1, xlab='Tratamentos', ylab='', 
                         col='blue', bty='l'))
mtext('Alturas (cm)', side=2, line=3.5)
points(médias.trat, pch="T", col='red', cex=1.5)

with(dados, plot.default(Recip, resp, las=1, xlab='Recipientes', ylab='', 
                         col='blue', bty='l'))
mtext('resp médias (cm)', side=2, line=3.5)
points(médias.rec, pch="R", col='red', cex=1.5)

with(dados, plot.default(Espec, resp, las=1, xlab='Espécies', ylab='', 
                         col='blue', bty='l'))
mtext('resp médias (cm)', side=2, line=3.5)
points(médias.esp, pch="E", col='red', cex=1.5)


# Gráfico de caixas
par(mai=c(1, .8, .2, .2))
#
# Tratamentos
with(dados, boxplot(resp ~ trat, names=c("T1", "T2", "T3", "T4", 'T5', 'T6'),
                    ylab="Alturas (cm)", xlab="Tratamentos", las=1, 
                    col='LightYellow'))
points(médias.trat, pch="+", col=2, cex=1.5)

# Espécies
with(dados, boxplot(resp ~ Espec, names=c("E1", "E2"), ylab="Alturas (cm)", 
                    xlab="Espécies", las=1, col='LightYellow'))
points(médias.esp, pch="+", col=2, cex=1.5)

# Recipientes
with(dados, boxplot(resp ~ Recip, names=c("R1", "R2", "R3"), ylab="Alturas (cm)", 
                    xlab="Recipientes", las=1, col='LightYellow'))
points(médias.rec, pch="+", col=2, cex=1.5)


#-------------------------------------------------
# Visualização de todas as médias do experimento -
#           em relação à média geral             -
#-------------------------------------------------
par(mai=c(1, 1, .2, .2))
plot.design(dados[ , c(2,3,4,6)], xlab='Fatores', ylab="", las=1, bty='l', col='blue')
mtext('Alturas (cm)', side=2, line=3.5)


#--------------------------------------------------------------
# Em experimentos fatoriais é importante verificar se existe  -
# interação entre os fatores. Inicialmente vamos fazer isto   -
# graficamente e mais a frente faremos um teste formal para   -
# presença de interação. Os comandos a seguir são usados para -
# produzir os gráficos.                                       -
#--------------------------------------------------------------
par(mai=c(1, 1, .2, .2))
with(dados, interaction.plot(Recip, Espec, resp, las=1, xlab='Recipientes',
                             ylab='Alturas médias (cm)', col=c('red','blue'), 
                             bty='l', trace.label=deparse(substitute(Espécies)), 
                             lwd=2.5))

with(dados, interaction.plot(Espec, Recip, resp, las=1, xlab='Espécies',
                             ylab='resp médias (cm)', col=c('red','blue'), 
                             bty='l', trace.label=deparse(substitute(Recipientes)), 
                             lwd=2.5))

#-----------------------------------------------------------------
# Seguindo o modelo adequado, o análise de variância para este   -
# experimento inteiramente casualizado em esquema fatorial pode  -
# ser obtida com o comando:                                      -
#-----------------------------------------------------------------
mod.1 = with(dados, aov(resp ~ trat))
summary(mod.1)
anova(mod.1)

mod.2 = with(dados, aov(resp ~ Recip + Espec + Recip*Espec))
summary(mod.2)

#--------------------------------------------------------------
# Entretanto o comando acima pode ser simplificado produzindo -
# os mesmos resultados com o comando:                         -
#--------------------------------------------------------------
mod.2 = with(dados, aov(resp ~ Recip*Espec))
summary(mod.2)

(médias.fat = model.tables(mod.2, ty="means"))


#------------------------------
# Verificação de pressupostos -
#------------------------------
#
# Normalidade dos erros
plot(mod.2, which=c(2:2), pch=19, col='red', las=1)

shapiro.test(mod.2$res)

# Homogeneidade das variâncias
with(dados, bartlett.test(mod.2$res ~ Trat))

# Independência dos erros
with(dados, plot(mod.2$res, las=1, pch=20, col='red', ylab='Resíduos'))


#--------------------------------------------------------------
# Quando a interação entre os fatores é significativa podemos -
# desdobrar os graus de liberdade de um fator dentro de cada  -
# nível do outro. A forma de fazer isto no R é reajustar o    -
# modelo utilizando a notação / que indica efeitos aninhados. -
# Desta forma podemos desdobrar os efeitos de espécie dentro  -
# de cada Recipiente e vice versa conforme mostrado a seguir: -
#--------------------------------------------------------------

#-------------------------------------
# Espécies dentro de cada Recipiente -
#-------------------------------------
fat.esprec = with(dados, aov(resp ~ Recip/Espec))
summary(fat.esprec, split=list("Recip:Espec"=list(r1=1,r2=2,r3=3)))

#--------------------------------------
# Recipientes dentro de cada Espécie  -
#--------------------------------------
fat.recesp = with(dados, aov(resp ~ Espec/Recip))
summary(fat.recesp, split=list("Espec:Recip" = list(e1=c(1,3), e2=c(2,4))))

#----------------------------
# Usando o pacote ExpDes.pt -
#----------------------------
require(ExpDes.pt)

with(dados, fat2.dic(Recip, Espec, resp, quali=c(TRUE,TRUE), mcomp="tukey",
                     fac.names=c("Recipientes", "Espécies")))


#----------------------------
# Usando o pacote easyanova -
#----------------------------
require(easyanova)

# Fator 1 - Espécie
# Fator 2 - Recipientes

ea2(dados[ , c(2,3,4)], design=1)

qf(0.05,1,8,lower.tail = F)
