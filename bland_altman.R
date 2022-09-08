# carregar (e instalar) pacotes
library(nlme)
library(lme4)
library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
library(stargazer)
library(readxl)
library(tidyverse)
library(corrplot)

# ler o banco de dados em excel
dados <- read_excel("gestacao.xlsx")
View(dados)

# exibir os nomes das vari?veis do banco de dados
names(dados)

# carregar o banco de dados
attach(dados)

# armazenar o banco de dados no DF
dfo <- data.frame(dados)

#Teste de normalidade de shapiro
shapiro.test(dum)
shapiro.test(eco_1)

# separar as vari?veis do dataset

dum <- dfo$dum
length(dum)

eco_1 <- dfo$eco_1
length(eco_1)

# comando para realizar o teste t para amostras pareadas entre as variáveis dum e eco_1 
t.test(dum, eco_1, paired = T)

# comando para calcular o coeficiente de correlação entre as variáveis dum e eco_1 
correl <- cor.test(dum, eco_1) 
correl

# as 3 linhas a seguir servem para fazer o diagrama de dispersão, com o coeficiente de correlação 
plot(dum,eco_1,xlim=c(min(eco_1,dum),max(eco_1,dum)),ylim=c(min(eco_1,dum),max(eco_1,dum)))
abline(coef=c(0,1))
text(max(eco_1,dum),min(eco_1,dum),pos=2,
     paste("r=",round(correl$estimate[[1]],2),", IC95%=[",
           round(correl$conf.int[[1]],2),";",
           round(correl$conf.int[[2]],2),"]",sep=""))

#função para fazer o gráfico do Bland-Altman
B_A<-function(x,y){
  x.name<-deparse(substitute(x))
  y.name<-deparse(substitute(y))
  xy<-na.omit(cbind(x,y))
  n<-length(xy[,1])
  mean.xy <- (xy[,1]+xy[,2])/2
  dif.xy <- xy[,1]-xy[,2]
  dif.mean <- mean(dif.xy,na.rm=T)
  dif.sd <- sqrt(var(dif.xy, na.rm=T))
  LIC <- dif.mean - (2 * dif.sd)
  LSC <- dif.mean + (2 * dif.sd)
  Amplitude <- LSC-LIC
  ttest<-t.test(x, y, paired = T)
  limite.y <- max(abs(min(dif.xy,LIC)),abs(max(dif.xy,LSC))) 
  plot(mean.xy, dif.xy, pch = 16,
       xlab = paste0("Média de ",x.name," e ",y.name,sep=""),
       ylab = paste0("Diferença entre ",x.name," e ",y.name,sep=""),
       ylim = c(ifelse(min(dif.xy,LIC)<0,-limite.y,limite.y)-.1*limite.y,limite.y+.1*limite.y))
  
  abline(h = 0, lty = 1)
  abline(h = dif.mean, lty = 1,col=3)
  abline(h = LIC, lty = 2,col=3)
  abline(h = LSC, lty = 2,col=3)
  text(max(mean.xy,na.rm=T),ifelse(dif.mean>0,max(dif.mean,0)+0.02*Amplitude,min(dif.mean,0)+0.02*Amplitude),pos=2,paste("bias=",round(dif.mean,2),ifelse(ttest$p.value<0.05,"*","")),col=2)
  text(max(mean.xy,na.rm=T),LIC+.02*Amplitude,pos=2,paste("LIC=",round(LIC,2)),col=2)
  text(max(mean.xy,na.rm=T),LSC+.02*Amplitude,pos=2,paste("LSC=",round(LSC,2)),col=2)
}

par(mfrow=c(1,2), mai=c(1,1,0.6,0.4))  
plot(dum,eco_1,xlim=c(min(eco_1,dum),max(eco_1,dum)),ylim=c(min(eco_1,dum),max(eco_1,dum)))
abline(coef=c(0,1))
text(max(eco_1,dum),min(eco_1,dum),pos=2,
     paste("r=",round(correl$estimate[[1]],2),", IC95%=[",
           round(correl$conf.int[[1]],2),";",
           round(correl$conf.int[[2]],2),"]",sep=""))

B_A(dum, eco_1)




