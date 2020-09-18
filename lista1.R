#lista 1
#install.packages("tidyverse")
library(tidyverse)
#1-a)
((8/22)^(-3))+((gamma(4)/22))

#b)
sqrt((125/102))+log10(33)

#c)
((100/22)^((-1)/4))+(exp(1))^((-2)/3)-(2/3)

#d)
choose(10,4)-factorial(7)

#e)
beta(2,3)+gamma(9)+abs(-5)-log(22)


#2-a)
seq(1:10)

#b)
seq(1,100,by=3)

#c)
seq(0,1000,by=50)

#d)
rep(c(1,22,5,3),c(6,2,2,4))

#e)
rep(seq(9,12),each=2,times=3)

#3-

p=c(rep(c(0,1),times=500))
q=c(1:1000)
r=c(rep(seq(10,50,by=10),times=200))
P=matrix(p,nrow=1000,ncol=1)
Q=matrix(q,nrow=1000)
R=matrix(r,nrow=1000)
A=cbind(P,Q,R)

#a)
A1=A[seq(1,1000,2),c(1,3)]

#b)
A[,2]=rep(seq(1,200),each=5)
A
#c)
A[seq(2,1000,2),]=rep(0,times=3)
A


#4-

library(dplyr)
install.packages("dplyr")

dados1=tibble(Nome=c("João","Maria","Ana","Fábia","Rodrigo","Renato","Luciana",
                     "Guilherme","Gabriel","Diogo"),
              Idade=c(27,22,21,37,29,27,21,18,19,25),
              Altura=c(1.83,1.53,1.79,1.58,1.65,1.70,1.51,1.66,1.72,1.83),
              Genero=c("M","F","F","F","M","M","F","M","M","M"),
              EstadoCivil=c("Solteiro","Solteiro","Casado","Solteiro","Casado",
                            "Solteiro","Solteiro","Solteiro","Casado","Casado"),
     Bairro=c("Icaraí","Ingá","Botafogo","Lagoa","Boa Viagem","Leblon","Leblon",
              "Ingá","Icaraí","Botafogo")
     )
dados1

dados2=tibble(Nome=c("João","Maria","Ana","Fábia","Rodrigo","Renato",
                      "Gabriel","Diogo"),       
               Peso=c(40,65,77,63,78,80,83,77)
               )        
  
dados3=tibble(Nome=c("João","Maria","Ana","Fábia","Rodrigo","Renato",
                       "Gabriel","Diogo","Vicente","Fernando"),
                Opniao=c("Contra","Contra","A Favor","Contra","A Favor","Contra",
                         "A Favor","A Favor","A Favor","A Favor")
              )  
  
  
dados4=tibble(Cidade=c("Niterói","Niterói","Niterói","Rio de Janeiro","Rio de Janeiro"
                       ,"Rio de Janeiro","Rio de Janeiro","Rio de Janeiro","Rio de Janeiro"
                       ,"Rio de Janeiro"), 
              Bairro=c("Ingá","Icaraí","Boa Viagem","Botafogo","Leblon",
                       "Copacabana","Ipanema","Lagoa","Gávea","São Francisco")
              )

#a)
dados5=inner_join(dados1,dados2,by="Nome")

#b)
dados.salvo=dados5[1:8,]
write_rds(dados.salvo,"Dadoslista1.rds")

#c)
dados6=semi_join(dados2,dados1,by="Nome")

#d)
dados6.salvo=dados6[1:8,]
write_excel_csv(dados6.salvo,"Dados6lista1.csv")

#e)
dados7=full_join(dados1,dados2,dados3,by="Nome")

#f)
#install.packages("haven")
dados7.salvo=dados7[1:10,]
write_dta(dados7.salvo,"Dados7lista1.dta")
library(haven)

#g)
dados8=inner_join(dados1,dados4,by="Bairro")

#h)
dados8.salvo=dados8[1:10,]
write_delim(dados8.salvo,"Dados8lista1.txt")

#i)
system.time (
  tempo<-read_csv("Dados6lista1.csv")
)

system.time (
  tempo1<-read_dta("Dados7lista1.dta")
)


system.time (
  tempo2<-read_table2("Dados8lista1.txt")
)

system.time (
  tempo3<-read_rds("Dadoslista1.rds")
)

#5-

setwd("C:\\Users\\Marianela hauret\\Dropbox\\Programas Rodolfo\\metodos computacionais\\lista1")
getwd()
install.packages("readxl")
library(readxl)
base=read_excel("Banco1.xls",sheet=1)

#a)

max(base$Altura)

#b)

aa=which.max(base$Altura)
   aa
base$Peso[which.max(base$Altura)]

#c)

base$Grupo[which.min(base$Peso)]

#d)

base$Grupo=ordered(base$Grupo,levels=c(1,2,3),labels=c("Placebo","Tratamento A","Tratamento B"))
base

#e)

base.salvar=base[1:50,]
write_excel_csv(base.salvar,"Banco1.csv")

#6-
library(readxl)
base1=read_csv("populacaototaljovem2010.csv")
base2=read_excel("dados2010.xls",sheet=1)

#a)
dim(base2)
base3=distinct(base2,codmun)

dim(base3)

#b)
grupE=group_by(base2,codmun,Estupros)
resultado=summarise(grupE,totalest=sum(Estupros,na.rm=TRUE))

#c)
base5=mutate(base2,idade.cat=c(Idade<18))
base5$idade.cat=ordered(base5$idade.cat,levels=c(TRUE,FALSE),labels=c(1,2))

AfogMenor18=group_by(base5,codmun,idade.cat=1,Afogamentos)
resultadoA=summarise(AfogMenor18,sum(Afogamentos))

AfogMaiorI18=group_by(base5,codmun,idade.cat=2,Afogamentos)
resultadoB=summarise(AfogMaiorI18,sum(Afogamentos))

#d)

#e)
base5=rename(base5, maior.idade = idade.cat)

#f)
base8=filter(base5,maior.idade==1)

#7-
tabela1=read_dta("basemae.dta")
#a)
tabela2=tabela1 %>% 
  mutate(UF=substring(micro_res,1,2),GR=substring(micro_res,1,1))

#b)
tabela3=tabela2 %>% 
  filter(UF==21,ano %in% c(2000,2001,2002,2003,2004,2005))  
  
tabela4=tabela3 %>% 
   group_by(ano) %>% 
   summarise(txMagressao=mean(txagressao,na.rm=TRUE),tx.media.estupro=mean(txestupro,na.rm=TRUE))

#c)
tabela5=tabela1 %>% 
  select(munic_nome_res,meso_res,meso_nome_res) %>% 
  group_by(meso_res,meso_nome_res) %>% 
  summarise(NumeMun=sum(length(munic_nome_res)))

#d)

tabela6=tabela1 %>% 
  group_by(ano) %>% 
  summarise(txMhomicidio=mean(txhomicidio,na.rm=TRUE),
            txMsuicidio=mean(txsuicidio,na.rm=TRUE))

#e)

tabela7=tabela2 %>% 
  group_by(ano,GR) %>%
  filter(!is.na(txhomicidio),!is.na(txsuicidio)) %>% 
  summarise(txMhomicidio=mean(txhomicidio,na.rm=TRUE),
            txMsuicidio=mean(txsuicidio,na.rm=TRUE))

# Refazendo ---------------------------------------------------------------

###Questão 1

#A)

(8/22)^(-3) + (gamma(4)/22)

#B)

sqrt(25/102) + log10(33)

#C)

(100/22)^(-1/4) + exp(-2/3) -2/3

#D)

choose(10,4) - factorial(7)

#E)

beta(2,3) + gamma(9) + abs(-5) + log(22)

### Questão 2

#A)

seq(1,10)

#B)

seq(1,100,3)

#C)

seq(0,1000,50)

#D)

rep(c(1,22,5,3),c(6,2,2,4))

#E)

rep(seq(9,12),each=2,times=3)

#### Questão 3

#Construindo a matriz A

A = matrix( c(rep(c(0,1),times = 500),
              seq(1,1000,1), 
              rep(seq(10,50,10),200)), 
            ncol = 3, 
            byrow = FALSE)

#A)

B = A[seq(1,1000,2),c(1,3)]

#B)

D = A
D[,1] = rep(seq(1,200,1),each = 5, times = 1)

#C)

E = A

E[seq(2,1000,2),] = rep(0,times = 3)
E
