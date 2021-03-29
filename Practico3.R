##--------------------------------------Practico TRES- Castro, De Martini, Der Merdiguian 
setwd("C:/Users/juan_/Desktop/Econometría/Practico3")
dir()
library()
library(stargazer)
library(dplyr)
library(haven)
library(stargazer)
library(AER) 
library(fastDummies) 
rm(list=ls())
data=read_dta("QOB.dta")



#--------------------------------Punto a
groups.quarter <- group_by(data,qob)
summarise(groups.quarter, promedio_grupos = mean(educ))




#--------------------------------Punto b
data.dec <- subset(data, yob<40)
lm.fit <- lm(lwage ~ educ + ageq + ageqsq + married + race + smsa, data=data.dec)
summary(lm.fit)
stargazer(lm.fit, type="latex",
          column.labels = c("lm.fit"),
          dep.var.labels=c("lwage"), out="pooled.ols.txt")




#--------------------------------Punto d
data.dummies <- dummy_cols(data.dec, select_columns = "qob")
attach(data.dummies)




#--------------------------------Punto e
relevancia <- lm(data=data.dummies, educ ~ qob_1 + qob_2 + qob_3)
summary(relevancia) #Mirar test F (Test de Ef Conjto)



#--------------------------------Punto f
#First Stage
first.stage <- lm(educ~ageq + ageqsq + married + race + smsa + qob_1 + qob_2 + qob_3, data=data.dec)
summary(first.stage)
data.dec$educ_hat <- first.stage$fitted.values




#--------------------------------Punto g
iv.fit <- ivreg(lwage ~ educ + ageq + ageqsq + married + race + smsa |
                  ageq + ageqsq + married + race + smsa + qob_1 + qob_2 + qob_3, data=data.dummies)
summary(iv.fit)
#Test Sobre identificación + instrumentos debiles
summary(iv.fit, df = Inf , diagnostics = TRUE)




#--------------------------------Punto h
summary(first.stage) #Mirar F, debilidad de inst, tambn el R2 
#Miro Correlación
cor(educ, qob_1)
cor(educ, qob_2)
cor(educ, qob_3)
cor(educ, qob_4)




#--------------------------------Punto i
#Secod Stage
second.stage <- lm(lwage ~ educ_hat + ageq + ageqsq + married + race + smsa, data=data.dec)
summary(second.stage)
stargazer(lm.fit, first.stage,second.stage, iv.fit, type="text",
          dep.var.labels=c("lwage"), 
          out="models_2sls.txt")





