################################################
####       PROJEKT ZALICZENIOWY TNA2        #### 
####       Autor: Piotr Sadura 398376       ####
####       17 stycznia 2019 r.              ####
################################################


library("pscl")
library("readxl")
library("MASS")
library("stargazer")
library("mfx") # Biblioteka zosta³a usuniêta z repozytorium CRAN
#nale¿y pobraæ wersjê 1.1 i zainstalowaæ j¹ manualnie

setwd("C:/Users/Piotrek/OneDrive - hk sar baomin inc/Tworzenie narzêdzi analitycznych II/")
dane = read_xlsx("himalayan.xlsx",1)
View(dane)
dane = na.omit(dane)
View(dane)
head(dane)
#Model logitowy z procedur¹ od ogó³u do szczegó³u
logit1=glm(success~smtdays+camps+totmembers, data=dane, family=binomial(link="logit"))
summary(logit1)
logit2=glm(success~camps+totmembers, data=dane, family=binomial(link="logit"))
summary(logit2)
logit3=glm(success~totmembers, data=dane, family=binomial(link="logit"))
summary(logit3)

#Model probitowy z procedura od ogó³u do szczegó³u
probit1=glm(success~smtdays+camps+totmembers, data=dane, family=binomial(link="probit"))
summary(probit1)
probit2=glm(success~camps+totmembers, data=dane, family=binomial(link="probit"))
summary(probit2)

#Linktest
logit_final = glm(success~totmembers,data = dane, family = binomial(link = "logit"))
summary(logit_final)

probit_final = glm(success~camps+totmembers, data=dane, family=binomial(link="probit"))
summary(probit_final)

#wartosci dopasowane
logit_final$fitted.values
yhat = predict(logit_final)
yhat2 = yhat^2

probit_final$fitted.values
yhat = predict(probit_final)
yhat2 = yhat^2

#model pomocniczny
aux_reg_log = glm(dane$success~yhat+yhat2, family = binomial(link = "logit"))
summary(aux_reg_log)

aux_reg_prob = glm(dane$success~yhat+yhat2, family = binomial(link = "logit"))
summary(aux_reg_prob)

stargazer(logit1, logit2,logit3,  type = "text")
stargazer(probit1, probit2, type = "text")

BIC(logit3)
BIC(probit2)

#Efekty cz¹stkowe
#logitmfx(success~smtdays+totmembers+camps, data=dane, atmean=FALSE)
#logitmfx(success~totmembers+camps, data=dane, atmean=FALSE)
#logitmfx(success~totmembers, data=dane, atmean=FALSE)

probitmfx(success~smtdays+totmembers+camps, data=dane, atmean=FALSE)
probitmfx(success~totmembers+camps, data=dane, atmean=FALSE)


#Interpretacja efektów cz¹stkowych znajduje siê w podpunkcie WYNIKI

