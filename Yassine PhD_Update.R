library(plm)
library(readxl)
library(clubSandwich)
library(lmtest)
library(performance)


Panel_data <- read_excel("C:/Users/LOKMAN/Desktop/Yassine PhD/Panel_data.xlsx", 
                         sheet = "Data_2")

data<-pdata.frame(Panel_data, index=c("ID", "Year"))
View(data)

form<-ROA ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO1 <- plm(form,data = data, 
                 model = "pooling",effect = "twoways")
summary(MCO1)

# Modele a effets fixes 
FE1 <- plm(form,data = data, 
                model = "within")
summary(FE1)

# Modele a effets aleatoires 
random <- plm(ROA ~ RSE+TAILLE+RISQUE, data = data, 
                random.method="swar",effect = "twoways")
summary(random)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE1,random, vcov = "vcovHC")


# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO1, effect="twoways", type="bp")

# Modele estime par la methode des MCO versus modele a effets fixes
pFtest(FE1, MCO1)




#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
bptest(ROA ~ RSE+TAILLE+RISQUE+factor(ID), data = Panel_data, studentize = F)


# Test d'autocorrelation
pdwtest(random, alternative="two.sided")
# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(random)



# 	Test de multicolinearite

check_collinearity(MCO1)


# 	Resultats du modele ajuste

coeftest(MCO1, vcov.=function(x) vcovHC(x, method="arellano",
                           type="HC0", cluster="group"))


#...........................Model 2.............

form2<-ROE ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO2 <- plm(form2,data = data, 
           model = "pooling",effect = "twoways")
summary(MCO2)


# Modele a effets fixes 
FE2 <- plm(form2,data = data, model = "within")
summary(FE2)


# Modele a effets aleatoires 
random2 <- plm(form2, data = data, random.method="swar"
               ,effect = "twoways")
summary(random2)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE2,random2, vcov = "vcovHC")



# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO2, effect="twoways", type="bp")

# Modele estime par la methode desMCO versus modele a effets fixes
pFtest(FE2, MCO2)

#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(ROE ~ RSE+TAILLE+RISQUE+factor(ID), data = Panel_data, studentize = F)

# Test d'autocorrelation
pdwtest(random2, alternative="two.sided")
# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(random2)
# 	Test de multicolinearite
library(performance)
check_collinearity(MCO2)

# 	Resultats du modele ajuste

coeftest(MCO2, vcov.=function(x) vcovHC(x, method="arellano",
                                        type="HC0", cluster="group"))

#...........................Model 3.............

form3<-ROS ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO3 <- plm(form3,data = data, 
            model = "pooling",effect = "twoways")
summary(MCO3)


# Modele a effets fixes 
FE3 <- plm(form3,data = data, 
           model = "within")
summary(FE3)


# Modele a effets aleatoires 
random3 <- plm(form3, data = data, 
           random.method="swar",effect = "twoways")
summary(random3)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE3,random3, vcov = "vcovHC")

# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO3, effect="twoways", type="bp")

# Modele estime par la methode des MCO versus modele a effets fixes
pFtest(FE3, MCO3)

#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(form3, data = data, studentize = F)

# Test d'autocorrelation
pdwtest(random3, alternative="two.sided")
# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(random3)
# 	Test de multicolinearite
library(performance)
check_collinearity(MCO3)



# 	Resultats du modele ajuste

coeftest(random3, vcov.=function(x) vcovHC(x, method="arellano",
                                        type="HC0", cluster="group"))

#...........................Model 4.............

form4<-ROCE ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO4 <- plm(form4,data = data, 
            model = "pooling",effect = "twoways")
summary(MCO4)


# Modele a effets fixes 
FE4 <- plm(form4,data = data, 
           model = "within")
summary(FE4)

# Modele a effets aleatoires 
random4 <- plm(form4, data = data, 
           random.method="swar",effect = "twoways")
summary(random4)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE4,random4, vcov = "vcovHC")


# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO4, effect="twoways", type="bp")

# Modele estime par la methode desMCO versus modele a effets fixes
pFtest(FE4, MCO4)

#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(form4, data = data, studentize = F)

# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(random4)

# 	Test de multicolinearite
library(performance)
check_collinearity(MCO3)



# 	Resultats du modele ajuste

coeftest(MCO4, vcov.=function(x) vcovHC(x, method="arellano",
                                           type="HC0", cluster="group"))



#...........................Model 5.............

form5<-MARGE_OPERA ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO5 <- plm(form5,data = data, 
            model = "pooling",effect = "twoways")
summary(MCO5)


# Modele a effets fixes 
FE5 <- plm(form5,data = data, 
           model = "within")
summary(FE5)


# Modele a effets aleatoires 
random5 <- plm(form5, data = data, 
           random.method="swar",effect = "twoways")
summary(random5)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE5,random5, vcov = "vcovHC")




# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO5, effect="twoways", type="bp")

# Modele estime par la methode desMCO versus modele a effets fixes
pFtest(FE5, MCO5)

#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(form5, data = data, studentize = F)

# Test d'autocorrelation
pdwtest(random5, alternative="two.sided")
# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(random5)

# 	Test de multicolinearite
library(performance)
check_collinearity(MCO3)



# 	Resultats du modele ajuste

coeftest(MCO5, vcov.=function(x) vcovHC(x, method="arellano",
                                        type="HC0", cluster="group"))



#...........................Model 6.............

form6<-MARGE_IBITDA ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO6 <- plm(form6,data = data, 
            model = "pooling",effect = "twoways")
summary(MCO6)


# Modele a effets fixes 
FE6 <- plm(form6,data = data, 
           model = "within")
summary(FE6)


# Modele a effets aleatoires 
random6 <- plm(form6, data = data, 
           random.method="swar",effect = "twoways")
summary(random6)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE6,random6, vcov = "vcovHC")


# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO6, effect="twoways", type="bp")

# Modele estime par la methode desMCO versus modele a effets fixes
pFtest(FE6, MCO6)

#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(form6, data = data, studentize = F)

# Test d'autocorrelation
pdwtest(FE6, alternative="two.sided")

# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(FE6)

# 	Test de multicolinearite
library(performance)
check_collinearity(MCO3)



# 	Resultats du modele ajuste

coeftest(FE6, vcov.=function(x) vcovHC(x, method="arellano",
                                        type="HC0", cluster="group"))

#...........................Model 7.............

form7<-EBE ~ RSE+TAILLE+RISQUE
# Modele de regression estime par la methode des MCO 
MCO7 <- plm(form7,data = data, 
            model = "pooling",effect = "twoways")
summary(MCO7)


# Modele a effets fixes 
FE7 <- plm(form7,data = data, 
           model = "within")
summary(FE7)


# Modele a effets aleatoires 
random7 <- plm(form7, data = data, 
           random.method="swar", effect = "twoways")
summary(random7)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE7,random7, vcov = "vcovHC")


# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO7, effect="twoways", type="bp")

# Modele estime par la methode desMCO versus modele a effets fixes
pFtest(FE7, MCO7)

#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(form7, data = data, studentize = F)



# Test d'autocorrelation
pdwtest(random7, alternative="two.sided")
# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(MCO7)

# 	Test de multicolinearite
library(performance)
check_collinearity(MCO3)



# 	Resultats du modele ajuste

coeftest(FE7, vcov.=function(x) vcovHC(x, method="arellano",
                                       type="HC0", cluster="group"))

#...........................Model 8.............

form8<-MBN ~ RSE+TAILLE+RISQUE

# Modele de regression estime par la methode des MCO 
MCO8 <- plm(form8,data = data, 
            model = "pooling",effect = "twoways")
summary(MCO8)


# Modele a effets fixes 
FE8 <- plm(form8,data = data, 
           model = "within")
summary(FE8)


# Modele a effets aleatoires 
random8 <- plm(form8, data = data, 
               random.method="swar",effect = "twoways")
summary(random8)

# Modele a effets fixes VS modele a effets aleatoires Haussman 
phtest(FE8,random8, vcov = "vcovHC")

# Modele estime par la methode des MCO versus modele a effets aleatoires
plmtest(MCO8, effect="twoways", type="bp")

# Modele estime par la methode des MCO versus modele a effets fixes
pFtest(FE8, MCO8)


#           Ajustement du modele choisi

# 	Test d'heteroscedasticite
library(lmtest)
bptest(form8, data = data, studentize = F)

# Test d'autocorrelation
pdwtest(random8, alternative="two.sided")
# Breusch-Godfrey/Wooldridge test for serial correlation
pbgtest(random8)
# 	Test de multicolinearite
library(performance)
check_collinearity(MCO8)

# 	Resultats du modele ajuste

coeftest(random8, vcov.=function(x) vcovHC(x, method="arellano",
                                           type="HC0", cluster="group"))


