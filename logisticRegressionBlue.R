#import data
leagueData2017 <- read.csv("data/2017.csv", header = TRUE)
leagueData2018summer <- read.csv("data/2018summer.csv", header = TRUE)
leagueData <- rbind(leagueData2017,leagueData2018summer)
leagueData2018spring <- read.csv("data/2018spring.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2018spring)
leagueData2018world <- read.csv("data/2018world.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2018world)
leagueData2019spring <- read.csv("data/2019spring.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2019spring)
leagueData2019summer <- read.csv("data/2019summer.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2019summer)
dim(leagueData)
names(leagueData)

#get libraries
library(ISLR)
library(tidyr)
library(dplyr)
library(ROCR)
library(Metrics)

set.seed(1234)

#variables explored are first kill team, herald team, first blood team and first tower team
#if kill gained by blue team, 1; if kill gained by red team, 0
#clean data
blueTeam = subset(leagueData,
                  select = c(result,k,d,a,teamkills,teamdeaths,fb,fbtime,fbassist,kpm,fd,fdtime,teamdragkills,oppdragkills,elders,
                             herald,heraldtime,ft,fttime,firstmidouter,firsttothreetowers,teamtowerkills,opptowerkills,fbaron,fbarontime,
                             teambaronkills,oppbaronkills,visionwards,visiblewardclearrate,csdat10,gdat10,gdat15,xpdat10,totalgold),
                  gamelength > 5 & player=='Team' & side =='Blue' & is.na(fb) == FALSE& is.na(kpm) == FALSE)
blueTeam$fdtime[is.na(blueTeam$fdtime)]= 0
blueTeam$fd[is.na(blueTeam$fdtime)]= -1
blueTeam$herald[is.na(blueTeam$herald)]=-1
blueTeam$heraldtime[is.na(blueTeam$heraldtime)]=-1
blueTeam$fbaron[is.na(blueTeam$fbaron)]=-1
blueTeam$fbarontime[is.na(blueTeam$fbarontime)]=-1
blueTeam$visiblewardclearrate[is.na(blueTeam$visiblewardclearrate)]=-1
blueTeam$kda = (blueTeam$k+blueTeam$a)/max(1,blueTeam$d)

#create train and test sets
sample <- sample.int(n = nrow(blueTeam), size = floor(.8*nrow(blueTeam)), replace = F)
train <- blueTeam[sample, ]
test  <- blueTeam[-sample, ]


#try different variables for logistic regression then use 10 fold cross validation to valid
#result==1 is blue team win, result===0 is red team win

#model at 10min
#create model
#try1 : AIC: 4219.5
#glm.fit10=glm(result~gdat10+xpdat10+csdat10+fb, data=train,family=binomial)
#try1 result: remove fb(z=1.384,pr=0.166,a=1) 
#             remove fbtime(z=-0.169,pr=0.866,a=1)
#             remove csdat10(zvalue = 1.604,pr=0.109,a=1)
#try2:
glm.fit10=glm(result~gdat10+xpdat10, data=train,family=binomial)
summary(glm.fit10)#AIC: 4218.8
#predict model
glm.pred10=ifelse(predict(glm.fit10,test,type = "response") > 0.5, "1", "0")
#model evaluation
table10 = table(glm.pred10,test$result)
table10#specificity=0.6548,sensitivity=0.6614,precision=0.7249
mean(glm.pred10==test$result)#66.44%
#cross validation
cv.error10 = cv.glm(train, glm.fit10, K=10)$delta[1]
1- cv.error10#78.70%
#AUC
auc(test$result,glm.pred10)

#model at 15min
#create model
#try1: AIC: 4575.4
#glm.fit15=glm(result~gdat10+xpdat10+gdat15+fd+fdtime+herald+heraldtime, data=blueTeam,family=binomial)
#try1 result: remove fdtime z=0.527,pr=0.59848,a=0.1
#             remove herald z=1.672,pr=0.09461,a=0.1  
#             remove heraldtime z=-1.60.pr=0.10948,a=1
#try2:
glm.fit15=glm(result~gdat10+gdat15+xpdat10+fd, data=train,family=binomial)
summary(glm.fit15)#AIC: 3674.5
#predict model
glm.pred15=ifelse(predict(glm.fit15,test,type = "response") > 0.5, "1", "0")
#model evaluation
table15 = table(glm.pred15,test$result)
table15#specificity=0.6957,sensitivity=0.7329,precision=0.7249
mean(glm.pred15==test$result)#71.51%
#cross validation
cv.error15 = cv.glm(train, glm.fit15, K=10)$delta[1]
1- cv.error15#82.25%
#AUC
auc(test$result,glm.pred15)

#model at 20min
#create model
#try1: glm.fit20=glm(result~gdat10+gdat15+xpdat10+fd+heraldtime+ft+fttime, data=blueTeam,family=binomial)
#try1 result: remove ft z=1.682,pr=0.092615,a=0.1
#             remove fttime z=0.935,pr=0.349706, a=1  
#try2(same model as 15 minutes)
glm.fit20=glm(result~gdat10+gdat15+xpdat10+fd+heraldtime, data=train,family=binomial)


#model at 25min
#create model
#glm.fit25=glm(result~gdat10+xpdat10+gdat15+fd+heraldtime+fbaron+fbarontime, data=blueTeam,family=binomial)
#AIC:3343.5
#try1 result: remove xpdat10 z=1.948,pr=0.05140,a=0.1
#try2:
glm.fit25=glm(result~gdat10+gdat15+fd+fbaron+fbarontime,data=train,family=binomial)
summary(glm.fit25)#AIC: 2666.1
#predict model
glm.pred25=ifelse(predict(glm.fit25,test,type = "response") > 0.5, "1", "0")
#model evaluation
table25 = table(glm.pred25,test$result)
table25#specificity=0.8301,sensitivity=0.8619,precision=0.8450
mean(glm.pred25==test$result)#84.66%
#cross validation
cv.error25 = cv.glm(train, glm.fit25, K=10)$delta[1]
1- cv.error25#88.12%
anova(glm.fit25, test = 'Chisq')
auc(test$result,glm.pred25)

#model at 30min
#create model
#try1: glm.fit30=glm(result~gdat10+gdat15+fd+heraldtime+fbaron+fbarontime, data=blueTeam,family=binomial)
#try1 result: remove gdat10 z=-0.980,pr=0.32711,a=1
#try2:
glm.fit30=glm(result~gdat15+fd+heraldtime+fbaron+fbarontime+teamdragkills+oppdragkills, data=train,family=binomial)
summary(glm.fit30)
#predict model
glm.pred30=ifelse(predict(glm.fit30,test,type = "response") > 0.5, "1", "0")
#model evaluation
table30 = table(glm.pred30,test$result)
table30#specificity=0.8794,sensitivity=0.8742,precision=0.8952
mean(glm.pred30==test$result)#87.66%
#cross validation
cv.error30 = cv.glm(train, glm.fit30, K=10)$delta[1]
1- cv.error30#90.20%
#AUC
auc(test$result,glm.pred30)


#model 35min and +
#create model
#try1: glm.fit15=glm(result~gdat15+fd+heraldtime+fbaron+fbarontime+teamdragkills+oppdragkills+teamdragkills+oppdragkills, data=blueTeam,family=binomial)
#try1 result: remove heraldtime z=-1.366,pr=0.1720,a=1
#             remove fbaron z=0.044,pr=0.9653,a=1 
#             remove fbarontime z=-1.942,pr=0.0522,a=0.1 
#             remove elders -1.800e-01  2.178e-01  -0.826 0.408671    a=1
#             remomove teamdragkills   4.945e-02  8.522e-02   0.580 0.561733 a=1
#try2:
glm.fit35=glm(result~gdat15+fd+oppdragkills+teambaronkills+oppbaronkills+totalgold, data=train,family=binomial)
summary(glm.fit35)
#predict model
glm.pred35=ifelse(predict(glm.fit35,test,type = "response") > 0.5, "1", "0")
#model evaluation
table35 = table(glm.pred35,test$result)
table35#specificity=0.9093,sensitivity=0.9172,precision=0.9192
mean(glm.pred35==test$result)#91.35%
#cross validation
cv.error35 = cv.glm(train, glm.fit35, K=10)$delta[1]
1- cv.error35#92.58%
#AUC
auc(test$result,glm.pred35)

