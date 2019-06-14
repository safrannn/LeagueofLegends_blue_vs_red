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

set.seed(1234)

#variables explored are first kill team, herald team, first blood team and first tower team
#if kill gained by red team, 1; if kill gained by red team, 0
#clean data
redTeam = subset(leagueData,
                  select = c(result,k,d,a,teamkills,teamdeaths,fb,fbtime,fbassist,kpm,fd,fdtime,teamdragkills,oppdragkills,elders,
                             herald,heraldtime,ft,fttime,firstmidouter,firsttothreetowers,teamtowerkills,opptowerkills,fbaron,fbarontime,
                             teambaronkills,oppbaronkills,visionwards,visiblewardclearrate,csdat10,gdat10,gdat15,xpdat10,totalgold),
                  gamelength > 5 & player=='Team' & side =='Red' & is.na(fb) == FALSE& is.na(kpm) == FALSE)
redTeam$fdtime[is.na(redTeam$fdtime)]= 0
redTeam$fd[is.na(redTeam$fdtime)]= -1
redTeam$herald[is.na(redTeam$herald)]=-1
redTeam$heraldtime[is.na(redTeam$heraldtime)]=-1
redTeam$fbaron[is.na(redTeam$fbaron)]=-1
redTeam$fbarontime[is.na(redTeam$fbarontime)]=-1
redTeam$visiblewardclearrate[is.na(redTeam$visiblewardclearrate)]=-1
redTeam$kda = (redTeam$k+redTeam$a)/max(1,redTeam$d)

#create train and test sets
sample <- sample.int(n = nrow(redTeam), size = floor(.8*nrow(redTeam)), replace = F)
train <- redTeam[sample, ]
test  <- redTeam[-sample, ]


#try different variables for logistic regression then use 10 fold cross validation to valid
#result==1 is red team win, result===0 is blue team win

#model at 10min
#create model
#try1 : glm.fit10=glm(result~gdat10+xpdat10+csdat10+fb+fbtime, data=train,family=binomial)
#AIC: 4212.6
#try1 result: remove fbtime(z=-0.528,pr=0.5975,a=1)
#             remove csdat10(zvalue = 0.966,pr=0.3339,a=1)
#             remove fb 1.313e-01  8.500e-02   1.545    0.122   a= 1
#try2:
glm.fit10=glm(result~gdat10+xpdat10, data=train,family=binomial)
summary(glm.fit10)#AIC: 4210.9
#predict model
glm.pred10=ifelse(predict(glm.fit10,test,type = "response") > 0.5, "1", "0")
#model evaluation
table10 = table(glm.pred10,test$result)
table10#specificity=0.6841,sensitivity=0.6196,precision=0.4426
mean(glm.pred10==test$result)#67.91%
#cross validation
cv.error10 = cv.glm(train, glm.fit10, K=10)$delta[1]
1- cv.error10#79.48%
#AUC
auc(test$result,glm.pred10)


#model at 15min
#create model
#try1: AIC: 4589.1
#glm.fit15=glm(result~gdat10+xpdat10+gdat15+fd+fdtime+herald+heraldtime, data=redTeam,family=binomial)
#try1 result: remove fdtime z=1.139,pr=0.254570,a=1
#             remove herald       1.278e-01  7.825e-02   1.633 0.102556,a=1
#             remove heraldtime   -9.300e-03  5.242e-03  -1.774 0.076066,a=0.1
#try2:
glm.fit15=glm(result~gdat10+gdat15+xpdat10+fd, data=train,family=binomial)
summary(glm.fit15)#AIC: 3677.9
#predict model
glm.pred15=ifelse(predict(glm.fit15,test,type = "response") > 0.5, "1", "0")
#model evaluation
table15 = table(glm.pred15,test$result)
table15#specificity=0.7664,sensitivity=0.7125,precision=0.6333
mean(glm.pred15==test$result)#74.69%
#cross validation
cv.error15 = cv.glm(train, glm.fit15, K=10)$delta[1]
1- cv.error15#82.44%
#AUC
auc(test$result,glm.pred15)


#model at 20min
#create model
#try1: AIC: 4592.2
#glm.fit20=glm(result~gdat10+gdat15+xpdat10+fd+heraldtime+ft+fttime, data=redTeam,family=binomial)
#try1 result: remove ft z=1.693,pr= 0.090509,a=0.1
#             remove fttime z=-1.693,pr=0.108022, a=1  
#             remove herald 1.278e-01  7.825e-02   1.633 0.102556,a=1
#try2(same model as 15 minutes)
glm.fit20=glm(result~gdat10+gdat15+xpdat10+fd+heraldtime, data=train,family=binomial)
summary(glm.fit20)#AIC: 3675.7



#model at 25min
#create model
#try1 result: remove -1.365e-02  6.528e-03  -2.091 0.036510 a=0.05
#try2:
glm.fit25=glm(result~gdat15+fd+fbaron+fbarontime+xpdat10+gdat10, data=train,family=binomial)
summary(glm.fit25)#AIC: 2620.4
#predict model
glm.pred25=ifelse(predict(glm.fit25,test,type = "response") > 0.5, "1", "0")
#model evaluation
table25 = table(glm.pred25,test$result)
table25#specificity=0.8561,sensitivity=0.8222,precision=0.7833
mean(glm.pred15==test$result)#74.69%
#cross validation
cv.error25 = cv.glm(train, glm.fit25, K=10)$delta[1]
1- cv.error25#88.59%
#AUC
auc(test$result,glm.pred25)


#model at 30min
#create model
#try1:glm.fit30=glm(result~gdat15+xpdat10+gdat10+heraldtime+fd+fbaron+fbarontime+teamdragkills+oppdragkills, data=train,family=binomial)
#try1 result: remove xpdat10 1.455e-04  8.312e-05   1.751 0.080004 . a=0.1
#             remove gdat10        -1.208e-04  7.341e-05  -1.645  0.09989 a=0.1
#try2
glm.fit30=glm(result~gdat15+fd+fbaron+fbarontime+teamdragkills+oppdragkills, data=train,family=binomial)
summary(glm.fit30)
#predict model
glm.pred30=ifelse(predict(glm.fit30,test,type = "response") > 0.5, "1", "0")
#model evaluation
table30 = table(glm.pred30,test$result)
table30#specificity=0.8764,sensitivity=0.8376,precision=0.8167
mean(glm.pred30==test$result)#86.10%
#cross validation
cv.error30 = cv.glm(train, glm.fit30, K=10)$delta[1]
1- cv.error30#90.23%
#AUC
auc(test$result,glm.pred30)


#model 35min and +
#create model
#try1: glm.fit35=glm(result~gdat15+fd+fbaron+fbarontime+teamdragkills+oppdragkills+teambaronkills+oppbaronkills+elders, data=redTeam,family=binomial)
#AIC: 2255.3
#try1 result: remove fbaron z=0.279,pr=0.779942,a=1 
#             remove fbarontime z=1.991,pr=0.046514,a=0.05 
#             remove teamdragkills  -6.371e-02  8.450e-02  -0.754  0.45089a=1
#             remove elders         -3.361e-01  2.111e-01  -1.593  0.11125    
#try2:
glm.fit35=glm(result~gdat15+totalgold+fd+oppdragkills+teambaronkills+oppbaronkills, data=train,family=binomial)
summary(glm.fit35)
#AIC: 1582.5
#predict model
glm.pred35=ifelse(predict(glm.fit35,test,type = "response") > 0.5, "1", "0")
#model evaluation
table35 = table(glm.pred35,test$result)
table35#specificity=0.9350,sensitivity=0.8673,precision=0.9083
mean(glm.pred35==test$result)#90.62%
#cross validation
cv.error35 = cv.glm(train, glm.fit35, K=10)$delta[1]
1- cv.error35#93.67%
#AUC
auc(test$result,glm.pred35)

