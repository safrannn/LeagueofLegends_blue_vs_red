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
library(tree)
library(ISLR)
library(tidyr)
library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(1234)

#variables explored are first kill team, herald team, first blood team and first tower team
#if kill gained by blue team, 1; if kill gained by red team, 0
#clean data

blueTeam = subset(leagueData,
                  select = c(result,k,d,a,teamkills,heraldtime,teamdeaths,fb,fbassist,kpm,fd,fdtime,teamdragkills,oppdragkills,
                          herald,ft,firstmidouter,firsttothreetowers,teamtowerkills,opptowerkills,fbaron,fbarontime,
                          teambaronkills,oppbaronkills,visionwards,visiblewardclearrate,csdat10,gdat10,gdat15,xpdat10,totalgold,elders),
                  gamelength > 5 & player=='Team' & side =='Blue' & is.na(fb) == FALSE& is.na(kpm) == FALSE)
blueTeam$fdtime[is.na(blueTeam$fdtime)]= 0
blueTeam$fd[is.na(blueTeam$fdtime)]= -1
blueTeam$herald[is.na(blueTeam$herald)]=-1
blueTeam$heraldtime[is.na(blueTeam$heraldtime)]=-1
blueTeam$fbaron[is.na(blueTeam$fbaron)]=-1
blueTeam$fbarontime[is.na(blueTeam$fbarontime)]=-1
blueTeam$elders[is.na(blueTeam$elders)]=-1
blueTeam$visiblewardclearrate[is.na(blueTeam$visiblewardclearrate)]=-1
blueTeam$kda = (blueTeam$k+blueTeam$a)/max(1,blueTeam$d)

dim(blueTeam)

#create train and test sets
sample <- sample.int(n = nrow(blueTeam), size = floor(.8*nrow(blueTeam)), replace = F)
train <- blueTeam[sample, ]
test  <- blueTeam[-sample, ]


#decision tree(blue win is 1)

#10min
#create model
#try1
fit10 <- rpart(train$result~gdat10+xpdat10+csdat10+fb, data = train, method = 'class')
#result summary
summary(fit10)
#cross validation result
printcp(fit10)
#plot tree
rpart.plot(fit10, type=1,extra=104)

#predict model
predict10 <-predict(fit10, test, type = 'class')
#measure performance
table10 <- table(test$result, predict10)
table10

#15min
#create model
fit15 <- rpart(train$result~gdat10+xpdat10+gdat15+fd+heraldtime+fdtime, data = train, method = 'class')
#result summary
summary(fit15)
#cross validation result
printcp(fit15)
#root node error=0.48616
#51.38%

#plot tree
rpart.plot(fit15, type=1,extra=104)

#predict model
predict15 <-predict(fit15, test, type = 'class')
#measure performance
table15 <- table(test$result, predict15)
table15 #70.93%

#25min
#create model
fit25 <- rpart(train$result~gdat10+xpdat10+gdat15+fd+ft+fdtime+herald+heraldtime+fbaron+fbarontime,
               data = train, method = 'class')
#result summary
summary(fit25)
#cross validation result
printcp(fit25)

#plot tree
rpart.plot(fit25, type=1,extra=104)

#predict model
predict25 <-predict(fit25, test, type = 'class')
#measure performance
table25 <- table(test$result, predict25)
table25 #82.35%

#30min
#create model
fit30 <- rpart(train$result~gdat10+xpdat10+gdat15+fd+ft+fdtime+herald+heraldtime+fbaron+fbarontime++fbaron+fbarontime+teamdragkills+oppdragkills,
               data = train, method = 'class')
#result summary
summary(fit30)
#cross validation result
printcp(fit30)

#plot tree
rpart.plot(fit30, type=1,extra=104)

#predict model
predict30 <-predict(fit30, test, type = 'class')
#measure performance
table30 <- table(test$result, predict30)
table30 #84.08%

#35min
#create model
fit35 <- rpart(train$result~gdat10+totalgold+xpdat10+gdat15+fd+ft+fdtime+herald+heraldtime+fbaron+fbarontime++fbaron+fbarontime+teamdragkills+oppdragkills+teambaronkills+oppbaronkills+elders,
               data = train, method = 'class')
#result summary
summary(fit35)
#cross validation result
printcp(fit35)

#plot tree
rpart.plot(fit35, type=1,extra=104)

#predict model
predict35 <-predict(fit35, test, type = 'class')
#measure performance
table35 <- table(test$result, predict35)
table35 #88.81%

