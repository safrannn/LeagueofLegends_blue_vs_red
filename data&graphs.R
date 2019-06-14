#import data
leagueData2017 <- read.csv("final project/data/2017.csv", header = TRUE)
leagueData2018summer <- read.csv("final project/data/2018summer.csv", header = TRUE)
leagueData <- rbind(leagueData2017,leagueData2018summer)
leagueData2018spring <- read.csv("final project/data/2018spring.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2018spring)
leagueData2018world <- read.csv("final project/data/2018world.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2018world)
leagueData2019spring <- read.csv("final project/data/2019spring.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2019spring)
leagueData2019summer <- read.csv("final project/data/2019summer.csv", header = TRUE)
leagueData <- rbind(leagueData,leagueData2019summer)
names(leagueData)

#get libraries
library(ggplot2)
library(formattable)
library(reshape2)
library(ggpubr)
library(cowplot)
#clean data
#remove if play time is less than 5 min
#games under 5 mins possibly caused by insufficient number of players
#players
leagueData1 = subset(leagueData,gamelength > 5 & player!='Team')
#Teams
leagueDataTeam = subset(leagueData,gamelength > 5 & player=='Team')
blue = subset(leagueData1,side=='Blue')
red = subset(leagueData1,side=='Red')
blueWin = subset(leagueData1,side=='Blue' & result==1)
redWin = subset(leagueData1,side=='Red' & result==1)
blueTeam = subset(leagueDataTeam,side=='Blue')
redTeam = subset(leagueDataTeam,side=='Red')
blueTeamWin = subset(leagueDataTeam,side=='Blue' & result==1)
redTeamWin = subset(leagueDataTeam,side=='Red' & result==1)

#get win rate blue side vs red side
wins <- data.frame(side=c("Blue", "Red"),
                   win=c(nrow(blueWin),nrow(redWin)),
                   rate=c(percent(nrow(blueWin)*2/nrow(leagueData1)),percent(nrow(redWin)*2/nrow(leagueData1))))
head(wins)
ggplot(data=wins,aes(x=side,y=win,fill=side)) + 
      geom_bar(stat="identity",width = 0.5)+ 
      geom_text(aes(label=rate), vjust=4,color='white',size=10)+
      theme_minimal()+scale_fill_manual(values = c("Blue" = "#66ccff","Red" = "#FF9999"))+
      labs(title="win rate")+
      theme(text = element_text(size=30))
                                                                                                                                                                                                                                                                                      
#wins and gamelength
blueTeamDragon = subset(leagueDataTeam,side=='Blue' & result==1)#378
redTeamDragon = subset(leagueDataTeam,side=='Red' & result==1)  #386
plot(data.frame(ftnumber=cut(blueTeamDragon[['gamelength']], breaks=seq(4,60,by=1))),
     xlab='minute',ylab='wins',
     main="wins and gamelength",cex.main=1.5,
     ylim=range(0,250),
     col='blue')
par(new=TRUE)
plot(data.frame(ftnumber=cut(redTeamDragon[['gamelength']], breaks=seq(4,60,by=1))),
     xaxt="n",yaxt="n",
     ylim=range(0,250),
     col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))

#Dragons
#Blue team have a higher win raate after getting the first dragon
#Though blue side will have a bigger advantage in early game getting the first dragon
#Red side is more likely to prevent them having this advantage
#more analysis can be done with player data depending on their position

#first Dragon taken by which team
bluefd = nrow(blueTeam[blueTeam$fd == 1,])          #684
redfd = nrow(redTeam[redTeam$fd == 1,])             #692
bluefdw = nrow(blueTeamWin[blueTeamWin$fd == 1,])   #431
redfdw = nrow(redTeamWin[redTeamWin$fd == 1,])      #371
fdTotal = bluefd + redfd
fdwTotal = bluefdw + redfdw
fd = data.frame(type=c('all','win team'),
                blue = c(round(100*bluefd/fdTotal,2),round(100*bluefdw/fdwTotal,2)),
                red = c(round(100*redfd/fdTotal,2),round(100*redfdw/fdwTotal,2)))
fd2 = melt(fd, id.vars='type')
ggplot(data=fd2,aes(x=type,y=value,fill=variable)) + 
  geom_bar(stat="identity", position = 'dodge',width=0.5)+
  scale_fill_manual("value", values = c("blue" = "#66ccff","red" = "#FF9999"))+
  theme_minimal()+
  labs(title="first dragon kills",y='%',x='')+
  theme(text = element_text(size=30))+
  geom_text(aes(label=value), position = position_dodge(width = 0.5),vjust=3,color='white',size=10)
  
#first dragon kill time 
blueTeamDragon = subset(leagueDataTeam,side=='Blue' & fd==1)#378
redTeamDragon = subset(leagueDataTeam,side=='Red' & fd==1)  #386
plot(data.frame(ftnumber=cut(blueTeamDragon[['fdtime']], breaks=seq(4,23,by=1))),
     xlab='minute',ylab='first dragon time',
     main="First Dragon time",cex.main=1.5,
     ylim=range(0,100),
     col='blue')
par(new=TRUE)
plot(data.frame(ftnumber=cut(redTeamDragon[['fdtime']], breaks=seq(4,23,by=1))),
     xaxt="n",yaxt="n",
     ylim=range(0,100),
     col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))

#Total kills
blueteamdragkills = format(mean(blue[['teamdragkills']],na.rm = TRUE),digit=4) #2.18
redteamdragkills = format(mean(red[['teamdragkills']],na.rm = TRUE),digit=4)   #2.03
teamdragkills= data.frame(side=c("Blue", "Red"),
                          dragkills=c(blueteamdragkills,redteamdragkills))
ggplot(data=teamdragkills,aes(x=side,y=dragkills,fill=side)) + 
  geom_bar(stat="identity",width = 0.5)+ 
  theme_minimal()+
  scale_fill_manual("Side", values = c("Blue" = "#66ccff","Red" = "#FF9999"))+
  labs(title="total dragon kills")+
  theme(text = element_text(size=30))+
  geom_text(aes(label=dragkills), vjust=4,color='white',size=10)


#Rift Herald taken by which team
#Herald might not be killed for the whole game
blueTeamHerald = subset(blueTeam,herald==1)               #461
redTeamHerald = subset(redTeam,herald==1)                 #298
blueTeamHeraldWin = subset(blueTeamWin,herald==1)               #283
redTeamHeraldWin = subset(redTeamWin,herald==1)                 #171
blueherald = nrow(blueTeamHerald)  
redherald = nrow(redTeamHerald)     
heraldTotal = blueherald + redherald
blueheraldw = nrow(blueTeamHeraldWin)   
redheraldw = nrow(redTeamHeraldWin)      
herald = data.frame(type=c('all','win team'),
                blue = c(round(100*blueherald/heraldTotal,2),round(100*blueheraldw/heraldTotal,2)),
                red = c(round(100*redherald/heraldTotal,2),round(100*redheraldw/heraldTotal,2)))
herald2 = melt(herald, id.vars='type')
ggplot(data=herald2,aes(x=type,y=value,fill=variable)) + 
  geom_bar(stat="identity", position = 'dodge',width=0.5)+
  scale_fill_manual("value", values = c("blue" = "#66ccff","red" = "#FF9999"))+
  theme_minimal()+
  labs(title="Rift Herald",y='%',x='')+
  theme(text = element_text(size=30))+
  geom_text(aes(label=value), position = position_dodge(width = 0.5),vjust=3,color='white',size=10)

#rift herald taken time
plot(data.frame(number=cut(blueTeamHerald[['heraldtime']], breaks=seq(9,21,by=1))),
     main="Rift Herald time",cex.main=1.5,
     ylim=range(0,150),
     xlab='minute',ylab='first tower time',
     col='blue')
par(new=TRUE)
plot(data.frame(number=cut(redTeamHerald[['heraldtime']], breaks=seq(9,21,by=1))),
     xaxt="n",yaxt="n",
     ylim=range(0,150),
     col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))


#First blood taken by which team
bluefb = nrow(blueTeam[blueTeam$fb == 1,])   #735
redfb = nrow(redTeam[redTeam$fb == 1,])      #641 
fbTotal = bluefb + redfb
bluefbw = nrow(blueTeamWin[blueTeamWin$fb == 1,])   #436
redfbw = nrow(redTeamWin[redTeamWin$fb == 1,])      #325 
fb = data.frame(type=c('all','win team'),
                blue = c(round(100*bluefb/fbTotal,2),round(100*bluefbw/fbTotal,2)),
                red = c(round(100*redfb/fbTotal,2),round(100*redfbw/fbTotal,2)))
fb2 = melt(fb, id.vars='type')
ggplot(data=fb2,aes(x=type,y=value,fill=variable)) + 
  geom_bar(stat="identity", position = 'dodge',width=0.5)+
  scale_fill_manual("value", values = c("blue" = "#66ccff","red" = "#FF9999"))+
  theme_minimal()+
  labs(title="first blood",y='%',x='')+
  theme(text = element_text(size=30))+
  geom_text(aes(label=value), position = position_dodge(width = 0.5),vjust=3,color='white',size=10)


#First blood assist rate
bluefbAssist = nrow(leagueData1[leagueData1$fbassist == 1&leagueData1$side == 'Blue',])
redfbAssist = nrow(leagueData1[leagueData1$fbassist == 1&leagueData1$side == 'Red',])           
fbAssist = data.frame(side=c("Blue", "Red"),
                    fbAssist=c(bluefbAssist,redfbAssist),
                    rate=c(format(bluefbAssist/bluefb,digit=4),format(redfbAssist/redfb,digit=4)))
ggplot(data=fbAssist,aes(x=side,y=fbAssist,fill=side)) + 
      geom_bar(stat="identity",width = 0.5)+ 
      geom_text(aes(label=rate), vjust=4,color='white',size=10)+
      theme_minimal()+scale_fill_manual("Side", values = c("Blue" = "#66ccff","Red" = "#FF9999"))+
      labs(title="first blood assist")+
      theme(text = element_text(size=30))

#First blood time
dbluefb = subset(leagueData1,fb==1&side=='Blue')
dredfb = subset(leagueData1,fb==1&side=='Red')
bluefbtime = mean(dbluefb[['fbtime']],na.rm=TRUE) #6.07 minute
redfbtime = mean(dredfb[['fbtime']],na.rm=TRUE)   #6.06 minute
#Positions Blue
bluefbPosition = c(nrow(dbluefb[dbluefb$position == 'Top',]),
               nrow(dbluefb[dbluefb$position == 'Jungle',]),
               nrow(dbluefb[dbluefb$position == 'Middle',]),
               nrow(dbluefb[dbluefb$position == 'ADC',]),
               nrow(dbluefb[dbluefb$position == 'Support',]))
dbluefbAssist = subset(leagueData1,fbassist==1&side=='Blue')
bluefbAssistPosition= c(nrow(dbluefbAssist[dbluefbAssist$position == 'Top',]),
                    nrow(dbluefbAssist[dbluefbAssist$position == 'Jungle',]),
                    nrow(dbluefbAssist[dbluefbAssist$position == 'Middle',]),
                    nrow(dbluefbAssist[dbluefbAssist$position == 'ADC',]),
                    nrow(dbluefbAssist[dbluefbAssist$position == 'Support',]))
dbluefbVictim = subset(leagueData1,fbvictim==1&side=='Blue')
bluefbVictimPosition= c(nrow(dbluefbVictim[dbluefbVictim$position == 'Top',]),
                    nrow(dbluefbVictim[dbluefbVictim$position == 'Jungle',]),
                    nrow(dbluefbVictim[dbluefbVictim$position == 'Middle',]),
                    nrow(dbluefbVictim[dbluefbVictim$position == 'ADC',]),
                    nrow(dbluefbVictim[dbluefbVictim$position == 'Support',]))
fbbluepositions = data.frame(position=c('Top','Jungle','Middle','ADC','Support'),
                         bluefbPosition,bluefbAssistPosition,bluefbVictimPosition,
                         'fb','fbAssist','fbVictim')

ggplot(fbbluepositions, aes(fill=position,x = 'fb', y = bluefbPosition))+
      geom_bar( stat="identity")+ theme_minimal()
ggplot(fbbluepositions, aes(fill=position,x = 'fbAssist', y = bluefbAssistPosition))+
  geom_bar( stat="identity")+ theme_minimal()
ggplot(fbbluepositions, aes(fill=position,x = 'fbVictim', y = bluefbVictimPosition))+
  geom_bar( stat="identity")+ theme_minimal()
#Positions Red
redfbPosition = c(nrow(dredfb[dredfb$position == 'Top',]),
                   nrow(dredfb[dredfb$position == 'Jungle',]),
                   nrow(dredfb[dredfb$position == 'Middle',]),
                   nrow(dredfb[dredfb$position == 'ADC',]),
                   nrow(dredfb[dredfb$position == 'Support',]))
dredfbAssist = subset(leagueData1,fbassist==1&side=='Red')
redfbAssistPosition= c(nrow(dredfbAssist[dredfbAssist$position == 'Top',]),
                        nrow(dredfbAssist[dredfbAssist$position == 'Jungle',]),
                        nrow(dredfbAssist[dredfbAssist$position == 'Middle',]),
                        nrow(dredfbAssist[dredfbAssist$position == 'ADC',]),
                        nrow(dredfbAssist[dredfbAssist$position == 'Support',]))
dredfbVictim = subset(leagueData1,fbvictim==1&side=='Red')
redfbVictimPosition= c(nrow(dredfbVictim[dredfbVictim$position == 'Top',]),
                        nrow(dredfbVictim[dredfbVictim$position == 'Jungle',]),
                        nrow(dredfbVictim[dredfbVictim$position == 'Middle',]),
                        nrow(dredfbVictim[dredfbVictim$position == 'ADC',]),
                        nrow(dredfbVictim[dredfbVictim$position == 'Support',]))
fbredpositions = data.frame(position=c('Top','Jungle','Middle','ADC','Support'),
                         redfbPosition,redfbAssistPosition,redfbVictimPosition,
                         'fb','fbAssist','fbVictim')

ggplot(fbredpositions, aes(fill=position,x = 'fb', y = redfbPosition))+
  geom_bar( stat="identity")+ theme_minimal()
ggplot(fbredpositions, aes(fill=position,x = 'fbAssist', y = redfbAssistPosition))+
  geom_bar( stat="identity")+ theme_minimal()
ggplot(fbredpositions, aes(fill=position,x = 'fbVictim', y = redfbVictimPosition))+
  geom_bar( stat="identity")+ theme_minimal()


#First tower taken by which team
blueft = nrow(blueTeam[blueTeam$ft == 1,])   #738
redft = nrow(redTeam[redTeam$ft == 1,])      #638 
ftTotal = blueft + redft
blueftw = nrow(blueTeamWin[blueTeamWin$ft == 1,])   #473
redftw = nrow(redTeamWin[redTeamWin$ft == 1,])      #359 
ft = data.frame(type=c('all','win team'),
                blue = c(round(100*blueft/ftTotal,2),round(100*blueftw/ftTotal,2)),
                red = c(round(100*redft/ftTotal,2),round(100*redftw/ftTotal,2)))
ft2 = melt(ft, id.vars='type')
ggplot(data=ft2,aes(x=type,y=value,fill=variable)) + 
  geom_bar(stat="identity", position = 'dodge',width=0.5)+
  scale_fill_manual("value", values = c("blue" = "#66ccff","red" = "#FF9999"))+
  theme_minimal()+
  labs(title="first tower taken",y='%',x='')+
  theme(text = element_text(size=30))+
  geom_text(aes(label=value), position = position_dodge(width = 0.5),vjust=3,color='white',size=10)


#First tower taken down time
bluefttime = mean(dblueft[['fttime']],na.rm = TRUE) #mean 14.50min, range 6.87min to 22.08min
redfttime = mean(dredft[['fttime']],na.rm = TRUE)   #mean 14.86min, range 9.34min to 23.34min

plot(data.frame(ftnumber=cut(dblueft[['fttime']], breaks=seq(5,25,by=1))),
     xlab='minute',ylab='first tower time',
     col='blue')
par(new=TRUE)
plot(data.frame(ftnumber=cut(dredft[['fttime']], breaks=seq(5,25,by=1))),
     xaxt="n",yaxt="n",
     col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))


#Gold average
#10 min and 15 min
blueGoldat10Mean = mean(blueWin[['goldat10']],na.rm=TRUE)
blueGoldat15Mean = mean(blueWin[['goldat15']],na.rm=TRUE)
redGoldat10Mean = mean(redWin[['goldat10']],na.rm=TRUE)
redGoldat15Mean = mean(redWin[['goldat15']],na.rm=TRUE)
gold = data.frame(red = c(redGoldat10Mean,redGoldat15Mean),
                  blue = c(blueGoldat10Mean,blueGoldat15Mean),
                  minute=c(10,15))
gold2 = melt(gold, id.vars='minute')
ggplot(data=gold2,aes(x=minute,y=value,fill=variable)) + 
      geom_bar(stat="identity",position='dodge',width=2)+
      expand_limits(x = c(5,20), y = 0)+
      theme_minimal()+ 
      scale_fill_manual("value", values = c("blue" = "#66ccff","red" = "#FF9999"))+
      labs(title="gold(win)")+
      theme(text = element_text(size=30))
#blue gold 10 and 15
blueTop=subset(blue,position == 'Top')
blueJungle=subset(blue,position == 'Jungle')
blueMiddle=subset(blue,position == 'Middle')
blueADC=subset(blue,position == 'ADC')
blueSupport=subset(blue,position == 'Support')
blueGoldat10Position = c(mean(blueTop[['goldat10']],na.rm = TRUE),
                         mean(blueJungle[['goldat10']],na.rm = TRUE),
                         mean(blueMiddle[['goldat10']],na.rm = TRUE),
                         mean(blueADC[['goldat10']],na.rm = TRUE),
                         mean(blueSupport[['goldat10']],na.rm = TRUE))
blueGoldat15Position = c(mean(blueTop[['goldat15']],na.rm = TRUE),
                         mean(blueJungle[['goldat15']],na.rm = TRUE),
                         mean(blueMiddle[['goldat15']],na.rm = TRUE),
                         mean(blueADC[['goldat15']],na.rm = TRUE),
                         mean(blueSupport[['goldat15']],na.rm = TRUE))
goldbluepositions = data.frame(position=c('Top','Jungle','Middle','ADC','Support'),
                               blueGoldat10Position,blueGoldat15Position,
                               'gold at 10','gold at 15')
ggplot(goldbluepositions, aes(fill=position,x = 'gold at 10', y = blueGoldat10Position))+
  geom_bar( stat="identity")+ theme_minimal()+
  theme(legend.position = "none")+labs(title="Blue 10min Positions")
ggplot(goldbluepositions, aes(fill=position,x = 'gold at 15', y = blueGoldat15Position))+
  geom_bar( stat="identity")+ theme_minimal()+
  theme(legend.position = "none")+labs(title="Blue 15min Positions")

#red gold 10 and 15
redTop=subset(red,position == 'Top')
redJungle=subset(red,position == 'Jungle')
redMiddle=subset(red,position == 'Middle')
redADC=subset(red,position == 'ADC')
redSupport=subset(red,position == 'Support')
redGoldat10Position = c(mean(redTop[['goldat10']],na.rm = TRUE),
                         mean(redJungle[['goldat10']],na.rm = TRUE),
                         mean(redMiddle[['goldat10']],na.rm = TRUE),
                         mean(redADC[['goldat10']],na.rm = TRUE),
                         mean(redSupport[['goldat10']],na.rm = TRUE))
redGoldat15Position = c(mean(redTop[['goldat15']],na.rm = TRUE),
                         mean(redJungle[['goldat15']],na.rm = TRUE),
                         mean(redMiddle[['goldat15']],na.rm = TRUE),
                         mean(redADC[['goldat15']],na.rm = TRUE),
                         mean(redSupport[['goldat15']],na.rm = TRUE))
goldredpositions = data.frame(position=c('Top','Jungle','Middle','ADC','Support'),
                              redGoldat10Position,redGoldat15Position,
                               'gold at 10','gold at 15')
ggplot(goldredpositions, aes(fill=position,x = 'gold at 10', y = redGoldat10Position))+
  geom_bar( stat="identity")+ theme_minimal()+
  theme(legend.position = "none")+labs(title="Red 10min Positions")
ggplot(goldredpositions, aes(fill=position,x = 'gold at 15', y = blueGoldat15Position))+
  geom_bar( stat="identity")+ theme_minimal()+
  theme(legend.position = "none")+labs(title="Red 15min Positions")



#Champions
nrow(blueWin[blueWin$champion=='Aatrox',])/nrow(blue[blue$champion=='Aatrox',]) #52.08%
nrow(redWin[redWin$champion=='Aatrox',])/nrow(red[red$champion=='Aatrox',])     #43.10%
nrow(blueWin[blueWin$champion=='Akali',])/nrow(blue[blue$champion=='Akali',]) #52.08%
nrow(redWin[redWin$champion=='Akali',])/nrow(red[red$champion=='Akali',])     #51.90%
nrow(blueWin[blueWin$champion=='Alistar',])/nrow(blue[blue$champion=='Alistar',]) #54.66%
nrow(redWin[redWin$champion=='Alistar',])/nrow(red[red$champion=='Alistar',])     #44.51%
nrow(blueWin[blueWin$champion=='Ashe',])/nrow(blue[blue$champion=='Ashe',]) #58.83%
nrow(redWin[redWin$champion=='Ashe',])/nrow(red[red$champion=='Ashe',])     #50.94%
nrow(blueWin[blueWin$champion=='Braum',])/nrow(blue[blue$champion=='Braum',]) #51.29%
nrow(redWin[redWin$champion=='Braum',])/nrow(red[red$champion=='Braum',])     #42.33%
nrow(blueWin[blueWin$champion=='Camille',])/nrow(blue[blue$champion=='Camille',]) #52%
nrow(redWin[redWin$champion=='Camille',])/nrow(red[red$champion=='Camille',])     #46.93%
nrow(blueWin[blueWin$champion=='Cassiopeia',])/nrow(blue[blue$champion=='Cassiopeia',]) #55.56%
nrow(redWin[redWin$champion=='Cassiopeia',])/nrow(red[red$champion=='Cassiopeia',])     #39.39%
nrow(blueWin[blueWin$champion=='Cassiopeia',])/nrow(blue[blue$champion=='Cassiopeia',]) #55.56%
nrow(redWin[redWin$champion=='Cassiopeia',])/nrow(red[red$champion=='Cassiopeia',])     #39.39%
nrow(blueWin[blueWin$champion=='Corki',])/nrow(blue[blue$champion=='Corki',]) #48.48%
nrow(redWin[redWin$champion=='Corki',])/nrow(red[red$champion=='Corki',])     #39.58%
nrow(blueWin[blueWin$champion=='Ezreal',])/nrow(blue[blue$champion=='Ezreal',]) #50.86%
nrow(redWin[redWin$champion=='Ezreal',])/nrow(red[red$champion=='Ezreal',])     #51.08%
nrow(blueWin[blueWin$champion=='Galio',])/nrow(blue[blue$champion=='Galio',]) #55.36%
nrow(redWin[redWin$champion=='Galio',])/nrow(red[red$champion=='Galio',])     #45.5%
nrow(blueWin[blueWin$champion=='Gangplank',])/nrow(blue[blue$champion=='Gangplank',]) #80%
nrow(redWin[redWin$champion=='Gangplank',])/nrow(red[red$champion=='Gangplank',])     #36.67%
nrow(blueWin[blueWin$champion=='Gragas',])/nrow(blue[blue$champion=='Gragas',]) #52.69%
nrow(redWin[redWin$champion=='Gragas',])/nrow(red[red$champion=='Gragas',])     #47.14%

nrow(blueWin[blueWin$champion=='Corki',])/nrow(blue[blue$champion=='Heimerdinger',]) #51.29%
nrow(redWin[redWin$champion=='Corki',])/nrow(red[red$champion=='Corki',])     #42.33%
nrow(blueWin[blueWin$champion=='Corki',])/nrow(blue[blue$champion=='Corki',]) #51.29%
nrow(redWin[redWin$champion=='Corki',])/nrow(red[red$champion=='Corki',])     #42.33%








