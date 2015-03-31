####First, read in the data (it's a large data set!)
rm(list = ls())
library(MASS)
library(sm)
library(gplots)
library(XML)
library(stringr)
library(nhlscrapr)
library(plyr)
library(dplyr)

#compile.all.games (rdata.folder="nhlr-data", output.folder="source-data",wait=1,seasons="20132014")

seasons <- 2005:2015
my.seasons <- paste(seasons[1:(length(seasons)-1)], seasons[2:length(seasons)], sep = "")
#  Load all data, store in a big data.frame
all.data <- NULL
grand.data <- NULL

for(ii in 1:(length(seasons)-1)){
  print(c(ii, my.seasons[ii]))
  load(paste0("source-data/nhlscrapr-", my.seasons[ii], ".RData"))
  all.data <- rbind(all.data, grand.data)
  grand.data <- NULL
}

recent<-all.data



##Calculate expected goals using Logistic Regression
##We do not include blocked shots cannot be included because of faults with the location data

shots<-c("SHOT","GOAL","MISS") 
r.shots<-recent[recent$etype%in%shots,] #shots, goals only 
r.shots<-r.shots[r.shots$period>0&r.shots$period<4,]   #nothing in OT
r.shots[r.shots$seconds<3420&r.shots$home.G==1,]$home.G<-2
r.shots[r.shots$seconds<3420&r.shots$away.G==1,]$away.G<-2
r.shots[r.shots$etype=="GOAL"&r.shots$away.G==1,]$home.G<-2
r.shots[r.shots$etype=="GOAL"&r.shots$away.G==1,]$away.G<-2
r.shots<-subset(r.shots, !is.na(distance))  #Getting rid of events without distance
r.shots2<-subset(r.shots, home.G > 1 & away.G > 1 ) 
r.shots$MissingShots<-is.na(r.shots$newxc)|is.na(r.shots$newyc)
r.shots$is.goal <- 1*(r.shots$etype == "GOAL")
r.shots$ID<-1:nrow(r.shots)

#This is the subset of the data for which we have shot location

r.shots1<-r.shots[r.shots$MissingShots=="FALSE",]
r.shots1$dist.from.slot <- abs(r.shots1$newyc)
r.shots1$newxc2 <-r.shots1$newxc^2

#Here is the logistic regression model used to calculate expected goals
#Sam has found the best model performance does not use shot type
#Lots of area for future study here

fit1<-glm(is.goal~(newxc+newxc2+dist.from.slot)^2,data=r.shots1,family=binomial()) 
summary(fit1)
r.shots1$shot.prob.distance<-fitted(fit1)

#For shots without coordinate information - distance only. Including type reduces noise.
r.shots2<-r.shots[r.shots$MissingShots=="TRUE",]
fit2<-glm(is.goal~(distance+type)^2,data=r.shots2,family=binomial())  
summary(fit2)
r.shots2$shot.prob.distance<-fitted(fit2)


colnames<-c("season","gcode","refdate","period","seconds","etype","ev.team","awayteam","hometeam","event.length","home.score","away.score",
            "shot.prob.distance","ID")

r.shots1<-r.shots1[,colnames]
r.shots2<-r.shots2[,colnames]

#Combine both data sets - when we have shot location and when we do not have it (older seasons)
pbp<-rbind(r.shots1,r.shots2)

#Newer point systems only
pbp<-pbp[!(pbp$season%in%"20022003"),]
pbp<-pbp[!(pbp$season%in%"20032004"),]

#Impute missing reference dates, by splitting into three groups
rdate<-"2002-01-01"
pbp$Date<-as.Date(rdate)+pbp$refdate
pbp[pbp$refdate==0&pbp$season=="20052006",]$Date<-"2005-10-15"
pbp[pbp$refdate==0&pbp$season=="20052006"&pbp$gcode>=20410,]$Date<-"2006-01-15"
pbp[pbp$refdate==0&pbp$season=="20052006"&pbp$gcode>=20820,]$Date<-"2006-03-15"
pbp[pbp$refdate==0&pbp$season=="20062007",]$Date<-"2006-10-15"
pbp[pbp$refdate==0&pbp$season=="20062007"&pbp$gcode>=20410,]$Date<-"2007-01-15"
pbp[pbp$refdate==0&pbp$season=="20062007"&pbp$gcode>=20820,]$Date<-"2007-03-15"
pbp$GID<-paste(pbp$awayteam,pbp$hometeam,pbp$date)


nhl<-pbp
nhl<-nhl[as.numeric(as.character(nhl$gcode))<30000,]  #No postseason information


#Next, we get information on the day of the year in which the game occurred 
nhl$Year<-substr(nhl$season, 1, 4)
max.dates<-aggregate(Date~Year,FUN="max",data=nhl)
nhl$DaysFromEnd<-0
for(i in 1:nrow(max.dates)){
  nhl[nhl$Year==max.dates[i,1],]$DaysFromEnd<-max.dates[i,2]-nhl[nhl$Year==max.dates[i,1],]$Date
}
#Olympic Years
nhl[nhl$Year==2006&nhl$DaysFromEnd>50,]$DaysFromEnd<-nhl[nhl$Year==2006&nhl$DaysFromEnd>50,]$DaysFromEnd-16
nhl[nhl$Year==2010&nhl$DaysFromEnd>50,]$DaysFromEnd<-nhl[nhl$Year==2010&nhl$DaysFromEnd>50,]$DaysFromEnd-15
nhl[nhl$Year==2014&nhl$DaysFromEnd>50,]$DaysFromEnd<-nhl[nhl$Year==2014&nhl$DaysFromEnd>50,]$DaysFromEnd-17
nhl[nhl$DaysFromEnd>188,]$DaysFromEnd<-188
nhl$WoS<-27-floor(nhl$DaysFromEnd/7)


#Next is a category for month and the type of game (conference, nonconference)
nhl$MonthNum<-substr(nhl$Date,6,7)
nhl$MonthCat<-"1.Early"
nhl[nhl$MonthNum=="01"|nhl$MonthNum=="02",]$MonthCat<-"2.JanFeb"
nhl[nhl$MonthNum=="03",]$MonthCat<-"3.Mar"
nhl[nhl$MonthNum=="04",]$MonthCat<-"4.Apr"
nhl[nhl$awayteam=="ARI",]$awayteam<-"PHX"
nhl[nhl$hometeam=="ARI",]$hometeam<-"PHX"
nhl$day<-weekdays(as.Date(nhl$Date))
nhl$day.cat<-"0.MonTues"
nhl[nhl$day%in%c("Wednesday","Thursday"),]$day.cat<-"1.WedsTh"
nhl[nhl$day%in%c("Friday","Saturday","Sunday"),]$day.cat<-"2.Weekend"
nhl$Conference.Vis<-"Eastern"
nhl$Conference.Home<-"Eastern"
Western<-c("VAN","STL","PHX","NSH","DET","CHI","S.J"
           ,"L.A","CAR","DAL","COL","MIN","ANA","EDM")
nhl[substr(nhl$awayteam,1,3)%in%Western,]$Conference.Vis<-"Western"
nhl[substr(nhl$hometeam,1,3)%in%Western,]$Conference.Home<-"Western"
nhl[substr(nhl$awayteam,1,3)=="WPG"&nhl$Year>=2014,]$Conference.Vis<-"Western"
nhl[substr(nhl$hometeam,1,3)=="WPG"&nhl$Year>=2014,]$Conference.Home<-"Western"
nhl[substr(nhl$awayteam,1,4)=="CBJ"&nhl$Year>=2014,]$Conference.Vis<-"Eastern"
nhl[substr(nhl$hometeam,1,4)=="CBJ"&nhl$Year>=2014,]$Conference.Home<-"Eastern"
nhl[substr(nhl$awayteam,1,3)=="DET"&nhl$Year>=2014,]$Conference.Vis<-"Eastern"
nhl[substr(nhl$hometeam,1,3)=="DET"&nhl$Year>=2014,]$Conference.Home<-"Eastern"
nhl.1<-nhl
nhl$Conference<-nhl$Conference.Vis==nhl$Conference.Home
table(nhl$Conference)
tabnames<-names(table(nhl.1$awayteam))
nhl.1$GameMinute<-floor(nhl.1$seconds/60)+1
nhl.1[nhl.1$GameMinute==61,]$GameMinute<-60
nhl.1<-nhl.1[order(nhl.1$ID),]

#Check some games
temp<-subset(nhl.1,nhl.1$season=="20052006"&nhl.1$gcode==20001)
temp<-filter(nhl.1,season=="20052006",gcode==20001)
temp<-nhl.1[nhl.1$season=="20132014"&nhl.1$gcode==20001,]


#This function extracts game behavior per game minute, by game situation

game.fun<-function(gyear,code){
  #temp<-nhl.1[nhl.1$season=="20132014"&nhl.1$gcode==20001,]
  temp<-filter(nhl.1,season==gyear,gcode==code)
  temp$diff<-abs(temp$home.score-temp$away.score)
  temp.stats<-matrix(nrow=3600,ncol=1,NA)
  temp.goal.time<-temp[temp$etype=="GOAL",]$seconds
  mat.event<-matrix(nrow=length(temp.goal.time)+1,ncol=3)
  mat.event[,1]<-c(0,temp.goal.time+1)
  mat.event[,2]<-c(temp.goal.time,3600)
  if (nrow(mat.event)==1){ mat.event[1,3]<-0
  }else {
    mat.event[(length(temp.goal.time)+1),3]<-temp[nrow(temp),]$diff
    for (i in 1:(nrow(mat.event)-1)){
      mat.event[i,3]<-temp[(temp$seconds==mat.event[i,2]),][1,]$diff}
    if (temp[nrow(temp),]$etype=="GOAL"){
      if (temp[nrow(temp),]$ev.team==temp[nrow(temp),]$awayteam)
        temp[nrow(temp),]$diff<-abs(temp[nrow(temp),]$home.score-(temp[nrow(temp),]$away.score+1))
      if (temp[nrow(temp),]$ev.team==temp[nrow(temp),]$hometeam)
        temp[nrow(temp),]$diff<-abs(temp[nrow(temp),]$away.score-(temp[nrow(temp),]$home.score+1))
      mat.event[nrow(mat.event),3]<-temp[nrow(temp),]$diff}}
  game.sum<-matrix(nrow=60,ncol=10,0)
  game.sum[,1]<-1:60
  tied.seconds<-0
  one.seconds<-0
  two.seconds<-0
  if(nrow(matrix(mat.event[mat.event[,3]==0,],ncol=3))>0){for (j in 1:nrow(matrix(mat.event[mat.event[,3]==0,],ncol=3))){
    tied.seconds<-c(tied.seconds,matrix(mat.event[mat.event[,3]==0,],ncol=3)[j,1]:matrix(mat.event[mat.event[,3]==0,],ncol=3)[j,2])}}
  if(nrow(matrix(mat.event[mat.event[,3]==1,],ncol=3))>0){for (j in 1:nrow(matrix(mat.event[mat.event[,3]==1,],ncol=3))){
    one.seconds<-c(one.seconds,matrix(mat.event[mat.event[,3]==1,],ncol=3)[j,1]:matrix(mat.event[mat.event[,3]==1,],ncol=3)[j,2])}}
  if(nrow(matrix(mat.event[mat.event[,3]>=2,],ncol=3))>0){for (j in 1:nrow(matrix(mat.event[mat.event[,3]>=2,],ncol=3))){
    two.seconds<-c(two.seconds,matrix(mat.event[mat.event[,3]>=2,],ncol=3)[j,1]:matrix(mat.event[mat.event[,3]>=2,],ncol=3)[j,2])}}
  
  events<-temp$seconds
  egoals<-temp$shot.prob.distance
  for (i in 1:60){
    LL<-60*(i-1)+1;UL<-60*i 
    game.sum[i,2]<-sum(LL:UL%in%tied.seconds)
    game.sum[i,3]<-sum(LL:UL%in%one.seconds)
    game.sum[i,4]<-sum(LL:UL%in%two.seconds)
    game.sum[i,5]<-sum(events%in%intersect(LL:UL,tied.seconds))
    game.sum[i,6]<-sum(events%in%intersect(LL:UL,one.seconds))
    game.sum[i,7]<-sum(events%in%intersect(LL:UL,two.seconds))
    game.sum[i,8]<-sum(egoals[events%in%intersect(LL:UL,tied.seconds)])
    game.sum[i,9]<-sum(egoals[events%in%intersect(LL:UL,one.seconds)])
    game.sum[i,10]<-sum(egoals[events%in%intersect(LL:UL,two.seconds)])
    colnames(game.sum)<-c("GameMinute","Tied.Seconds","One.Seconds","Two.Seconds",
                          "Tied.Shots","One.Shots","Two.Shots",
                          "Tied.EG","One.EG","Two.EG")
  }  
  game.sum<-data.frame(game.sum)
  
  
  game.sum$season<-temp$season[1];game.sum$Date<-temp$Date[1];game.sum$WoS[1]<-temp$WoS[1];game.sum$MonthNum<-temp$MonthNum[1]
  game.sum$Conference<-temp$Conference[1];game.sum$day<-temp$day[1];game.sum$away<-temp$awayteam[1];game.sum$home<-temp$hometeam[1];game.sum$gcode<-temp$gcode[1]
  game.sum
}

#Next, we run the above function for each game. This takes a few hours

Minute.Data<-NULL
seas<-unique(nhl.1$season)
for (m in 1:length(seas)){#length(seas)  #length(seas) is 20132014
  gids<-unique(nhl.1[nhl.1$season==seas[m],]$gcode)
  for (n in 1:length(gids)){#length(gids)
    Minute.Data<-rbind(Minute.Data,game.fun(seas[m],gids[n]));print(n);print(m)}}

#Tada! We have per-minute data for each NHL game 

write.csv(Minute.Data,"NHL.MinuteData.csv")




##############################################
#Now is the fun part: Graphing the per-minute behavior.

Minute.Data<-read.csv("NHL.MinuteData.csv")

#I am most interested in seasons with shot location information
seas<-c("20072008","20082009","20092010","20102011","20112012","20122013","20132014","20142015")

Minute.Data<-Minute.Data[Minute.Data$season%in%seas,]


#Expected goal rate per game second, tied games
EG<-aggregate(Tied.EG~GameMinute,data=Minute.Data,FUN="sum")
EG2<-aggregate(Tied.Seconds~GameMinute,data=Minute.Data,FUN="sum")
EG$Rate<-EG[,2]/EG2[,2]
Tied<-EG
Tied$Situation<-"Tied game"

#Expected goal rate per game second, one-goal games
EG<-aggregate(One.EG~GameMinute,data=Minute.Data,FUN="sum")
EG2<-aggregate(One.Seconds~GameMinute,data=Minute.Data,FUN="sum")
EG$Rate<-EG[,2]/EG2[,2]
One<-EG
One$Situation<-"One goal game"


Overall<-rbind(Tied[,c(1,3,4)],One[,c(1,3,4)])
Overall$Period<-1
Overall[Overall$GameMinute>20,]$Period<-2
Overall[Overall$GameMinute>40,]$Period<-3


###### Here's the graph. Finally, we have team behavior by minute and game situation

p<-ggplot(Overall[Overall$GameMinute<=60,], aes(x = GameMinute, y = 60*Rate,lty=Situation))+
  stat_smooth(method = "loess",span=1,se=FALSE,size=1)+facet_grid(.~Period,scale="free_x")
p
