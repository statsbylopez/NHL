library(MASS)
library(sm)
library(gplots)
library(XML)
library(stringr)
library(ggplot2)
library(splines)
require("extrafont")
require(scales)
require(grid)
library(dplyry)
loadfonts()

nhl<-NULL
urls<- c("http://www.hockey-reference.com/leagues/NHL_1990_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1991_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1992_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1993_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1994_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1995_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1996_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1997_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1998_games.html",
         "http://www.hockey-reference.com/leagues/NHL_1999_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2006_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2007_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2008_games.html",
        "http://www.hockey-reference.com/leagues/NHL_2009_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2010_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2011_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2012_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2013_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2014_games.html",
         "http://www.hockey-reference.com/leagues/NHL_2015_games.html")

for (i in 1:length(urls)){
  tables <- readHTMLTable(urls[i])
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  temp<-tables[[which.max(n.rows)]]
  temp$Year<-i+1989
  nhl<-rbind(nhl,temp)
}

names(nhl)<-c("Date","Visitor","VisGoals","Home","HomeGoals","OTCat","Notes","Year")
table(nhl$OTCat)
nhl<-nhl[nhl$OTCat!="Get Tickets",]
nhl$OT<-nhl$OTCat=="OT"|nhl$OTCat=="SO"
nhl$SO<-nhl$OTCat=="SO"
nhl[nhl$Year>=2000,]$Year<-nhl[nhl$Year>=2000,]$Year+6
head(nhl)


nhl<-nhl[nhl$Year!=1995,]     #Drop lockout seasons here
nhl<-nhl[nhl$Year!=2013,]
#nhl<-nhl[nhl$Year!=2015,]
nhl$NewPS<-nhl$Year>2000
nhl$Date<-as.Date(nhl$Date)
max.dates<-aggregate(Date~Year,FUN="max",data=nhl)
nhl$DaysFromEnd<-0
nhl.1<-nhl[nhl$HomeGoals!="",]  #eliminates the 5 games lost in data (due to snow days, mostly)

nhl$PS<-"1990-1999"
nhl[nhl$Year>1999,]$PS<-"2000-2004"
nhl[nhl$Year>2004,]$PS<-"2005-current"


#Olympics in 1998 (18), 2006 (16), 2010 (15), 2014 (17 days lost)

for(i in 1:nrow(max.dates)){
  nhl[nhl$Year==max.dates[i,1],]$DaysFromEnd<-max.dates[i,2]-nhl[nhl$Year==max.dates[i,1],]$Date
}

nhl[nhl$Year==1998&nhl$DaysFromEnd>54,]$DaysFromEnd<-nhl[nhl$Year==1998&nhl$DaysFromEnd>54,]$DaysFromEnd-18
nhl[nhl$Year==2006&nhl$DaysFromEnd>50,]$DaysFromEnd<-nhl[nhl$Year==2006&nhl$DaysFromEnd>50,]$DaysFromEnd-16
nhl[nhl$Year==2010&nhl$DaysFromEnd>50,]$DaysFromEnd<-nhl[nhl$Year==2010&nhl$DaysFromEnd>50,]$DaysFromEnd-15
nhl[nhl$Year==2014&nhl$DaysFromEnd>50,]$DaysFromEnd<-nhl[nhl$Year==2014&nhl$DaysFromEnd>50,]$DaysFromEnd-17

nhl[nhl$DaysFromEnd>188,]$DaysFromEnd<-188

nhl$DayofSeason<-189-nhl$DaysFromEnd

###### Graph using ggplot2

p<-qplot(DayofSeason, as.numeric(OT), lty=PS,data = nhl,geom="smooth",size=1.4,
      method="glm",family="binomial", formula = y ~ ns(x, 2)) +  scale_x_continuous("Day of the Season")+
  Five38Thm+ theme(legend.position="none")+
  annotate("text", label = "Current PS, 2005-2015", x = 50, y = 0.25,size=6)+
  annotate("text", label = "Old PS, 1990-1999", x = 50, y = 0.17,size=6)
p
