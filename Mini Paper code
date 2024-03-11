library(Lahman)
library(tidyverse)
library(lubridate)
library(retrosheet)
library(dslabs)
library(ggthemes)
library(moments)
library(ggpubr)


rs2018all <- getRetrosheet("game",2018)
names(rs2018all)
head(rs2018all)
rs2018 <- rs2018all %>% 
  filter(HmTm == "COL" | VisTm == "COL")
names(rs2018)
head(rs2018)

rs2018play <- getRetrosheet("play",team = "COL",2018)
head(rs2018play)

rs2018all$Month <- month(ymd(rs2018all$Date),label=F,abbr=T)

#Trying to get plots (by month) of the average runs given up for each MLB team and for home and away
#then make the dot for the Colorado Rockies red (similar to pythagorean expectation plot at bottom) to see where they stacked up against other teams in 2018 
rs2018all %>% 
  group_by(Month) %>% 
  mutate(avg_runs_given_up_when_home = mean(VisRuns)) %>% 
  mutate(avg_runs_given_up_when_away = mean(HmRuns)) %>% 
  ggplot(rs2018all, aes(HmTm, avg_runs_given_up_when_home)) + geom_point() + facet_wrap(~Month)
  ggplot(rs2018all, aes(VisTm, avg_runs_given_up_when_away)) + geom_point() + facet_wrap(~Month)
  

#Trying to get scatterplots (by month and then home/away) with the date of the games on the x-axis 
#and the count of same-handedness matchups for the Rockies on the y-axis
#then have the dots when the Rockies won be green and the dots when they lost be red
rs2018 %>% 
  group_by(Month) %>% 
  filter(HmTm == "COL") %>% 
  ggplot(aes(HmTm)) + geom_point() + facet_wrap(~Month)

rs2018 %>% 
  group_by(Month) %>% 
  filter(VisTm == "COL") %>% 
  ggplot(aes(VisTm)) + geom_point() + facet_wrap(~Month)
  


#Get retroid and handedness of player from People table and then join to retrosheet data
#figure out how to get both the handedness of the pitcher and hitter in the data


#Sample Pythagorean Expectation Example Plot
PEdata <- Teams %>% select(yearID,lgID,teamID,W,L,R,RA) %>% 
  filter(yearID==2018,lgID=="NL") %>% 
  mutate(WPct=R^2/(R^2+RA^2),expectedwins=WPct*(W+L),diff=W-expectedwins) 

PEdata 

ggplot(PEdata,aes(expectedwins,W)) + geom_point(col = ifelse(1:nrow(PEdata) == 5, "red","black")) + stat_smooth(method="lm") + labs(x="Expected Wins", y="Wins", title="Rockies vs. NL 2018") + stat_cor(aes(label=..rr.label..)) 
 

pitcher <- c("Adam Ottavino","Wade Davis", "Seunghwan Oh","German Marquez","Scott Oberg","Tyler Anderson","Kyle Freeland","Jon Gray","Chris Rusin","Antonio Senzatela","Jake McGee","Chad Bettis","Bryan Shaw")
handedness <- c("R","R","R","R","R","L","L","R","L","R","L","R","R")
xwoba <- c(.230,.247,.262,.282,.291,.296,.300,.307,.309,.319,.336,.343,.352)
pitcher_type <- c("Reliever","Reliever","Reliever","Starter","Reliever","Starter","Starter","Starter","Reliever","Starter","Reliever","Reliever","Reliever")

phxp <- cbind(pitcher, handedness, pitcher_type, xwoba)
phxp

names(People)


  

