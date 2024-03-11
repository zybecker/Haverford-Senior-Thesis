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

people = Lahman::People %>%select(retroID,bats) %>% as_tibble

retroIDjoin <- inner_join(as_tibble(rs2018play[[1]]$play), people, by = "retroID")
print(retroIDjoin)



lapply(rs2018play,function(x){
  x$play <- inner_join(
    as_tibble(x$play),
    people,
    by = "retroID")
  return(x)
})

listPlay<- lapply(rs2018play,function(x){
  play <- inner_join(
    as_tibble(x$play),
    people,
    by = "retroID") %>% filter(play != "NP") %>% filter(team != 1) %>% filter(bats != "B")
  return(play)
})

bind_rows(listPlay,.id="Game")


gamenum <- (1:46)
winorloss <- c("L","W","L","L","L","W","L","W","L","L","W","W","W","L","L","L","W","L","W","L","W","W","W","L","L","L","L","L","L","L","L","W","W","W","W","L","L","W","W","W","L","W","W","W","W","W")
handednessmatchups <- c(16, 16, 18, 15, 15, 17, 30, 8, 23, 21, 9, 19, 29, 5, 28, 30, 12, 24, 11, 13, 11, 26, 14, 22, 17, 24, 25, 17, 15, 11, 20, 25, 21, 13, 25, 10, 20, 13, 14, 11, 25, 10, 14, 20, 13, 14)
totalmatchups <- c(45, 36, 37, 41, 41, 35, 50,  35, 46, 49, 33, 35, 35, 44, 44, 44, 36, 43, 39, 41, 41, 42, 40, 42, 46, 46, 40, 42, 45, 43, 47, 40,  49, 42, 38, 40, 43, 33, 35, 33, 42, 36, 37, 43, 33, 37)
handednessprop <- handednessmatchups/totalmatchups

firsthalfhandedness <- cbind(gamenum, winorloss, handednessmatchups, totalmatchups, handednessprop)
firsthalfhandednessdf <- dataframe <- data.frame(firsthalfhandedness)
firsthalfhandednessdf
ggplot(firsthalfhandednessdf, aes(gamenum, handednessprop), color=winorloss) + geom_point(col = ifelse(winorloss == "W", "dark green","red"),size=6) + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) + labs(x="Games", y="Percentage of Matchups that are Same-handed", title="Same-handedness Proportions for Wins and Losses")

a <- c("W","W","L","L")
b <- c(16, 18, 14, 13)
c <- c(41, 39, 44, 42)
d <- b/c
e <- (1:4)

data <- cbind(a,b,c,d,e)
dataframe <- data.frame(data)
ggplot(dataframe, aes(e,d),color=a) + geom_point()






