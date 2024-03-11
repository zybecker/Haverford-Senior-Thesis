require(devtools)

install_github("BIllPetti/baseballr")

require(baseballr)
require(dplyr)
require(readr)


get_retrosheet_data(path_to_directory = "~/Desktop/retrosheet", 
                    years_to_acquire = 2018)

data2018 <- read_csv("~/Desktop/retrosheet/download.folder/unzipped/all2018.csv") %>%
  glimpse()	


library(Lahman)
library(tidyverse)
library(lubridate)
library(retrosheet)
library(dslabs)
library(ggthemes)
library(moments)
library(ggpubr)
library(scales)


#creating full dataframe with all the variables
Rockies2018full <- data2018 %>% 
  filter(substr(game_id, 1, 3) == "COL" | away_team_id == "COL") 

#checking if data frame and getting summary of data
as.data.frame(Rockies2018full)
is.data.frame(Rockies2018full)

#Creating dataframe that only includes the columns that I want
Rockies2018 <- Rockies2018full %>% 
  select(game_id, away_team_id, inn_ct, bat_home_id, outs_ct, away_score_ct, home_score_ct, bat_id, resp_bat_hand_cd, pit_id, pit_hand_cd, resp_pit_id, resp_pit_hand_cd, leadoff_fl, base1_run_id, base2_run_id, base3_run_id, event_tx, event_cd, event_outs_ct, rbi_ct, bat_dest_id, run1_dest_id,run2_dest_id,run3_dest_id, game_new_fl,game_end_fl,event_id)


#Creating current_state column
Rockies2018 <- Rockies2018 %>% 
  mutate(current_state = case_when(
    outs_ct ==  0 & is.na(base1_run_id)  & is.na(base2_run_id) & is.na(base3_run_id) ~ "1",
    outs_ct ==  0 & base1_run_id != "<NA>"  & is.na(base2_run_id) & is.na(base3_run_id) ~ "2",
    outs_ct ==  0 & is.na(base1_run_id)  & base2_run_id != "<NA>" & is.na(base3_run_id) ~ "3",
    outs_ct ==  0 & is.na(base1_run_id)  & is.na(base2_run_id) & base3_run_id != "<NA>" ~ "4",
    outs_ct ==  0 & base1_run_id != "<NA>"  & base2_run_id != "<NA>" & is.na(base3_run_id) ~ "5",
    outs_ct ==  0 & base1_run_id != "<NA>"  & is.na(base2_run_id) & base3_run_id != "<NA>" ~ "6",
    outs_ct ==  0 & is.na(base1_run_id)  & base2_run_id != "<NA>" & base3_run_id != "<NA>" ~ "7",
    outs_ct ==  0 & base1_run_id != "<NA>"  & base2_run_id != "<NA>" & base3_run_id != "<NA>" ~ "8",
    outs_ct ==  1 & is.na(base1_run_id)  & is.na(base2_run_id) & is.na(base3_run_id) ~ "9",
    outs_ct ==  1 & base1_run_id != "<NA>"  & is.na(base2_run_id) & is.na(base3_run_id) ~ "10",
    outs_ct ==  1 & is.na(base1_run_id)  & base2_run_id != "<NA>" & is.na(base3_run_id) ~ "11",
    outs_ct ==  1 & is.na(base1_run_id)  & is.na(base2_run_id) & base3_run_id != "<NA>" ~ "12",
    outs_ct ==  1 & base1_run_id != "<NA>"  & base2_run_id != "<NA>" & is.na(base3_run_id) ~ "13",
    outs_ct ==  1 & base1_run_id != "<NA>"  & is.na(base2_run_id) & base3_run_id != "<NA>" ~ "14",
    outs_ct ==  1 & is.na(base1_run_id)  & base2_run_id != "<NA>" & base3_run_id != "<NA>" ~ "15",
    outs_ct ==  1 & base1_run_id != "<NA>"  & base2_run_id != "<NA>" & base3_run_id != "<NA>" ~ "16",
    outs_ct ==  2 & is.na(base1_run_id)  & is.na(base2_run_id) & is.na(base3_run_id) ~ "17",
    outs_ct ==  2 & base1_run_id != "<NA>"  & is.na(base2_run_id) & is.na(base3_run_id) ~ "18",
    outs_ct ==  2 & is.na(base1_run_id)  & base2_run_id != "<NA>" & is.na(base3_run_id) ~ "19",
    outs_ct ==  2 & is.na(base1_run_id)  & is.na(base2_run_id) & base3_run_id != "<NA>" ~ "20",
    outs_ct ==  2 & base1_run_id != "<NA>"  & base2_run_id != "<NA>" & is.na(base3_run_id) ~ "21",
    outs_ct ==  2 & base1_run_id != "<NA>"  & is.na(base2_run_id) & base3_run_id != "<NA>" ~ "22",
    outs_ct ==  2 & is.na(base1_run_id)  & base2_run_id != "<NA>" & base3_run_id != "<NA>" ~ "23",
    outs_ct ==  2 & base1_run_id != "<NA>"  & base2_run_id != "<NA>" & base3_run_id != "<NA>" ~ "24",
    TRUE ~ "other"
  ))

Rockies2018defenseonly <- Rockies2018 %>% 
  ungroup() %>% 
  filter(pit_id %in% rockies_pitcher_list)

#Creating high leverage appearances variable
highleverage <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("almoy001","daviw001","dunnm002","hoffj003","howas002","mcgej001","obers001","ottaa001","pounb001","rusic001","shawb001")) %>% 
  filter(abs(home_score_ct - away_score_ct) < 4) %>% 
  group_by(pit_id) %>% 
  summarise(situations=sum(current_state %in% c("7","8","15","16","23","24")))


#creating next_state column
Rockies2018 <- Rockies2018 %>%
  group_by(inn_ct) %>% mutate(next_state = lead(current_state, 1))


#Adjusting next_state for absorption states
Rockies2018 <- Rockies2018 %>% 
  mutate(next_state = case_when(
    current_state %in% c("9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24") & lead(current_state, 1) == "1" & rbi_ct == "0" ~ "25",
    current_state %in% c("9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24") & lead(current_state, 1) == "1" & rbi_ct == "1" ~ "26",
    current_state %in% c("9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24") & lead(current_state, 1) == "1" & rbi_ct == "2" ~ "27",
    current_state %in% c("9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24") & lead(current_state, 1) == "1" & rbi_ct == "3" ~ "28",
    TRUE ~ lead(current_state, 1)))

late_game_transitions <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("almoy001","daviw001","dunnm002","hoffj003","howas002","mcgej001","obers001","ottaa001","pounb001","rusic001","shawb001")) %>%
  filter(inn_ct > 6) %>% 
  select(pit_id, next_state) %>% 
  group_by(pit_id) %>% 
  group_by(next_state) %>% 
  summarise(transitions=sum(next_state %in% c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28"))) 

late_game_transitions <- late_game_transitions[-29,]
late_game_transitions <- late_game_transitions %>% 
  mutate(next_state = fct_relevel(next_state,"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28"))

transition_probabilities <- Rockies2018defenseonly %>% 
  select(current_state,next_state) %>% 
  group_by(current_state,next_state) %>% 
  mutate(occurrences = n()) 

transition_probabilities <- transition_probabilities %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")


transition_probabilitiesxwoba <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("ottaa001","daviw001","oh--s001","marqg001","obers001","andet002")) %>% 
  select(current_state,next_state) %>% 
  group_by(current_state,next_state) %>% 
  mutate(occurrences = n())
 

transition_probabilitiesxwoba <- transition_probabilitiesxwoba %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")


transition_probabilitiesxfip <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("ottaa001","daviw001","oh--s001","marqg001","obers001","grayj003")) %>% 
  select(current_state,next_state) %>% 
  group_by(current_state,next_state) %>% 
  mutate(occurrences = n()) 

transition_probabilitiesxfip <- transition_probabilitiesxfip %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")


transition_probabilitiessc <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("ottaa001","andet002","bettc001","freek001","senza001","rusic001")) %>% 
  select(current_state,next_state) %>% 
  group_by(current_state,next_state) %>% 
  mutate(occurrences = n()) 
  
transition_probabilitiessc <- transition_probabilitiessc %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")

transition_probabilitiesbaa <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("ottaa001","daviw001","oh--s001","marqg001","obers001","freek001")) %>% 
  select(current_state,next_state) %>% 
  group_by(current_state,next_state) %>% 
  mutate(occurrences = n()) 

transition_probabilitiesbaa <- transition_probabilitiesbaa %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")


transition_probabilitiesera <- Rockies2018defenseonly %>% 
  filter(pit_id %in% c("ottaa001","daviw001","oh--s001","marqg001","obers001","andet002")) %>% 
  select(current_state,next_state) %>% 
  group_by(current_state,next_state) %>% 
  mutate(occurrences = n()) 

transition_probabilitiesera <- transition_probabilitiesera %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")


transition_probabilitieslgt <- Rockies2018defenseonly %>% 
  filter(inn_ct > 6) %>% 
  select(current_state, next_state) %>% 
  group_by(current_state, next_state) %>% 
  mutate(occurrences = n())
  
transition_probabilitieslgt <- transition_probabilitieslgt %>% 
  distinct(occurrence_prop = occurrences/sum(occurrences), .keep_all = TRUE) %>% 
  filter(next_state != "<NA>")


pitcher <- c("Adam Ottavino","Wade Davis", "Seunghwan Oh","German Marquez","Scott Oberg","Tyler Anderson","Kyle Freeland","Jon Gray","Chris Rusin","Antonio Senzatela","Jake McGee","Chad Bettis","Bryan Shaw")
handedness <- c("R","R","R","R","R","L","L","R","L","R","L","R","R")
pitcher_type <- c("Reliever","Reliever","Reliever","Starter","Reliever","Starter","Starter","Starter","Reliever","Starter","Reliever","Reliever","Reliever")
xwoba <- c(.230,.247,.262,.282,.291,.296,.300,.307,.309,.319,.336,.343,.352)
xfip <- c(3.13,3.63,4.05,3.10,2.83,4.21,4.22,3.47,4.25,4.43,4.41,4.76,4.35)
soft_contact <- c(20.1, 14.8,11.3,17.5,16.0,20.9,20.0,16.0,21.1,20.1,11.1,20.5,14.9)
baa <- c(.158, .185, .209,.241,.213,.248,.240, .266,.268,.266,.285,.265,.313)
era <- c(2.43, 4.13, 2.53, 3.77, 2.45, 4.55, 2.85, 5.12, 6.09, 4.38, 6.49, 5.01, 5.93)

statstable <- cbind(pitcher,handedness,pitcher_type,xwoba,xfip,soft_contact,baa,era)

transition_probabilitieslgt <- transition_probabilitieslgt %>% 
  mutate(current_state = fct_relevel(current_state, "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))

transition_probabilitieslgt <- transition_probabilitieslgt %>% 
  mutate(next_state = fct_relevel(next_state, "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28"))






#ggplots
ggplot(Rockies2018, aes(factor(current_state, level=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)))) + geom_bar() + labs(x="State in Inning", title="Distribution of States in 2018 Rockies Season") + theme_classic()
ggplot(highleverage, aes(x=pit_id,y=situations)) + geom_bar(stat="identity") + theme(legend.position = "none") + scale_x_discrete(labels=c("Yency Almonte","Wade Davis","Mike Dunn","Jeff Hoffman","Jake Mcgee","Scott Oberg","Adam Ottavino","Brooks Pounders","Chris Rusin","Bryan Shaw")) + labs(x="Pitcher",y="Number of High Leverage At Bats",title="Which Relief Pitchers had the Most High Leverage Appearances?")
ggplot(late_game_transitions, aes(x=next_state,y=transitions)) + geom_bar(stat="identity") + theme_linedraw() + theme_gray() + labs(x="Transition States", title="What Were the Most Common Late Game Transitions?")
ggplot(transition_probabilities, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))
ggplot(transition_probabilitiesxwoba, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))
ggplot(transition_probabilitiesxfip, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))
ggplot(transition_probabilitiessc, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))
ggplot(transition_probabilitiesbaa, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))
ggplot(transition_probabilitiesera, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))
ggplot(transition_probabilitieslgt, aes(current_state, next_state)) + geom_raster(aes(fill=occurrence_prop))



'%ni%' <- Negate('%in%')
rockies_pitcher_list <- c("almoy001","daviw001","dunnm002","hoffj003","howas002","mcgej001","obers001","ottaa001","pounb001","rusic001","shawb001","oh--s001","freek001","marqg001","johnd005","senza001","andet002","bettc001","grayj003","vastj001")




Rockies2018defenseonly <- Rockies2018 %>% 
  ungroup() %>% 
  filter(pit_id %in% rockies_pitcher_list)



Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  arrange(game_id) %>% 
  mutate(sub_made = case_when(
    pit_id == lag(pit_id, 1) ~ "No",
    pit_id != lag(pit_id, 1) ~ "Yes",
    TRUE ~ "N/A")) 



Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  mutate(sub_made = case_when(
    leadoff_fl == "TRUE" & inn_ct == "1" ~ "N/A",
    pit_id == lag(pit_id, 1) ~ "No",
    pit_id != lag(pit_id, 1) ~ "Yes",
    TRUE ~ "N/A"
  ))


Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  mutate(sub_strategy = case_when(
    sub_made == "Yes" & pit_id == "almoy001" ~ "rest for others",
    sub_made == "Yes" & pit_id == "daviw001" ~ "xwoba, xfip, baa",
    sub_made == "Yes" & pit_id == "dunnm002" ~ "rest for others",
    sub_made == "Yes" & pit_id == "hoffj003" ~ "rest for others",
    sub_made == "Yes" & pit_id == "howas002" ~ "rest for others",
    sub_made == "Yes" & pit_id == "mcgej001" ~ "handedness",
    sub_made == "Yes" & pit_id == "musgj001" ~ "handedness",
    sub_made == "Yes" & pit_id == "obers001" ~ "xwoba, xfip, baa, era",
    sub_made == "Yes" & pit_id == "ottaa001" ~ "xwoba, xfip, baa, era",
    sub_made == "Yes" & pit_id == "pounb001" ~ "rest for others",
    sub_made == "Yes" & pit_id == "rusic001" ~ "sc, handedness",
    sub_made == "Yes" & pit_id == "shawb001" ~ "handedness",
    sub_made == "Yes" & pit_id == "oh--s001" ~ "xwoba, baa, era",
    sub_made == "Yes" & pit_id == "freek001" ~ "sc, baa",
    sub_made == "Yes" & pit_id == "marqg001" ~ "xwoba, xfip, era",
    sub_made == "Yes" & pit_id == "johnd005" ~ "rest for others",
    sub_made == "Yes" & pit_id == "senza001" ~ "sc",
    sub_made == "Yes" & pit_id == "andet002" ~ "xwoba, sc, era",
    sub_made == "Yes" & pit_id == "bettc001" ~ "sc",
    sub_made == "Yes" & pit_id == "grayj003" ~ "xfip",
    sub_made == "Yes" & pit_id == "vastj001" ~ "rest for others",
    sub_made == "No" ~ "N/A",
    sub_made == "N/A" ~ "N/A",
    TRUE ~ "N/A"))

  
Rockies2018 <- Rockies2018 %>% 
  mutate(sub_made = case_when(
    pit_id == lag(pit_id, 1) ~ "No",
    pit_id != lag(pit_id, 1) & pit_id %in% rockies_pitcher_list & lag(pit_id, 1) %in% rockies_pitcher_list & game_new_fl != "TRUE"  ~ "Yes",
    pit_id != lag(pit_id, 1) %ni% rockies_pitcher_list & lag(pit_id, 1) %ni% rockies_pitcher_list & game_new_fl != "TRUE" ~ "Yes",
    game_new_fl == "TRUE" ~ "N/A",
    TRUE ~ "No"))
    


Rockies2018 <- Rockies2018 %>% 
  mutate(sub_strategy = case_when(
    sub_made == "Yes" & pit_id == "almoy001" ~ "rest for others",
    sub_made == "Yes" & pit_id == "daviw001" ~ "xwoba",
    sub_made == "Yes" & pit_id == "dunnm002" ~ "rest for others",
    sub_made == "Yes" & pit_id == "hoffj003" ~ "rest for others",
    sub_made == "Yes" & pit_id == "howas002" ~ "rest for others",
    sub_made == "Yes" & pit_id == "mcgej001" ~ "handedness",
    sub_made == "Yes" & pit_id == "musgj001" ~ "handedness",
    sub_made == "Yes" & pit_id == "obers001" ~ "xfip",
    sub_made == "Yes" & pit_id == "ottaa001" ~ "baa",
    sub_made == "Yes" & pit_id == "pounb001" ~ "rest for others",
    sub_made == "Yes" & pit_id == "rusic001" ~ "sc",
    sub_made == "Yes" & pit_id == "shawb001" ~ "handedness",
    sub_made == "Yes" & pit_id == "oh--s001" ~ "era",
    sub_made == "Yes" & pit_id == "freek001" ~ "baa",
    sub_made == "Yes" & pit_id == "marqg001" ~ "xfip",
    sub_made == "Yes" & pit_id == "johnd005" ~ "rest for others",
    sub_made == "Yes" & pit_id == "senza001" ~ "sc",
    sub_made == "Yes" & pit_id == "andet002" ~ "sc",
    sub_made == "Yes" & pit_id == "bettc001" ~ "sc",
    sub_made == "Yes" & pit_id == "grayj003" ~ "xfip",
    sub_made == "Yes" & pit_id == "vastj001" ~ "rest for others",
    TRUE ~ "N/A"))


Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  mutate(lag_pit_id = case_when(
    sub_made == "N/A" & sub_made == "N/A" ~ "N/A",
    TRUE ~ lag(pit_id,1)
  ))


Rockies2018defenseonly$transition_value <- as.numeric(Rockies2018defenseonly$transition_value)

Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  mutate(good_or_bad = case_when(
    transition_value > 0 ~ 1,
    transition_value < 0 ~ 0
  ))


filtered_transition_value <- Rockies2018defenseonly[which(Rockies2018defenseonly$sub_strategy != "N/A"),]

transition_value_model <- glm(filtered_transition_value$good_or_bad ~ filtered_transition_value$sub_strategy, data = Rockies2018defenseonly, family = "binomial")
transition_value_model
summary(transition_value_model)


filtered_win_or_loss <- Rockies2018defenseonly[which(Rockies2018defenseonly$win_or_loss != "N/A"),]

win_or_loss_model <- glm(as.numeric(filtered_win_or_loss$win_or_loss) ~ filtered_win_or_loss$sub_strategy, data = Rockies2018defenseonly, family = "binomial")
win_or_loss_model
summary(win_or_loss_model)

as.numeric(filtered_win_or_loss$win_or_loss)

summary(Rockies2018defenseonly)

as.numeric(Rockies2018defenseonly$inn_ct)
as.numeric(Rockies2018$away_score_ct)
as.numeric(Rockies2018$home_score_ct)
as.numeric(Rockies2018defenseonly$win_or_loss)

Rockies2018 <- Rockies2018 %>% 
  mutate(win_or_loss = case_when(
    away_team_id == "COL" & game_end_fl == "TRUE" & away_score_ct > home_score_ct ~ "Win",
    away_team_id == "COL" & game_end_fl == "TRUE" & away_score_ct < home_score_ct ~ "Loss",
    away_team_id != "COL" & game_end_fl == "TRUE" & away_score_ct < home_score_ct ~ "Win",
    away_team_id != "COL" & game_end_fl == "TRUE" & away_score_ct > home_score_ct ~ "Loss",
    TRUE ~ "Other"))
    
  
  
Rockies2018defenseonly %>% 
  group_by(sub_strategy) %>% 
  summarise(mean=mean(transition_value))
    
    
    
Rockies2018defenseonlyscreenshot <- Rockies2018defenseonly %>% 
  select(game_id,inn_ct,outs_ct,resp_bat_hand_cd,resp_pit_hand_cd,resp_pit_id,base1_run_id,base2_run_id,base3_run_id,event_tx,event_cd,event_id,current_state,next_state,sub_made,sub_strategy,transition_value,win_or_loss)
    
Rockies2018defenseonlyscreenshot


handednessplot <- Rockies2018defenseonly %>% 
  group_by(game_id) %>% 
  mutate(totalmatchups = n()) %>% 
  summarise(matchups = sum(ifelse(resp_pit_hand_cd == resp_bat_hand_cd, 1, 0))) %>% 
  arrange(matchups) 

handednessplot2 <- handednessplot %>% 
  group_by(matchups) %>% 
  mutate(win_pct = (sum(case_when(win_or_loss == "1" ~ 1, TRUE ~ 0)))/(sum(case_when(win_or_loss == "1" ~ 1, TRUE ~ 1))))
  
handednessplot2 <- handednessplot2 %>% 
  select(matchups, win_pct)

handednessplot2 <- handednessplot2 %>% 
  group_by(matchups) %>% 
  summarise(winpct = mean(win_pct))


ggplot(handednessplot2, aes(matchups,winpct)) + geom_col(fill = "#0099f9") + scale_y_continuous(labels = percent_format()) +  geom_hline(yintercept = mean(handednessplot2$winpct), linetype = "dashed", size = .5, alpha = .5) + labs(x="Total Matchups",y="Win %")





