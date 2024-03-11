filtered_transition_value <- Rockies2018defenseonly[which(Rockies2018defenseonly$sub_strategy != "N/A"),]

transition_value_model <- glm(filtered_transition_value$good_or_bad ~ filtered_transition_value$sub_strategy, data = Rockies2018defenseonly, family = "binomial")
transition_value_model
summary(transition_value_model)

tvaluemodel <- lm(filtered_transition_value$transition_value ~ filtered_transition_value$sub_strategy, data=Rockies2018defenseonly)
summary(tvaluemodel)


ggplot(filtered_transition_value, aes(sub_strategy,transition_value)) + geom_point(size=4) + stat_smooth(method="lm") + theme_classic()

filtered_win_or_loss <- Rockies2018defenseonly[which(Rockies2018defenseonly$win_or_loss != "N/A"),]

win_or_loss_model <- glm(as.numeric(filtered_win_or_loss$win_or_loss) ~ filtered_win_or_loss$sub_strategy + filtered_win_or_loss$away_score_ct + filtered_win_or_loss$home_score_ct + filtered_win_or_loss$inn_ct + filtered_win_or_loss$current_state, data = Rockies2018defenseonly, family = "binomial")
win_or_loss_model
summary(win_or_loss_model)

plot(win_or_loss_model)


next_state_model <- multinom(filtered_transition_value$next_state ~ filtered_transition_value$sub_strategy + filtered_transition_value$current_state, data = Rockies2018defenseonly)
next_state_model
summary(next_state_model)
coef(next_state_model)



as.numeric(filtered_win_or_loss$win_or_loss)
cor(as.numeric(Rockies2018defenseonly))

nextstatemodel <- lm(filtered_transition_value$transition_value ~ filtered_transition_value$sub_strategy, data = clean_Rockies2018defenseonly)
summary(nextstatemodel)

clean_Rockies2018defenseonly <- na.omit(Rockies2018defenseonly)
