Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  mutate(transition_value = case_when(
    current_state == "1" & next_state == "1" ~ "1",
    current_state == "1" & next_state == "2" ~ "0.41",
    current_state == "1" & next_state == "3" ~ "0.64",
    current_state == "1" & next_state == "4" ~ "0.9",
    current_state == "1" & next_state == "9" ~ "-0.24",
    current_state == "2" & next_state == "1" ~ "1.59",
    current_state == "2" & next_state == "2" ~ "1",
    current_state == "2" & next_state == "3" ~ "1.23",
    current_state == "2" & next_state == "4" ~ "1.49",
    current_state == "2" & next_state == "5" ~ "0.61",
    current_state == "2" & next_state == "6" ~ "0.86",
    current_state == "2" & next_state == "7" ~ "1.1",
    current_state == "2" & next_state == "9" ~ "0.35",
    current_state == "2" & next_state == "10" ~ "-0.38",
    current_state == "2" & next_state == "11" ~ "-0.22",
    current_state == "2" & next_state == "12" ~ "0.06",
    current_state == "2" & next_state == "17" ~ "-0.83",
    current_state == "3" & next_state == "1" ~ "1.36",
    current_state == "3" & next_state == "2" ~ "0.77",
    current_state == "3" & next_state == "3" ~ "1",
    current_state == "3" & next_state == "4" ~ "1.26",
    current_state == "3" & next_state == "5" ~ "0.38",
    current_state == "3" & next_state == "6" ~ "0.63",
    current_state == "3" & next_state == "7" ~ "0.87",
    current_state == "3" & next_state == "9" ~ "0.12",
    current_state == "3" & next_state == "10" ~ "-0.38",
    current_state == "3" & next_state == "11" ~ "-0.22",
    current_state == "3" & next_state == "12" ~ "0.06", 
    current_state == "3" & next_state == "17" ~ "-1.06",
    current_state == "4" & next_state == "1" ~ "1.1",
    current_state == "4" & next_state == "2" ~ "0.51",
    current_state == "4" & next_state == "3" ~ "0.74",
    current_state == "4" & next_state == "4" ~ "1",
    current_state == "4" & next_state == "6" ~ "0.37",
    current_state == "4" & next_state == "7" ~ "0.61",
    current_state == "4" & next_state == "9" ~ "-0.14",
    current_state == "4" & next_state == "10" ~ "-0.87",
    current_state == "4" & next_state == "11" ~ "-0.71",
    current_state == "4" & next_state == "12" ~ "-0.43",
    current_state == "4" & next_state == "17" ~ "-1.32",
    current_state == "5" & next_state == "1" ~ "1.98",
    current_state == "5" & next_state == "2" ~ "1.39",
    current_state == "5" & next_state == "3" ~ "1.62",
    current_state == "5" & next_state == "4" ~ "1.88",
    current_state == "5" & next_state == "5" ~ "1",
    current_state == "5" & next_state == "6" ~ "1.25",
    current_state == "5" & next_state == "7" ~ "1.49",
    current_state == "5" & next_state == "8" ~ "0.77",
    current_state == "5" & next_state == "9" ~ "0.74",
    current_state == "5" & next_state == "10" ~ "0.01",
    current_state == "5" & next_state == "11" ~ "0.17",
    current_state == "5" & next_state == "12" ~ "0.45",
    current_state == "5" & next_state == "13" ~ "-0.55",
    current_state == "5" & next_state == "14" ~ "-0.32",
    current_state == "5" & next_state == "15" ~ "-0.13",
    current_state == "5" & next_state == "17" ~ "-0.44",
    current_state == "5" & next_state == "18" ~ "-1.31",
    current_state == "5" & next_state == "19" ~ "-1.22",
    current_state == "5" & next_state == "20" ~ "-1.17",
    current_state == "5" & next_state == "25" ~ "-1.55",
    current_state == "6" & next_state == "1" ~ "1.73",
    current_state == "6" & next_state == "2" ~ "1.14",
    current_state == "6" & next_state == "3" ~ "1.37",
    current_state == "6" & next_state == "4" ~ "1.63",
    current_state == "6" & next_state == "5" ~ "0.75",
    current_state == "6" & next_state == "6" ~ "1",
    current_state == "6" & next_state == "7" ~ "1.24",
    current_state == "6" & next_state == "8" ~ "0.52",
    current_state == "6" & next_state == "9" ~ "0.49", 
    current_state == "6" & next_state == "10" ~ "-0.24",
    current_state == "6" & next_state == "11" ~ "-0.08",
    current_state == "6" & next_state == "12" ~ "0.2",
    current_state == "6" & next_state == "13" ~ "-0.8",
    current_state == "6" & next_state == "14" ~ "-0.57",
    current_state == "6" & next_state == "15" ~ "-0.38",
    current_state == "6" & next_state == "17" ~ "-0.69",
    current_state == "6" & next_state == "18" ~ "-1.56",
    current_state == "6" & next_state == "19" ~ "-1.47",
    current_state == "6" & next_state == "20" ~ "-1.42",
    current_state == "6" & next_state == "25" ~ "-1.8",
    current_state == "7" & next_state == "1" ~ "0.49",
    current_state == "7" & next_state == "2" ~ "0.9",
    current_state == "7" & next_state == "3" ~ "1.13",
    current_state == "7" & next_state == "4" ~ "1.39",
    current_state == "7" & next_state == "5" ~ "0.51",
    current_state == "7" & next_state == "6" ~ "0.76",
    current_state == "7" & next_state == "7" ~ "1",
    current_state == "7" & next_state == "8" ~ "0.28",
    current_state == "7" & next_state == "9" ~ "0.25",
    current_state == "7" & next_state == "10" ~ "-0.48",
    current_state == "7" & next_state == "11" ~ "-0.32",
    current_state == "7" & next_state == "12" ~ "-0.04",
    current_state == "7" & next_state == "13" ~ "-1.04",
    current_state == "7" & next_state == "14" ~ "-0.81",
    current_state == "7" & next_state == "15" ~ "-0.62",
    current_state == "7" & next_state == "17" ~ "-0.93",
    current_state == "7" & next_state == "18" ~ "-1.8",
    current_state == "7" & next_state == "19" ~ "-1.71",
    current_state == "7" & next_state == "20" ~ "-1.66",
    current_state == "7" & next_state == "25" ~ "-2.04",
    current_state == "8" & next_state == "1" ~ "2.21",
    current_state == "8" & next_state == "2" ~ "1.62",
    current_state == "8" & next_state == "3" ~ "1.85",
    current_state == "8" & next_state == "4" ~ "2.11",
    current_state == "8" & next_state == "5" ~ "1.23",
    current_state == "8" & next_state == "6" ~ "1.48",
    current_state == "8" & next_state == "7" ~ "1.72",
    current_state == "8" & next_state == "8" ~ "1",
    current_state == "8" & next_state == "9" ~ "0.97", 
    current_state == "8" & next_state == "10" ~ "0.24",
    current_state == "8" & next_state == "11" ~ "0.4",
    current_state == "8" & next_state == "12" ~ "0.68",
    current_state == "8" & next_state == "13" ~ "-0.32",
    current_state == "8" & next_state == "14" ~ "-0.09",
    current_state == "8" & next_state == "15" ~ "0.1",
    current_state == "8" & next_state == "16" ~ "-0.69",
    current_state == "8" & next_state == "17" ~ "-0.21",
    current_state == "8" & next_state == "18" ~ "-1.08",
    current_state == "8" & next_state == "19" ~ "-0.99",
    current_state == "8" & next_state == "20" ~ "-0.94",
    current_state == "8" & next_state == "21" ~ "-1.86",
    current_state == "8" & next_state == "22" ~ "-1.78",
    current_state == "8" & next_state == "23" ~ "-1.72",
    current_state == "8" & next_state == "25" ~ "-2.32",
    current_state == "8" & next_state == "26" ~ "-1.32",
    current_state == "9" & next_state == "9" ~ "1",
    current_state == "9" & next_state == "10" ~ "0.27",
    current_state == "9" & next_state == "11" ~ "0.43",
    current_state == "9" & next_state == "12" ~ "0.71",
    current_state == "9" & next_state == "17" ~ "-0.18",
    current_state == "10" & next_state == "9" ~ "1.73",
    current_state == "10" & next_state == "10" ~ "1",
    current_state == "10" & next_state == "11" ~ "1.16",
    current_state == "10" & next_state == "12" ~ "1.44",
    current_state == "10" & next_state == "13" ~ "0.44",
    current_state == "10" & next_state == "14" ~ "0.67",
    current_state == "10" & next_state == "15" ~ "0.86",
    current_state == "10" & next_state == "17" ~ "0.55",
    current_state == "10" & next_state == "18" ~ "-0.32",
    current_state == "10" & next_state == "19" ~ "-0.23",
    current_state == "10" & next_state == "20" ~ "-0.18",
    current_state == "10" & next_state == "25" ~ "-0.56",
    current_state == "11" & next_state == "9" ~ "1.57",
    current_state == "11" & next_state == "10" ~ "0.84",
    current_state == "11" & next_state == "11" ~ "1",
    current_state == "11" & next_state == "12" ~ "1.28",
    current_state == "11" & next_state == "13" ~ "0.28",
    current_state == "11" & next_state == "14" ~ "0.51",
    current_state == "11" & next_state == "15" ~ "0.7", 
    current_state == "11" & next_state == "17" ~ "0.39",
    current_state == "11" & next_state == "18" ~ "-0.48",
    current_state == "11" & next_state == "19" ~ "-0.39",
    current_state == "11" & next_state == "20" ~ "-0.34",
    current_state == "11" & next_state == "25" ~ "-0.72",
    current_state == "12" & next_state == "9" ~ "1.29",
    current_state == "12" & next_state == "10" ~ "0.57",
    current_state == "12" & next_state == "11" ~ "0.72",
    current_state == "12" & next_state == "12" ~ "1",
    current_state == "12" & next_state == "14" ~ "0.23",
    current_state == "12" & next_state == "15" ~ "0.42",
    current_state == "12" & next_state == "17" ~ "0.11",
    current_state == "12" & next_state == "18" ~ "-0.76",
    current_state == "12" & next_state == "19" ~ "-0.67",
    current_state == "12" & next_state == "20" ~ "-0.62",
    current_state == "12" & next_state == "25" ~ "-1",
    current_state == "13" & next_state == "9" ~ "2.29",
    current_state == "13" & next_state == "10" ~ "1.56",
    current_state == "13" & next_state == "11" ~ "1.72",
    current_state == "13" & next_state == "12" ~ "2",
    current_state == "13" & next_state == "13" ~ "1",
    current_state == "13" & next_state == "14" ~ "1.23",
    current_state == "13" & next_state == "15" ~ "1.42",
    current_state == "13" & next_state == "16" ~ "0.63",
    current_state == "13" & next_state == "17" ~ "1.11",
    current_state == "13" & next_state == "18" ~ "0.24",
    current_state == "13" & next_state == "19" ~ "0.33",
    current_state == "13" & next_state == "20" ~ "0.38",
    current_state == "13" & next_state == "21" ~ "-0.54",
    current_state == "13" & next_state == "22" ~ "-0.46",
    current_state == "13" & next_state == "23" ~ "-0.4",
    current_state == "13" & next_state == "25" ~ "-1",
    current_state == "13" & next_state == "26" ~ "0",
    current_state == "14" & next_state == "9" ~ "2.06",
    current_state == "14" & next_state == "10" ~ "1.33",
    current_state == "14" & next_state == "11" ~ "1.49",
    current_state == "14" & next_state == "12" ~ "1.77",
    current_state == "14" & next_state == "13" ~ "0.77",
    current_state == "14" & next_state == "14" ~ "1",
    current_state == "14" & next_state == "15" ~ "1.19", 
    current_state == "14" & next_state == "16" ~ "0.4",
    current_state == "14" & next_state == "17" ~ "0.88",
    current_state == "14" & next_state == "18" ~ "0.01",
    current_state == "14" & next_state == "19" ~ "0.1",
    current_state == "14" & next_state == "20" ~ "0.15",
    current_state == "14" & next_state == "21" ~ "-0.77",
    current_state == "14" & next_state == "22" ~ "-0.69",
    current_state == "14" & next_state == "23" ~ "-0.63",
    current_state == "14" & next_state == "25" ~ "-1.23",
    current_state == "14" & next_state == "26" ~ "-0.23",
    current_state == "15" & next_state == "9" ~ "1.87",
    current_state == "15" & next_state == "10" ~ "1.14",
    current_state == "15" & next_state == "11" ~ "1.3",
    current_state == "15" & next_state == "12" ~ "1.58",
    current_state == "15" & next_state == "13" ~ "0.58",
    current_state == "15" & next_state == "14" ~ "0.81",
    current_state == "15" & next_state == "15" ~ "1",
    current_state == "15" & next_state == "16" ~ "0.21",
    current_state == "15" & next_state == "17" ~ "0.69",
    current_state == "15" & next_state == "18" ~ "-0.18",
    current_state == "15" & next_state == "19" ~ "-0.09",
    current_state == "15" & next_state == "20" ~ "-0.04",
    current_state == "15" & next_state == "21" ~ "-0.96",
    current_state == "15" & next_state == "22" ~ "-0.88",
    current_state == "15" & next_state == "23" ~ "-0.82",
    current_state == "15" & next_state == "25" ~ "-1.42",
    current_state == "15" & next_state == "26" ~ "-0.42",
    current_state == "16" & next_state == "9" ~ "2.66",
    current_state == "16" & next_state == "10" ~ "1.93",
    current_state == "16" & next_state == "11" ~ "2.09",
    current_state == "16" & next_state == "12" ~ "2.37",
    current_state == "16" & next_state == "13" ~ "1.37",
    current_state == "16" & next_state == "14" ~ "1.6",
    current_state == "16" & next_state == "15" ~ "1.79",
    current_state == "16" & next_state == "16" ~ "1",
    current_state == "16" & next_state == "17" ~ "1.48",
    current_state == "16" & next_state == "18" ~ "0.61",
    current_state == "16" & next_state == "19" ~ "0.7",
    current_state == "16" & next_state == "20" ~ "0.75",
    current_state == "16" & next_state == "21" ~ "-0.17",
    current_state == "16" & next_state == "22" ~ "-0.09",
    current_state == "16" & next_state == "23" ~ "-0.03",
    current_state == "16" & next_state == "24" ~ "0.86",
    current_state == "16" & next_state == "25" ~ "-1.63",
    current_state == "16" & next_state == "26" ~ "-0.63",
    current_state == "16" & next_state == "27" ~ "0.37",
    current_state == "17" & next_state == "17" ~ "1",
    current_state == "17" & next_state == "18" ~ "0.13",
    current_state == "17" & next_state == "19" ~ "0.22",
    current_state == "17" & next_state == "20" ~ "0.27",
    current_state == "17" & next_state == "25" ~ "-0.11",
    current_state == "18" & next_state == "17" ~ "1.87",
    current_state == "18" & next_state == "18" ~ "1",
    current_state == "18" & next_state == "19" ~ "0.91",
    current_state == "18" & next_state == "20" ~ "0.86",
    current_state == "18" & next_state == "21" ~ "0.22",
    current_state == "18" & next_state == "22" ~ "0.3",
    current_state == "18" & next_state == "23" ~ "0.36",
    current_state == "18" & next_state == "25" ~ "-0.24",
    current_state == "18" & next_state == "26" ~ "0.76",
    current_state == "19" & next_state == "17" ~ "1.78",
    current_state == "19" & next_state == "18" ~ ".91",
    current_state == "19" & next_state == "19" ~ "1",
    current_state == "19" & next_state == "20" ~ "1.05",
    current_state == "19" & next_state == "21" ~ "0.13",
    current_state == "19" & next_state == "22" ~ "0.21",
    current_state == "19" & next_state == "23" ~ "0.27",
    current_state == "19" & next_state == "25" ~ "-0.33",
    current_state == "19" & next_state == "26" ~ "0.67",
    current_state == "20" & next_state == "17" ~ "1.73",
    current_state == "20" & next_state == "18" ~ "0.86",
    current_state == "20" & next_state == "19" ~ "0.95",
    current_state == "20" & next_state == "20" ~ "1",
    current_state == "20" & next_state == "22" ~ "0.16",
    current_state == "20" & next_state == "23" ~ "0.22",
    current_state == "20" & next_state == "25" ~ "-0.38",
    current_state == "20" & next_state == "26" ~ "0.62",
    current_state == "21" & next_state == "17" ~ "2.65",
    current_state == "21" & next_state == "18" ~ "1.78",
    current_state == "21" & next_state == "19" ~ "1.87",
    current_state == "21" & next_state == "20" ~ "1.92",
    current_state == "21" & next_state == "21" ~ "1",
    current_state == "21" & next_state == "22" ~ "0.92",
    current_state == "21" & next_state == "23" ~ "0.86",
    current_state == "21" & next_state == "24" ~ "0.31",
    current_state == "21" & next_state == "25" ~ "-0.46",
    current_state == "21" & next_state == "26" ~ "0.54",
    current_state == "21" & next_state == "27" ~ "1.54",
    current_state == "22" & next_state == "17" ~ "2.51",
    current_state == "22" & next_state == "18" ~ "1.64",
    current_state == "22" & next_state == "19" ~ "1.73",
    current_state == "22" & next_state == "20" ~ "1.78",
    current_state == "22" & next_state == "21" ~ "0.92",
    current_state == "22" & next_state == "22" ~ "1",
    current_state == "22" & next_state == "23" ~ "1.06",
    current_state == "22" & next_state == "24" ~ "0.23",
    current_state == "22" & next_state == "25" ~ "-0.54",
    current_state == "22" & next_state == "26" ~ "0.46",
    current_state == "22" & next_state == "27" ~ "1.46",
    current_state == "23" & next_state == "17" ~ "2.51",
    current_state == "23" & next_state == "18" ~ "1.64",
    current_state == "23" & next_state == "19" ~ "1.73",
    current_state == "23" & next_state == "20" ~ "1.78",
    current_state == "23" & next_state == "21" ~ "0.86",
    current_state == "23" & next_state == "22" ~ "0.94",
    current_state == "23" & next_state == "23" ~ "1",
    current_state == "23" & next_state == "24" ~ "0.17",
    current_state == "23" & next_state == "25" ~ "-0.6",
    current_state == "23" & next_state == "26" ~ "0.4",
    current_state == "23" & next_state == "27" ~ "1.4",
    current_state == "24" & next_state == "17" ~ "3.34",
    current_state == "24" & next_state == "18" ~ "2.47",
    current_state == "24" & next_state == "19" ~ "2.56",
    current_state == "24" & next_state == "20" ~ "2.61",
    current_state == "24" & next_state == "21" ~ "1.69",
    current_state == "24" & next_state == "22" ~ "1.77",
    current_state == "24" & next_state == "23" ~ "1.83",
    current_state == "24" & next_state == "24" ~ "1",
    current_state == "24" & next_state == "25" ~ "-0.77",
    current_state == "24" & next_state == "26" ~ "0.23",
    current_state == "24" & next_state == "27" ~ "1.23",
    current_state == "24" & next_state == "28" ~ "2.23",
    TRUE ~ "N/A"))


distinct_games <- Rockies2018defenseonly %>% 
  distinct(game_id)


Rockies2018defenseonly <- Rockies2018defenseonly %>% 
  mutate(win_or_loss = case_when(
    game_id == "ANA201808270" ~ "0",
    game_id == "ANA201808280" ~ "1",
    game_id == "ARI201803290" ~ "0",
    game_id == "ARI201803300" ~ "0",
    game_id == "ARI201803310" ~ "1",
    game_id == "ARI201807200" ~ "1",
    game_id == "ARI201807210" ~ "1",
    game_id == "ARI201807220" ~ "0",
    game_id == "ARI201809210" ~ "1",
    game_id == "ARI201809220" ~ "1",
    game_id == "ARI201809230" ~ "1",
    game_id == "ATL201808160" ~ "1",
    game_id == "ATL201808170" ~ "1",
    game_id == "ATL201808180" ~ "1",
    game_id == "ATL201808190" ~ "1",
    game_id == "CHN201804300" ~ "0",
    game_id == "CHN201805010" ~ "1",
    game_id == "CHN201805020" ~ "1",
    game_id == "CIN201806050" ~ "1",
    game_id == "CIN201806060" ~ "1",
    game_id == "CIN201806070" ~ "0",
    game_id == "COL201804060" ~ "0",
    game_id == "COL201804070" ~ "1",
    game_id == "COL201804080" ~ "0",
    game_id == "COL201804090" ~ "0",
    game_id == "COL201804100" ~ "0",
    game_id == "COL201804110" ~ "1",
    game_id == "COL201804200" ~ "0",
    game_id == "COL201804210" ~ "1",
    game_id == "COL201804220" ~ "0",
    game_id == "COL201804230" ~ "0",
    game_id == "COL201804240" ~ "1",
    game_id == "COL201804250" ~ "1",
    game_id == "COL201805080" ~ "1",
    game_id == "COL201805090" ~ "0",
    game_id == "COL201805100" ~ "0",
    game_id == "COL201805110" ~ "0",
    game_id == "COL201805120" ~ "1",
    game_id == "COL201805130" ~ "0",
    game_id == "COL201805250" ~ "1",
    game_id == "COL201805260" ~ "0",
    game_id == "COL201805270" ~ "1",
    game_id == "COL201805280" ~ "1",
    game_id == "COL201805290" ~ "1",
    game_id == "COL201805300" ~ "0",
    game_id == "COL201806010" ~ "0",
    game_id == "COL201806020" ~ "0",
    game_id == "COL201806030" ~ "0",
    game_id == "COL201806080" ~ "0",
    game_id == "COL201806090" ~ "0",
    game_id == "COL201806100" ~ "0",
    game_id == "COL201806180" ~ "0",
    game_id == "COL201806190" ~ "1",
    game_id == "COL201806200" ~ "1",
    game_id == "COL201804210" ~ "1",
    game_id == "COL201804220" ~ "1",
    game_id == "COL201804230" ~ "0",
    game_id == "COL201804240" ~ "0",
    game_id == "COL201807020" ~ "1",
    game_id == "COL201807030" ~ "1",
    game_id == "COL201807040" ~ "1",
    game_id == "COL201807100" ~ "0",
    game_id == "COL201807110" ~ "1",
    game_id == "COL201807120" ~ "1",
    game_id == "COL201807130" ~ "1",
    game_id == "COL201807140" ~ "1",
    game_id == "COL201807150" ~ "1",
    game_id == "COL201807240" ~ "0",
    game_id == "COL201807250" ~ "1",
    game_id == "COL201807270" ~ "1",
    game_id == "COL201807280" ~ "1",
    game_id == "COL201807290" ~ "1",
    game_id == "COL201808060" ~ "1",
    game_id == "COL201808070" ~ "0",
    game_id == "COL201808080" ~ "0",
    game_id == "COL201808090" ~ "0",
    game_id == "COL201808100" ~ "1",
    game_id == "COL201808110" ~ "1",
    game_id == "COL201808120" ~ "1",
    game_id == "COL201808210" ~ "0",
    game_id == "COL201808220" ~ "1",
    game_id == "COL201808230" ~ "1",
    game_id == "COL201808240" ~ "0",
    game_id == "COL201808250" ~ "1",
    game_id == "COL201808260" ~ "0",
    game_id == "COL201809030" ~ "1",
    game_id == "COL201809040" ~ "1",
    game_id == "COL201809050" ~ "1",
    game_id == "COL201809070" ~ "0",
    game_id == "COL201809080" ~ "1",
    game_id == "COL201809090" ~ "0",
    game_id == "COL201809100" ~ "1",
    game_id == "COL201809110" ~ "0",
    game_id == "COL201809120" ~ "1",
    game_id == "COL201809130" ~ "1",
    game_id == "COL201809240" ~ "1",
    game_id == "COL201809250" ~ "1",
    game_id == "COL201809260" ~ "1",
    game_id == "COL201809270" ~ "1",
    game_id == "COL201809280" ~ "1",
    game_id == "COL201809290" ~ "0",
    game_id == "COL201809300" ~ "1",
    game_id == "HOU201808140" ~ "1",
    game_id == "HOU201808150" ~ "0",
    game_id == "LAN201805210" ~ "1",
    game_id == "LAN201805220" ~ "0",
    game_id == "LAN201805230" ~ "0",
    game_id == "LAN201806290" ~ "1",
    game_id == "LAN201806300" ~ "1",
    game_id == "LAN201807010" ~ "0",
    game_id == "LAN201809170" ~ "0",
    game_id == "LAN201809180" ~ "0",
    game_id == "LAN201809190" ~ "0",
    game_id == "LAN201810010" ~ "0",
    game_id == "MIA201804270" ~ "1",
    game_id == "MIA201804280" ~ "0",
    game_id == "MIA201804290" ~ "0",
    game_id == "MIL201808030" ~ "0",
    game_id == "MIL201808040" ~ "0",
    game_id == "MIL201808050" ~ "1",
    game_id == "NYN201805040" ~ "1",
    game_id == "NYN201805050" ~ "1",
    game_id == "NYN201805060" ~ "1",
    game_id == "PHI201806120" ~ "0",
    game_id == "PHI201806130" ~ "1",
    game_id == "PHI201806140" ~ "0",
    game_id == "PIT201804160" ~ "1",
    game_id == "PIT201804170" ~ "1",
    game_id == "PIT201804180" ~ "0",
    game_id == "SDN201804020" ~ "1",
    game_id == "SDN201804030" ~ "0",
    game_id == "SDN201804040" ~ "1",
    game_id == "SDN201804050" ~ "1",
    game_id == "SDN201805140" ~ "1",
    game_id == "SDN201805150" ~ "0",
    game_id == "SDN201808300" ~ "0",
    game_id == "SDN201808310" ~ "0",
    game_id == "SDN201809010" ~ "1",
    game_id == "SDN201809020" ~ "1",
    game_id == "SEA201807070" ~ "1",
    game_id == "SEA201807080" ~ "1",
    game_id == "SEA201807090" ~ "0",
    game_id == "SFN201805170" ~ "1",
    game_id == "SFN201805180" ~ "1",
    game_id == "SFN201805190" ~ "0",
    game_id == "SFN201805200" ~ "0",
    game_id == "SFN201806260" ~ "0",
    game_id == "SFN201806270" ~ "0",
    game_id == "SFN201806280" ~ "1",
    game_id == "SFN201809140" ~ "0",
    game_id == "SFN201809150" ~ "0",
    game_id == "SFN201809160" ~ "1",
    game_id == "SLN201807300" ~ "0",
    game_id == "SLN201807310" ~ "1",
    game_id == "SLN201808010" ~ "0",
    game_id == "SLN201808020" ~ "0",
    game_id == "TEX201806150" ~ "1",
    game_id == "TEX201806160" ~ "0",
    game_id == "TEX201806170" ~ "0",
    game_id == "WAS201804120" ~ "1",
    game_id == "WAS201804130" ~ "1",
    game_id == "WAS201804140" ~ "0",
    game_id == "WAS201804150" ~ "1",
    TRUE ~ "N/A"))





  handednessplot <- handednessplot %>% 
  group_by(game_id) %>% 
  mutate(win_or_loss = case_when(
    game_id == "ANA201808270" ~ "0",
    game_id == "ANA201808280" ~ "1",
    game_id == "ARI201803290" ~ "0",
    game_id == "ARI201803300" ~ "0",
    game_id == "ARI201803310" ~ "1",
    game_id == "ARI201807200" ~ "1",
    game_id == "ARI201807210" ~ "1",
    game_id == "ARI201807220" ~ "0",
    game_id == "ARI201809210" ~ "1",
    game_id == "ARI201809220" ~ "1",
    game_id == "ARI201809230" ~ "1",
    game_id == "ATL201808160" ~ "1",
    game_id == "ATL201808170" ~ "1",
    game_id == "ATL201808180" ~ "1",
    game_id == "ATL201808190" ~ "1",
    game_id == "CHN201804300" ~ "0",
    game_id == "CHN201805010" ~ "1",
    game_id == "CHN201805020" ~ "1",
    game_id == "CIN201806050" ~ "1",
    game_id == "CIN201806060" ~ "1",
    game_id == "CIN201806070" ~ "0",
    game_id == "COL201804060" ~ "0",
    game_id == "COL201804070" ~ "1",
    game_id == "COL201804080" ~ "0",
    game_id == "COL201804090" ~ "0",
    game_id == "COL201804100" ~ "0",
    game_id == "COL201804110" ~ "1",
    game_id == "COL201804200" ~ "0",
    game_id == "COL201804210" ~ "1",
    game_id == "COL201804220" ~ "0",
    game_id == "COL201804230" ~ "0",
    game_id == "COL201804240" ~ "1",
    game_id == "COL201804250" ~ "1",
    game_id == "COL201805080" ~ "1",
    game_id == "COL201805090" ~ "0",
    game_id == "COL201805100" ~ "0",
    game_id == "COL201805110" ~ "0",
    game_id == "COL201805120" ~ "1",
    game_id == "COL201805130" ~ "0",
    game_id == "COL201805250" ~ "1",
    game_id == "COL201805260" ~ "0",
    game_id == "COL201805270" ~ "1",
    game_id == "COL201805280" ~ "1",
    game_id == "COL201805290" ~ "1",
    game_id == "COL201805300" ~ "0",
    game_id == "COL201806010" ~ "0",
    game_id == "COL201806020" ~ "0",
    game_id == "COL201806030" ~ "0",
    game_id == "COL201806080" ~ "0",
    game_id == "COL201806090" ~ "0",
    game_id == "COL201806100" ~ "0",
    game_id == "COL201806180" ~ "0",
    game_id == "COL201806190" ~ "1",
    game_id == "COL201806200" ~ "1",
    game_id == "COL201804210" ~ "1",
    game_id == "COL201804220" ~ "1",
    game_id == "COL201804230" ~ "0",
    game_id == "COL201804240" ~ "0",
    game_id == "COL201807020" ~ "1",
    game_id == "COL201807030" ~ "1",
    game_id == "COL201807040" ~ "1",
    game_id == "COL201807100" ~ "0",
    game_id == "COL201807110" ~ "1",
    game_id == "COL201807120" ~ "1",
    game_id == "COL201807130" ~ "1",
    game_id == "COL201807140" ~ "1",
    game_id == "COL201807150" ~ "1",
    game_id == "COL201807240" ~ "0",
    game_id == "COL201807250" ~ "1",
    game_id == "COL201807270" ~ "1",
    game_id == "COL201807280" ~ "1",
    game_id == "COL201807290" ~ "1",
    game_id == "COL201808060" ~ "1",
    game_id == "COL201808070" ~ "0",
    game_id == "COL201808080" ~ "0",
    game_id == "COL201808090" ~ "0",
    game_id == "COL201808100" ~ "1",
    game_id == "COL201808110" ~ "1",
    game_id == "COL201808120" ~ "1",
    game_id == "COL201808210" ~ "0",
    game_id == "COL201808220" ~ "1",
    game_id == "COL201808230" ~ "1",
    game_id == "COL201808240" ~ "0",
    game_id == "COL201808250" ~ "1",
    game_id == "COL201808260" ~ "0",
    game_id == "COL201809030" ~ "1",
    game_id == "COL201809040" ~ "1",
    game_id == "COL201809050" ~ "1",
    game_id == "COL201809070" ~ "0",
    game_id == "COL201809080" ~ "1",
    game_id == "COL201809090" ~ "0",
    game_id == "COL201809100" ~ "1",
    game_id == "COL201809110" ~ "0",
    game_id == "COL201809120" ~ "1",
    game_id == "COL201809130" ~ "1",
    game_id == "COL201809240" ~ "1",
    game_id == "COL201809250" ~ "1",
    game_id == "COL201809260" ~ "1",
    game_id == "COL201809270" ~ "1",
    game_id == "COL201809280" ~ "1",
    game_id == "COL201809290" ~ "0",
    game_id == "COL201809300" ~ "1",
    game_id == "HOU201808140" ~ "1",
    game_id == "HOU201808150" ~ "0",
    game_id == "LAN201805210" ~ "1",
    game_id == "LAN201805220" ~ "0",
    game_id == "LAN201805230" ~ "0",
    game_id == "LAN201806290" ~ "1",
    game_id == "LAN201806300" ~ "1",
    game_id == "LAN201807010" ~ "0",
    game_id == "LAN201809170" ~ "0",
    game_id == "LAN201809180" ~ "0",
    game_id == "LAN201809190" ~ "0",
    game_id == "LAN201810010" ~ "0",
    game_id == "MIA201804270" ~ "1",
    game_id == "MIA201804280" ~ "0",
    game_id == "MIA201804290" ~ "0",
    game_id == "MIL201808030" ~ "0",
    game_id == "MIL201808040" ~ "0",
    game_id == "MIL201808050" ~ "1",
    game_id == "NYN201805040" ~ "1",
    game_id == "NYN201805050" ~ "1",
    game_id == "NYN201805060" ~ "1",
    game_id == "PHI201806120" ~ "0",
    game_id == "PHI201806130" ~ "1",
    game_id == "PHI201806140" ~ "0",
    game_id == "PIT201804160" ~ "1",
    game_id == "PIT201804170" ~ "1",
    game_id == "PIT201804180" ~ "0",
    game_id == "SDN201804020" ~ "1",
    game_id == "SDN201804030" ~ "0",
    game_id == "SDN201804040" ~ "1",
    game_id == "SDN201804050" ~ "1",
    game_id == "SDN201805140" ~ "1",
    game_id == "SDN201805150" ~ "0",
    game_id == "SDN201808300" ~ "0",
    game_id == "SDN201808310" ~ "0",
    game_id == "SDN201809010" ~ "1",
    game_id == "SDN201809020" ~ "1",
    game_id == "SEA201807070" ~ "1",
    game_id == "SEA201807080" ~ "1",
    game_id == "SEA201807090" ~ "0",
    game_id == "SFN201805170" ~ "1",
    game_id == "SFN201805180" ~ "1",
    game_id == "SFN201805190" ~ "0",
    game_id == "SFN201805200" ~ "0",
    game_id == "SFN201806260" ~ "0",
    game_id == "SFN201806270" ~ "0",
    game_id == "SFN201806280" ~ "1",
    game_id == "SFN201809140" ~ "0",
    game_id == "SFN201809150" ~ "0",
    game_id == "SFN201809160" ~ "1",
    game_id == "SLN201807300" ~ "0",
    game_id == "SLN201807310" ~ "1",
    game_id == "SLN201808010" ~ "0",
    game_id == "SLN201808020" ~ "0",
    game_id == "TEX201806150" ~ "1",
    game_id == "TEX201806160" ~ "0",
    game_id == "TEX201806170" ~ "0",
    game_id == "WAS201804120" ~ "1",
    game_id == "WAS201804130" ~ "1",
    game_id == "WAS201804140" ~ "0",
    game_id == "COL201806230" ~ "0",
    game_id == "COL201806210" ~ "1",
    game_id == "SEA201807060" ~ "1",
    game_id == "COL201806240" ~ "0",
    game_id == "COL201806220" ~ "1",
    game_id == "WAS201804150" ~ "1",
    TRUE ~ "N/A"))



