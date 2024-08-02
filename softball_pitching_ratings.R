library(tidyverse)
library(relaimpo)
library(lme4)
library(mixedup)

pitching <- softballR::load_ncaa_softball_playerbox(season = 2024, category = "Pitching")
scoreboard <- softballR::load_ncaa_softball_scoreboard(season = 2024) %>%
  dplyr::select(game_id,home_team)

pitch_mod_df <- pitching %>%
  left_join(scoreboard, by = "game_id") %>%
  mutate(k_per = so / bf,
         bb_per = bb / bf,
         fo_per = fo / bf,
         hr_per = hr_a / bf,
         era = er / ip * 9,
         player_id = paste0(team, "-", player)) %>%
  drop_na(er, ip, home_team, opponent) %>%
  group_by(player_id) %>%
  filter(n() > 5) %>% 
  ungroup()

so_mod <- lmer(so ~ bf + (1|home_team) + (1|opponent) + (1|player_id),
               data = pitch_mod_df,
               control=lmerControl(optimizer = "nloptwrap"))

bb_mod <- lmer(bb ~ bf + (1|home_team) + (1|opponent) + (1|player_id),
               data = pitch_mod_df,
               control=lmerControl(optimizer = "nloptwrap"))

go_mod <- lmer(go ~ bf + (1|home_team) + (1|opponent) + (1|player_id),
               data = pitch_mod_df,
               control=lmerControl(optimizer = "nloptwrap"))

so_effects <- extract_random_effects(so_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("K_Rat" = value, "K_SE" = se)
bb_effects <- extract_random_effects(bb_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("BB_Rat" = value, "BB_SE" = se)
go_effects <- extract_random_effects(go_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("GO_Rat" = value, "GO_SE" = se)

combined_effects <- so_effects %>%
  left_join(bb_effects, by = "group") %>%
  left_join(go_effects, by = "group")

pitch_smry <- pitching %>%
  group_by(player, team) %>%
  summarise(tot_k = sum(so), 
            tot_bb = sum(bb),
            tot_go = sum(go),
            tot_er = sum(er),
            tot_bf = sum(bf),
            tot_ip = sum(ip)) %>%
  mutate(era = tot_er / tot_ip * 9,
         k_per = tot_k / tot_bf,
         bb_per = tot_bb / tot_bf,
         go_per = tot_go / tot_bf,
         player_id = paste0(team, "-", player)) %>%
  left_join(combined_effects, by = c("player_id" = "group"))

era_mod2 <- lm(era ~ K_Rat + BB_Rat + GO_Rat, data = pitch_smry)

rel_effects <- calc.relimp(era_mod2, rela=TRUE)
rel_effects

final_rats <- pitch_smry %>%
  drop_na() %>%
  mutate(OVR = .58*K_Rat - .36*BB_Rat + (1-.58-.36)*GO_Rat) %>%
  dplyr::select(team, K_Rat, BB_Rat, GO_Rat, OVR)