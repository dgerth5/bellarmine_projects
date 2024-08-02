library(tidyverse)
library(relaimpo)
library(lme4)
library(mixedup)

hitting <- softballR::load_ncaa_softball_playerbox(season = 2024, category = "Hitting")
scoreboard <- softballR::load_ncaa_softball_scoreboard(season = 2024) %>%
  dplyr::select(game_id,home_team)

# series level stats

hit_mod_df <- hitting %>%
  left_join(scoreboard, by = "game_id") %>%
  group_by(opponent, home_team, team, player) %>%
  summarise(tot_ab = sum(ab),
            tot_h = sum(h),
            tot_2b = sum(x2b),
            tot_3b = sum(x3b),
            tot_hr = sum(hr),
            tot_bb = sum(bb),
            tot_k = sum(k),
            tot_sb = sum(sb),
            tot_cs = sum(cs),
            tot_go = sum(go),
            tot_fo = sum(fo),
            tot_sac = sum(sf) + sum(sh)) %>%
  filter(tot_ab > 0) %>%
  mutate(tot_pa = tot_ab + tot_sac + tot_bb,
         tot_gap_pow = tot_2b + tot_3b,
         weight_pow = tot_2b*2 + tot_3b*3 + tot_hr*4,
         player_id = paste0(team, "-", player)) %>%
  ungroup() %>%
  group_by(player_id) %>%
  filter(n() > 5) %>% 
  ungroup()

bb_mod <- lmer(tot_bb ~ tot_pa + (1|home_team) + (1|opponent) + (1|player_id),
                 data = hit_mod_df,
                 control=lmerControl(optimizer = "nloptwrap"))

k_mod <- lmer(tot_k ~ tot_pa + (1|home_team) + (1|opponent) + (1|player_id),
              data = hit_mod_df,
              control=lmerControl(optimizer = "nloptwrap"))

hit_mod <- lmer(tot_h ~ tot_pa + (1|home_team) + (1|opponent) + (1|player_id),
                data = hit_mod_df,
                control=lmerControl(optimizer = "nloptwrap"))

pwr_mod <- lmer(weight_pow ~ tot_h + (1|home_team) + (1|opponent) + (1|player_id),
                data = hit_mod_df,
                control=lmerControl(optimizer = "nloptwrap"))

k_effects <- extract_random_effects(k_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("K_Rat" = value, "K_SE" = se)
bb_effects <- extract_random_effects(bb_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("BB_Rat" = value, "BB_SE" = se)
hit_effects <- extract_random_effects(hit_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("Hit_Rat" = value, "Hit_SE" = se)
pow_effects <- extract_random_effects(pwr_mod) %>% filter(group_var == "player_id") %>% dplyr::select(group, value, se) %>% rename("Pwr_Rat" = value, "Pwr_SE" = se)

combined_effects <- k_effects %>%
  left_join(bb_effects, by = "group") %>%
  left_join(hit_effects, by = "group") %>%
  left_join(pow_effects, by = "group") %>%
  mutate(OVR = K_Rat*.25 + BB_Rat*.25 + Hit_Rat*.25 + Pwr_Rat*.25)

summary(combined_effects)
