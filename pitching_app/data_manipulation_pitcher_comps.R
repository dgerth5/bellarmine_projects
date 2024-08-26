library(tidyverse)
library(data.table)

df <- fread("pitching_app/TM_2024_reg_szn (1).csv")

colnames(df)
sort(unique(df$AutoPitchType))

metrics_smry <- df %>%
  filter(Level == "D1") %>%
  filter(!AutoPitchType %in% c("Other")) %>%
  drop_na(PitcherThrows, RelHeight, RelSide, Extension, InducedVertBreak, HorzBreak, AutoPitchType) %>%
  group_by(PitcherId, Pitcher, AutoPitchType, PitcherThrows) %>%
  summarise(h_rel = mean(RelSide),
            v_rel = mean(RelHeight),
            ext = mean(Extension),
            velo = mean(RelSpeed),
            h_break = mean(HorzBreak),
            v_break = mean(InducedVertBreak),
            times_thrown = n()) %>%
  ungroup() %>%
  group_by(PitcherId, Pitcher) %>%
  mutate(usage = times_thrown / sum(times_thrown)*100) %>%
  filter(usage > 5) %>%
  ungroup()

primary_fb <- metrics_smry %>%
  filter(AutoPitchType %in% c("Four-Seam","Sinker")) %>%
  group_by(PitcherId) %>%
  slice_max(order_by = usage, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(primary_fb = AutoPitchType) %>%
  select(PitcherId, primary_fb) 

df1 <- metrics_smry %>%
  left_join(primary_fb, by = "PitcherId") %>%
  drop_na()

# averages

rel_pt <- df1 %>%
  filter(PitcherThrows != "Both") %>%
  group_by(PitcherThrows) %>%
  summarise(h_rel_mean = mean(h_rel),
            h_rel_sd = sd(h_rel),
            v_rel_mean = mean(v_rel),
            v_rel_sd = sd(h_rel),
            ext_mean = mean(ext),
            ext_sd = sd(ext))

pitch_dat <- df1 %>%
  filter(PitcherThrows != "Both") %>%
  group_by(PitcherThrows, AutoPitchType) %>%
  summarise(velo_mean = mean(velo),
            velo_sd = sd(velo),
            h_break_mean = mean(h_break),
            h_break_sd = sd(h_break),
            v_break_mean = mean(v_break),
            v_break_sd = sd(v_break))

norm_df <- df1 %>%
  left_join(rel_pt, by = "PitcherThrows") %>%
  left_join(pitch_dat, by = c("PitcherThrows", "AutoPitchType")) %>%
  mutate(n_hrp = (h_rel - h_rel_mean) / h_rel_sd,
         n_vrp = (v_rel - v_rel_mean) / v_rel_sd,
         n_ext = (ext - ext_mean) / ext_sd,
         n_velo = (velo - velo_mean) / velo_sd,
         n_hb = (h_break - h_break_mean) / h_break_sd,
         n_vb = (v_break - v_break_mean) / v_break_sd)

saveRDS(list(rel_pt = rel_pt, pitch_dat = pitch_dat, norm_df = norm_df), "similarity_data.RDS")
