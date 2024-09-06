library(data.table)
library(tidyverse)
library(readr)

df <- fread("C:/Users/david/Downloads/TM_2024_reg_szn (1).csv")

df_smry <- df %>%
  group_by(Pitcher, AutoPitchType) %>%
  summarise(pitches_thrown = n(),
            velo = round(mean(RelSpeed, na.rm = TRUE),1),
            x_rel = round(mean(RelSide, na.rm = TRUE),1),
            z_rel = round(mean(RelHeight, na.rm = TRUE),1),
            hmov = round(mean(HorzBreak, na.rm = TRUE),1),
            vmov = round(mean(InducedVertBreak, na.rm = TRUE),1),
            spinrate = round(mean(SpinRate, na.rm = TRUE),0),
            ext = round(mean(Extension, na.rm = TRUE),1),
            vaa = round(mean(VertApprAngle, na.rm = TRUE),1),
            haa = round(mean(HorzApprAngle, na.rm = TRUE),1)) %>%
  drop_na() %>%
  ungroup() %>%
  group_by(Pitcher) %>%
  arrange(-pitches_thrown)

df_tot <- df %>%
  select(Pitcher, AutoPitchType, RelSpeed, InducedVertBreak, HorzBreak) %>%
  rename("Pitch" = AutoPitchType, "Velo" = RelSpeed) %>%
  drop_na()

write_csv(df_smry, "pitcher_df_smry.csv")
write_csv(df_tot, "pitcher_df_tot.csv")
 