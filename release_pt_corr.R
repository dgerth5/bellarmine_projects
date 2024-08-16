library(data.table)
library(tidyverse)

df <- fread("C:/Users/david/Downloads/TM_2024_reg_szn (1).csv")

df2 <- df %>%
  filter(Level == "D1") %>%
  #filter(AutoPitchType == "Four-Seam") %>%
  group_by(AutoPitchType) %>%
  summarise(m = mean(abs(Extension),na.rm = TRUE),
            s = sd(RelHeight,na.rm = TRUE))

df$RelHeight

df2 <- df %>%
  filter(Level == "D1") %>%
 # filter(AutoPitchType == "Four-Seam") %>%
  filter(PitcherThrows == "Right") %>%
  select(RelSide,RelHeight,HorzApprAngle,VertApprAngle,InducedVertBreak,HorzBreak,PlateLocHeight,PlateLocSide) %>%
  drop_na()

corrplot::corrplot(cor(df2), type = "lower")
