library(data.table)
library(tidyverse)
library(catboost)

df <- fread("C:/Users/david/Downloads/TM_2024_reg_szn (1).csv")

upc <- sort(unique(df$PitchCall))
ball_events <- c(upc[2],upc[3])
strike_call_events <- c(upc[13], upc[14])
strike_sw_events <- c(upc[15], upc[16])
foul_events <- c(upc[6],upc[7],upc[8],upc[9])
ip_events <- c(upc[11], upc[12])
remove_events <- c(upc[1], upc[4], upc[5], upc[17])

df2 <- df %>%
  filter(!PitchCall %in% remove_events) %>%
  filter(Level == "D1") %>%
  mutate(cleanOutcome = case_when(PitchCall %in% ball_events ~ "Ball",
                                  PitchCall %in% strike_call_events ~ "CalledStrike",
                                  PitchCall %in% strike_sw_events ~ "Whiff",
                                  PitchCall %in% foul_events ~ "Foul",
                                  PitchCall %in% ip_events ~ AutoHitType),
         isSwing = if_else(cleanOutcome %in% c("Ball", "CalledStrike"), 1, 0))


apt <- sort(unique(df2$AutoPitchType))

ff_type <- c(apt[4], apt[6])
bb_type <- c(apt[2], apt[3], apt[7])
ch_type <- c(apt[1], apt[8])


model_fn <- function(df, pitch_type_vector){
  
  set.seed(134)
  
  df_by_type <- df %>%
    filter(AutoPitchType %in% pitch_type_vector) %>%
    mutate(sameHand = if_else(PitcherThrows == BatterSide, 1, 0)) %>%
    select(cleanOutcome,PlateLocHeight,PlateLocSide,RelSpeed,InducedVertBreak,HorzBreak,RelSide,RelHeight,SpinRate,Extension,sameHand) %>%
    drop_na()
  
  df_by_type$sameHand <- as.factor(df_by_type$sameHand)
  
  id <- sample(1:nrow(df_by_type), round(0.75*nrow(df_by_type)))
  
  train <- df_by_type[id, ]
  test <- df_by_type[-id, ]
  
  train_pool <- catboost.load_pool(data = train[, -1],
                                   label = as.integer(as.factor(train$cleanOutcome)) - 1)
  test_pool <- catboost.load_pool(data = test[, -1],
                                  label = as.integer(as.factor(test$cleanOutcome)) - 1)
  
  params <- list(
    loss_function = 'MultiClassOneVsAll',
    eval_metric = 'AUC',
    iterations = 1000,
    random_seed = 134,
    verbose = 250
  )
  
  model <- catboost.train(train_pool, params = params)
  
  return(model)
}

ff_model <- model_fn(df2, ff_type)
bb_model <- model_fn(df2, bb_type)
ch_model <- model_fn(df2, ch_type)

saveRDS(list(ff_model = ff_model, bb_model = bb_model, ch_model = ch_model), "pitch_models.RDS")
