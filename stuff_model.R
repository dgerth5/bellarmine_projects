library(data.table)
library(tidyverse)
library(MLmetrics)
library(catboost)
library(ParBayesianOptimization)

df <- fread("C:/Users/david/Downloads/TM_2024_reg_szn (1).csv")

unique(df$Level)

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
                                  PitchCall %in% strike_sw_events ~ "SwingingStrike",
                                  PitchCall %in% foul_events ~ "FoulBall",
                                  PitchCall %in% ip_events ~ AutoHitType),
         isSwing = if_else(cleanOutcome %in% c("Ball", "CalledStrike"), 1, 0))

unique(df2$cleanOutcome)

apt <- sort(unique(df2$AutoPitchType))

ff_type <- c(apt[4], apt[6])
bb_type <- c(apt[2], apt[3], apt[7])
ch_type <- c(apt[1], apt[8])

#colnames(df2)

train_test_by_type <- function(df, pitch_type_vector){
  
  set.seed(134)
  
  df_by_type <- df %>%
    filter(AutoPitchType %in% pitch_type_vector) %>%
    select(cleanOutcome,RelSpeed,InducedVertBreak,HorzBreak,RelSide,RelHeight,SpinRate,Extension) %>%
    drop_na()
  
  id <- sample(1:nrow(df_by_type), round(0.75*nrow(df_by_type)))
  
  train_df <- df_by_type[id, ]
  test_df <- df_by_type[-id, ]
  
  lst <- list(train_df,test_df)
  
  return(lst)
  
}

ff_df <- train_test_by_type(df2, bb_type)

train <- as.data.frame(ff_df[[1]])
test <- as.data.frame(ff_df[[2]])

summary_df <- as.data.frame(train) %>%
  group_by(cleanOutcome) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(n2 = n / sum(n),
         weight = 1/n2)

train$cleanOutcome <- as.integer(as.factor(train$cleanOutcome)) - 1
test$cleanOutcome <- as.integer(as.factor(test$cleanOutcome)) - 1

train_pool <- catboost.load_pool(data = train[, -1],
                                 label = train$cleanOutcome)
test_pool <- catboost.load_pool(data = test[, -1],
                                label = test$cleanOutcome)

params <- list(
  loss_function = 'MultiClass',
  eval_metric = 'AUC',
  class_weights = summary_df$weight,
  iterations = 1000,
  learning_rate = 0.1,
  depth = 6,
  random_seed = 134,
  verbose = 100
)

model <- catboost.train(train_pool, params = params)

class_predictions <- catboost.predict(model, test_pool, prediction_type = "Class")
MLmetrics::Accuracy(class_predictions, test$cleanOutcome)

conf_matrix <- table(Predicted = class_predictions, Actual = test$cleanOutcome)
print(conf_matrix)
