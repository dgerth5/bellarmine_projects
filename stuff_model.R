library(data.table)
library(tidyverse)
library(MLmetrics)
library(catboost)
library(ggplot2)
library(pROC)

df <- fread("C:/Users/david/Downloads/TM_2024_reg_szn (1).csv")

unique(df$BatterSide)

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
    mutate(sameHand = if_else(PitcherThrows == BatterSide, 1, 0)) %>%
    # select(cleanOutcome,PlateLocHeight,PlateLocSide) %>%
    select(cleanOutcome,PlateLocHeight,PlateLocSide,RelSpeed,InducedVertBreak,HorzBreak,RelSide,RelHeight,SpinRate,Extension,sameHand) %>%
    drop_na()
  
  df_by_type$PitcherThrows <- as.factor(df_by_type$sameHand)
  
  id <- sample(1:nrow(df_by_type), round(0.75*nrow(df_by_type)))
  
  train_df <- df_by_type[id, ]
  test_df <- df_by_type[-id, ]
  
  lst <- list(train_df,test_df)
  
  return(lst)
  
}

ff_df <- train_test_by_type(df2, ff_type)

train <- as.data.frame(ff_df[[1]])
test <- as.data.frame(ff_df[[2]])

# 
# train$cleanOutcome <- as.integer(as.factor(train$cleanOutcome)) - 1
# test$cleanOutcome <- as.integer(as.factor(test$cleanOutcome)) - 1

train_pool <- catboost.load_pool(data = train[, -1],
                                 label = as.integer(as.factor(train$cleanOutcome)) - 1)
test_pool <- catboost.load_pool(data = test[, -1],
                                label = as.integer(as.factor(test$cleanOutcome)) - 1)

params <- list(
  loss_function = 'MultiClass',
  eval_metric = 'AUC',
  iterations = 1000,
  learning_rate = 0.1,
  depth = 6,
  random_seed = 134,
  verbose = 100
)

model <- catboost.train(train_pool, params = params)

class_predictions <- catboost.predict(model, test_pool, prediction_type = "Class")
MLmetrics::Accuracy(class_predictions, as.integer(as.factor(test$cleanOutcome)) - 1)

conf_matrix <- table(Predicted = class_predictions, Actual = as.integer(as.factor(test$cleanOutcome)) - 1)
print(conf_matrix)

prob_predictions <- catboost.predict(model, test_pool, prediction_type = "Probability")
prob_df <- as.data.frame(prob_predictions)
names(prob_df) <- levels(as.factor(test$cleanOutcome))

bin_predictions <- prob_df %>%
  mutate(Actual = test$cleanOutcome) %>%
  gather(key = "Class", value = "PredictedRate", -Actual) %>%
  group_by(Class) %>%
  mutate(Bin = ntile(PredictedRate, 50)) %>%
  group_by(Class, Bin) %>%
  summarise(PredictedRate = mean(PredictedRate),
            ActualRate = mean(Actual == Class))
ggplot(bin_predictions, aes(x = PredictedRate, y = ActualRate)) +
  geom_point(color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlim(c(0,1)) + ylim(c(0,1))+
  facet_wrap(~ Class) +
  labs(x = "Predicted Rate", y = "Actual Rate") +
  theme_minimal()

test$cleanOutcome <- as.factor(test$cleanOutcome)
levels_outcome <- levels(test$cleanOutcome)
colnames(prob_predictions) <- levels_outcome

multiclass_roc <- multiclass.roc(test$cleanOutcome, prob_predictions)
auc(multiclass_roc)

