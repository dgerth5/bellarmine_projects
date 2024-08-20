library(data.table)
library(tidyverse)
library(MLmetrics)
library(catboost)
library(pROC)

df <- fread("C:/Users/david/Downloads/TM_2024_reg_szn (1).csv")

ip_events <- c("InPlay","InPLay")

# exit_speed, angle, Stadium

mod_df <- df %>%
  filter(PitchCall %in% ip_events) %>%
  filter(!PlayResult %in% c("Sacrifice","Error","Undefined","error")) %>%
  mutate(cleanOutcome = case_when(PlayResult == "SIngle" ~ "Single",
                                  PlayResult == "homerun" ~ "HomeRun",
                                  PlayResult == "Homerun" ~ "HomeRun",
                                  PlayResult == "FieldersChoice" ~ "Out",
                                  TRUE ~ PlayResult)) %>%
  select(cleanOutcome, PitcherTeam, Stadium, ExitSpeed, Angle) %>%
  mutate_at(c("cleanOutcome","PitcherTeam","Stadium"), as.factor) %>%
  drop_na()

# split train/test

set.seed(104)

id <- sample(1:nrow(mod_df), round(0.75*nrow(mod_df)))

train <- mod_df[id,]
test <- mod_df[-id,]

mod_pool <- catboost.load_pool(data = train[, -1],
                               label = as.integer(train$cleanOutcome) - 1)

test_pool <- catboost.load_pool(data = test[, -1],
                               label = as.integer(test$cleanOutcome) - 1)

params <- list(
  loss_function = 'MultiClass',
  eval_metric = 'AUC',
  iterations = 1000,
  learning_rate = 0.1,
  depth = 6,
  random_seed = 134,
  verbose = 100
)

model <- catboost.train(mod_pool, params = params)

prop_preds <- catboost.predict(model, test_pool, prediction_type = "Probability")

prob_df <- as.data.frame(prop_preds)
names(prob_df) <- levels(as.factor(mod_df$cleanOutcome))

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


multiclass.roc(test$cleanOutcome, prob_df)
