library(data.table)
library(tidyverse)
library(MLmetrics)
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
  mutate(cleanOutcome = case_when(PitchCall %in% ball_events ~ "Ball",
                                  PitchCall %in% strike_call_events ~ "CalledStrike",
                                  PitchCall %in% strike_sw_events ~ "SwingingStrike",
                                  PitchCall %in% foul_events ~ "FoulBall",
                                  PitchCall %in% ip_events ~ AutoHitType))

unique(df2$cleanOutcome)

df2$PlateLocHeight

apt <- sort(unique(df2$AutoPitchType))

ff_type <- c(apt[4], apt[6])
bb_type <- c(apt[2], apt[3], apt[7])
ch_type <- c(apt[1], apt[8])

colnames(df2)

train_test_by_type <- function(df, pitch_type_vector){
  
  set.seed(134)
  
  df_by_type <- df %>%
    filter(AutoPitchType %in% pitch_type_vector) %>%
    select(cleanOutcome,RelSpeed,InducedVertBreak,HorzBreak,RelSide,RelHeight,SpinRate,Extension, PitcherThrows) %>%
    drop_na()
  
  df_by_type$PitcherThrows <- as.factor(df_by_type$PitcherThrows)
  
  id <- sample(1:nrow(df_by_type), round(0.75*nrow(df_by_type)))
  
  train_df <- df_by_type[id, ]
  test_df <- df_by_type[-id, ]
  
  lst <- list(train_df,test_df)
  
  return(lst)
  
}

ff_df <- train_test_by_type(df2, bb_type)

train_catboost_model <- function(train_df, test_df) {
  
  # Convert cleanOutcome to a factor and then to integer
  train_df$cleanOutcome <- as.integer(as.factor(train_df$cleanOutcome)) - 1
  test_df$cleanOutcome <- as.integer(as.factor(test_df$cleanOutcome)) - 1
  
  # Convert the datasets into CatBoost Pools without specifying text or categorical features
  train_pool <- catboost.load_pool(data = train_df[, -1], 
                                   label = train_df$cleanOutcome)
  test_pool <- catboost.load_pool(data = test_df[, -1], 
                                  label = test_df$cleanOutcome)
  
  # Define the model parameters
  params <- list(
    loss_function = 'MultiClass',
    eval_metric = 'Accuracy',
    iterations = 1000,
    learning_rate = 0.1,
    depth = 6,
    random_seed = 134,
    verbose = 100         # Show output every 100 iterations
  )
  
  # Train the model
  model <- catboost.train(train_pool, params = params)
  
  # Predict on the test set
  predictions <- catboost.predict(model, test_pool, prediction_type = "Probability")
  
  # Get the predicted classes
  predicted_classes <- apply(predictions, 1, which.max) - 1  # CatBoost uses 0-based indexing for classes
  
  # Calculate accuracy
  accuracy <- Accuracy(test_df$cleanOutcome, predicted_classes)
  
  # Calculate AUC
  #auc <- MulticlassAUC(predictions, test_df$cleanOutcome)
  
  return(accuracy)
}

# Train the model and get accuracy and AUC
results <- train_catboost_model(as.data.frame(ff_df[[1]]), as.data.frame(ff_df[[2]]))

# Print results
results
