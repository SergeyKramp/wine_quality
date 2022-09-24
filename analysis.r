library(tidyverse)
library(data.table)
library(GGally)
library(MLmetrics)

white <- fread("winequality-white.csv")
red <- fread("winequality-red.csv")

#Overview
summary(white)

#Missing data analysis
colSums(is.na(white))
colSums(is.na(red))

# Correlation matrix for white wine
ggcorr(white, c("pairwise", "spearman"))
ggcorr(white, c("pairwise", "pearson"))


# Correlation matrix for red wine
ggcorr(red, c("pairwise", "spearman"))
ggcorr(red, c("pairwise", "pearson"))

# Get variables with significant correlation to quality
white_full_sum <- summary(lm(quality ~ ., data = white))
red_full_sum <- summary(lm(quality ~ ., data = red))

white_sig_vars <- names(white_full_sum$coefficients[,"Pr(>|t|)"] < 0.05)[-1]
red_sig_vars <- names(red_full_sum$coefficients[,"Pr(>|t|)"] < 0.05)[-1]

# Potential models
model_w_full <- lm(quality ~ ., data = white)
model_w_back <- step(model_w_full, direction = "backward")
model_w_for <- step(lm(quality ~ 1, data = white), 
                    scope = ~ `fixed acidity` +`volatile acidity` + `citric acid` + `citric acid` +
                      chlorides + `free sulfur dioxide` + `total sulfur dioxide` + density + pH +
                      sulphates + alcohol,
                    direction = "forward")

anova(model_w_full, model_w_back) # full model not significantly 'better' than smaller backward model
anova(model_w_full, model_w_for) # full model is 'better' than reduced model


model_r_full <- lm(quality ~ ., data = red)
model_r_back <- step(model_r_full, direction = "backward")
model_r_for <- step(lm(quality ~ 1, data = red), 
                    scope = ~ `fixed acidity` +`volatile acidity` + `citric acid` + `citric acid` +
                      chlorides + `free sulfur dioxide` + `total sulfur dioxide` + density + pH +
                      sulphates + alcohol,
                    direction = "forward")

anova(model_r_full, model_r_back) # full model not significantly 'better' than smaller backward model
anova(model_r_full, model_r_for) # full model not significantly 'better' than smaller forward model

models_white <- list(model_w_full, model_w_back, model_w_for)
models_red <- list(model_r_full, model_r_back, model_r_for)

pipeline <- function(data, train_prop = 0.9, k_folds = 5, models){
  
  # Train-Test split
  train_rows <- floor(train_prop * nrow(data))
  test_rows <- nrow(data) - train_rows
  
  train_test_index <- sample(c(rep("train", train_rows), rep("test", test_rows)))
  
  X <- data[train_test_index == "train"]
  y <- data[train_test_index == "test"]
  
  # Folds split
  partitions <- sample(rep(1:k_folds, ceiling(nrow(X)/k_folds)))[1:nrow(X)]
  
  # Vector to store model MSEs
  cv_mses <- c()
  
  # K-Fold Cross Validation
  for(m in 1: length(models)) {
    
    # empty vector to store MSEs
    mses <- c()
  
    for(k in 1:k_folds){
      
      # split training data into training and validation
      train <- X[partitions != k]
      valid <- X[partitions == k]
      
      # train model and get predictions
      model <- update(models[m][[1]], data = train)
      y_hat <- predict(model, valid)
      mse <- MSE(y_hat, valid$quality)  
      mses[k] <- mse
    }
    
    # Store average MSE
    cv_mses[m] <- mean(mses)
  }
  
  # Get best performing model and its MSE
  best_model <- models[[which(cv_mses == min(cv_mses))]]
  best_model_trained <- update(best_model, data = X)
  test_mse <- MSE(predict(best_model_trained, y), y$quality)
  
  return(c(best_model$call, test_mse))
}

model_white <- pipeline(data = white, train_prop = 0.9, k_folds = 10, models = models_white)
model_red <- pipeline(data = red, train_prop = 0.9, k_folds = 10, models = models_red)
