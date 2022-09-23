library(tidyverse)
library(data.table)
library(GGally)

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

white_sig_vars <- gsub("`", "",names(white_full_sum$coefficients[,"Pr(>|t|)"] < 0.05)[-1])
red_sig_vars <- gsub("`", "",names(red_full_sum$coefficients[,"Pr(>|t|)"] < 0.05)[-1])

# Potential models
models <- c("quality ~ .",
            "quality ~ alcohol",
            "quality ~ alc")

pipeline <- function(data, train_prop = 0.9, k_folds = 5, models){
  
  # Train-Test split
  train_rows = floor(train_prop * nrow(data))
  test_rows = nrow(data) - train_rows
  
  train_test_index <- sample(c(rep("train", train_rows), rep("test", test_rows)))
  
  X <- data[train_test_index == "train"]
  y <- data[train_test_index == "test"]
  
  #Folds split
  partitions <- sample(rep(1:k_folds, ceiling(nrow(X)/k_folds)))[1:nrow(X)]
  
  # K-Fold Cross Validation
  for(k in 1:k_folds) {
    
    # empty vector to store model MSE
    mses <- c()
    
    # split training data into training and validation
    train <- X[partitions != k]
    valid <- X[partitions == k]
    
    for(m in 1:length(models)){
      
    }
  }
}




# Full regression model - white
