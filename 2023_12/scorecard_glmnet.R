library(scorecard)


scorecard_glmnet <- function(bins, 
                             model, 
                             points0 = 600, 
                             odds0 = 1/19, 
                             pdo = 50,
                             lambda = 0){

  #Inputs:
    #bins: Binning of the scorecard model
    #mod: glmnet model
    #lambda: Optimal lambda associated. The value is not neede
  
  #Outputs: A list with all the information required by scorecard_ply
  
  if(is.na(lambda) == T){lambda = 0}
  
  card = list()
  
  b = pdo / log(2)
  a = points0 + b * log(odds0)

  basepoints <- data.frame(variable = "basepoints",
                           bin = NA,
                           woe = NA,
                           points =   a - b * coef(model, s = lambda)[1] 
                           ) 
  
  card[[1]] <- as.data.table(basepoints)
  variables <- vector("character", length(coef(mod, s = lambda)) - 1)
  
  for (i in c(2: length(coef(model, s = lambda)))){
    
    bins[[(i - 1)]]$points = -bins[[(i - 1)]]$woe * 
      coef(mod, s = lambda)[i] *
      b
    
    card [[i]] = as.data.table(bins[[(i - 1)]])
    
    variables[i - 1] <- unique(bins[[(i - 1)]]$variable)
    
  }
  
  names(card) <- c("basepoints", variables)
  
  return(card)
  
}



#Example

library(data.table)
library(dplyr)
library(scorecard)
library(glmnet)

df <- fread("../data/creditcard.csv", sep = ",")


df_list <- split_df(df, y = "Class", ratios = c(0.6, 0.4), seed = 30)
label_list <- lapply(df_list, function(x) x$Class)

df_train <- df_list$train
df_test <- df_list$test

#WoE en train

bins_train <- woebin(df_train, y = "Class")

df_woe_list = lapply(df_list, function(x) woebin_ply(x, bins_train))

input1 <- as.matrix(df_woe_list$train[,-"Class"])
target1 <- df_woe_list$train$Class

# Ajustar un modelo de regresión ridge. Paciencia tarda unos 3 min en un ordenador con 16GB i5
cv_m <- cv.glmnet(input1, target1, family = "binomial", alpha = 0)
lambda_optimo <- cv_m$lambda.min

m <- glmnet(input1, target1, family = "binomial", alpha = 0, lambda = lambda_optimo)

card <- scorecard_glmnet(bins_train, m, lambda_optimo)

application <- scorecard_ply(df_list$test, card)








