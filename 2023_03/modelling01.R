#https://cran.r-project.org/web/packages/scorecard/vignettes/demo.html

library(data.table)
library(dplyr)
library(scorecard)

df <- fread("data/creditcard.csv", sep = ",")

names(df)

#The var_filter function drops column variables that don’t meet the thresholds 
#for missing rate (> 95% by default), information value (IV) (< 0.02 by default), 
#or identical value rate (> 95% by default).

#Este paso no se suele dar a este nivel debe estudiarse antes las variables

#df_f1 <- var_filter(df, y = "Class")

df_list <- split_df(df, y = "Class", ratios = c(0.6, 0.4), seed = 30)
label_list <- lapply(df_list, function(x) x$Class)

df_train <- df_list$train
df_test <- df_list$test

#WoE en train

bins_train <- woebin(df_train, y = "Class")

#Ejemplo de un bin: 
#bins_train$Time


#The user can also adjust bin breaks interactively by using the woebin_adj function.

# breaks_adj <- woebin_adj(dt_f, y = "creditability", bins = bins)
#Furthermore, the user can set the bin breaks manually via the breaks_list = list() argument in the woebin function. Note the use of %,% as a separator to create a single bin from two classes in a categorical independent variable.

#breaks_adj <- list(
#  age.in.years = c(26, 35, 40),
#  other.debtors.or.guarantors = c("none", "co-applicant%,%guarantor")
#)

#bins_adj <- woebin(dt_f, y = "creditability", breaks_list = breaks_adj)
#Once your WoE bins are established for all desired independent variables, apply the binning logic to the training and test datasets.

dt_woe_list = lapply(df_list, function(x) woebin_ply(x, bins_train))
m1 = glm( Class ~ ., family = binomial(), data = dt_woe_list$train)

summary(m1)

# Evaluación

pred_list = lapply(dt_woe_list, function(x) predict(m1, x, type='response'))
perf = perf_eva(pred = pred_list, label = label_list, show_plot = c("ks","roc"),
                pred_desc = F)

#Scorecard
# Build the card
card <- scorecard(bins_train, m1)

#Ejemplo de un score parcial:
#card$Time

# Obtain Credit Scores
score_train <- scorecard_ply(df_train, card)
score_test <- scorecard_ply(df_test, card)

score_list = lapply(df_list, function(x) scorecard_ply(x, card))

# Analyze the PSI
perf_psi(score = score_list, label = label_list)


