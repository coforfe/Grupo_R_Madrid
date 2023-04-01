#https://cran.r-project.org/web/packages/scorecard/vignettes/demo.html

library(data.table)
library(dplyr)
library(scorecard)

df <- fread("data/creditcard.csv", sep = ",")
names(df)

#Se separan ahora los datos para que tenga coherencia la separación 
#test training en el stacking

#Se construye un modelo con df1

df_list <- split_df(df, y = "Class", ratios = c(0.6, 0.4), seed = 30)
label_list <- lapply(df_list, function(x) x$Class)

df_train <- df_list$train
df_test <- df_list$test


#Dos conjuntos de datos por separado para "jugar al stacking"

df_train1 <- df_train[,c("Time", "V1", "V2", "V3", "V4", "V5", "V6", "V7",     
             "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
             "V16", "V17", "V18", "V19", "V20", "Class" )]
df_test1 <- df_test[,c("Time", "V1", "V2", "V3", "V4", "V5", "V6", "V7",     
                         "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
                         "V16", "V17", "V18", "V19", "V20", "Class" )]

df_train2 <- df_train[,c("V21", "V22", "V23", "V24",
                         "V25", "V26", "V27", "V28", "Amount", "Class" )]
df_test2 <- df_test[,c("V21", "V22", "V23", "V24", 
                       "V25", "V26", "V27", "V28", "Amount", "Class" )]

#Se construye un modelo Base 1

#WoE en train

df_list1 = list(df_train1, df_test1)

bins_train1 <- woebin(df_train1, y = "Class")
dt_woe_list1 = lapply(df_list1, function(x) woebin_ply(x, bins_train1))
names(dt_woe_list1[[1]])

m1 = glm( Class ~ ., family = binomial(), data = dt_woe_list1[[1]])

summary(m1)

# Evaluación modelo 1

pred_list1 = lapply(dt_woe_list1, function(x) predict(m1, x, type='response'))
names(pred_list1)[1] = "train"
names(pred_list1)[2] = "test"

perf1 = perf_eva(pred = pred_list1, label = label_list, 
                 show_plot = c("ks","roc"),
                pred_desc = F)

#Se construye un modelo Base 2

#WoE en train

df_list2 = list(df_train2, df_test2)

bins_train2 <- woebin(df_train2, y = "Class")
dt_woe_list2 = lapply(df_list2, function(x) woebin_ply(x, bins_train2))
names(dt_woe_list2[[1]])

m2 = glm( Class ~ ., family = binomial(), data = dt_woe_list2[[1]])

summary(m2)

# Evaluación modelo 2

pred_list2 = lapply(dt_woe_list2, function(x) predict(m2, x, type='response'))
names(pred_list2)[1] = "train"
names(pred_list2)[2] = "test"

perf2 = perf_eva(pred = pred_list2, label = label_list, 
                 show_plot = c("ks","roc"),
                 pred_desc = F)

#Cuasi staking del modelo 2 en el 1

#Como se tiene, en este caso las predicciones hechas al estar todo sobre
#el mismo training test, añadimos la variable explicativa al df1_train1
#y al test y se repite el proceso desde la construcción woe

df_train3 <- df_train1
df_test3 <- df_test1

df_train3$stacking <- pred_list2[[1]]
df_test3$stacking <- pred_list2[[2]]

#WoE en train

df_list3 = list(df_train3, df_test3)

bins_train3 <- woebin(df_train3, y = "Class")
dt_woe_list3 = lapply(df_list3, function(x) woebin_ply(x, bins_train3))
names(dt_woe_list3[[1]])

m3 = glm( Class ~ ., family = binomial(), data = dt_woe_list3[[1]])

summary(m3)

# Evaluación modelo 3

pred_list3 = lapply(dt_woe_list3, function(x) predict(m3, x, type='response'))
names(pred_list3)[1] = "train"
names(pred_list3)[2] = "test"

perf3 = perf_eva(pred = pred_list3, label = label_list, 
                 show_plot = c("ks","roc"),
                 pred_desc = F)


#Mixtura de modelos del m1 con el m2 en este caso no hay que re-estimar
#Se elije el nivel de mixtura y los modelos conservan sus características

pred_mixt <- list((0.8*pred_list1[[1]] + 0.2*pred_list2[[1]]), 
                  (0.8*pred_list1[[2]] + 0.2*pred_list2[[2]]))
names(pred_mixt)[1] = "train"
names(pred_mixt)[2] = "test"

perf_mixt = perf_eva(pred = pred_mixt, label = label_list, 
                 show_plot = c("ks","roc"),
                 pred_desc = F)


#Stacking completo de modelos bajo una regresión logística

#Se toma la target de alguno de los modelos
df_stacking_train <- df_train1[,c("Class")]
df_stacking_test <- df_test1[,c("Class")]

#Se toma la predicción del modelo 1 como input

df_stacking_train$stacking1 <- pred_list1[[1]]
df_stacking_test$stacking1 <- pred_list1[[2]]

#Lo mismo con el modelo 2

df_stacking_train$stacking2 <- pred_list2[[1]]
df_stacking_test$stacking2 <- pred_list2[[2]]

#Se sigue el procedimiento CS para estas 2 variables

#WoE en train

df_stacking = list(df_stacking_train, df_stacking_test)

bins_stacking <- woebin(df_stacking_train, y = "Class")
dt_woe_stacking = lapply(df_stacking, function(x) woebin_ply(x, bins_stacking))
names(dt_woe_stacking[[1]])

mstacking = glm( Class ~ ., family = binomial(), data = dt_woe_stacking[[1]])

summary(mstacking)


# Evaluación modelo stacking

pred_stacking = lapply(dt_woe_stacking, function(x) predict(mstacking, 
                                                            x, type='response'))
names(pred_stacking)[1] = "train"
names(pred_stacking)[2] = "test"

perfstacking = perf_eva(pred = pred_stacking, label = label_list, 
                 show_plot = c("ks","roc"),
                 pred_desc = F)



#Stacking sin transformación CS

mstacking2 = glm( Class ~ ., family = binomial(), data = df_stacking_train)
summary(mstacking2)

dt_stacking2 <- list(df_stacking_train, df_stacking_test)

pred_stacking2 = lapply(dt_stacking2, function(x) predict(mstacking2, 
                                                            x, type='response'))
names(pred_stacking2)[1] = "train"
names(pred_stacking2)[2] = "test"

perfstacking2 = perf_eva(pred = pred_stacking2, label = label_list, 
                        show_plot = c("ks","roc"),
                        pred_desc = F)
