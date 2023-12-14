library(data.table)
library(dplyr)


#Se leen los datos haciendo uso de rutas relativas
df <- fread("../data/creditcard.csv")

#Se crea un dataframe forma por 2 únicos registros para observár cómo
#se llevaría a cabo una aplicación del modelo en real

names(df)

df1 <- df %>%
  filter(Class == 1)

df0 <- df %>%
  filter(Class == 0)

individuo <- rbind(df1[1,], df0[1,])

#Se sacan a los individuos del dataset para que no interfieran en el entre-
#namiento

df1 <- df1[-1,]
df0 <- df0[-1,]

df <- rbind(df0, df1)

#Se guarda todo en la carpeta data

write.table(df, "../data/total.csv", row.names = F, sep = ";")
write.table(individuo, "../data/population.csv", row.names = F, sep = ";")