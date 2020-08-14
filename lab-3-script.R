library(tidyverse)
library(arulesViz)

# Guardamos la URL de la base de datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"

# Leemos directamente sobre los datos y se guarda en un data frame
# El separador de columas es una coma ","
cars <- read.table(url, sep = ",")

# Colocamos los nombres a las columnas
colnames(cars) <- c("buyingPrice", 
                    "maintenanceCost", 
                    "numberOfDoors", 
                    "numberOfPersons",
                    "sizeOfLuggageBoot",
                    "safety",
                    "decision")

rules = apriori(
  data = cars
)

inspect(rules)


rules.by.conf<-sort(rules, by="confidence", decreasing=TRUE) 
inspect(rules.by.conf)


rules.unacc<-apriori(cars, 
                     parameter=list(supp=0.001,conf = 0.08), 
                     appearance=list(rhs="decision=unacc")) 
rules.unacc.byconf<-sort(rules.unacc, by="confidence", decreasing=TRUE)
inspect(rules.unacc.byconf[1:100])


rules.vgood<-apriori(cars, parameter=list(supp=0.001,conf = 0.08), 
                     appearance=list(default="lhs",rhs="decision=vgood"), control=list(verbose=F)) 
rules.vgood.byconf<-sort(rules.vgood, by="confidence", decreasing=TRUE)
inspect(rules.vgood.byconf[1:15])





