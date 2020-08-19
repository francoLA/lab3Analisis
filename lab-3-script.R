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


####### AUTOS NO ACEPTABLES ##########

# Obtenemos las reglas para autos no aceptables
rules.unacc<-apriori(cars, 
                     parameter=list(supp=0.001,conf = 0.08), 
                     appearance=list(rhs="decision=unacc")) 

# Se ordena de manera decreciente
rules.unacc.byconf<-sort(rules.unacc, by="confidence", decreasing=TRUE)

# Regla {Seguridad baja} => {Auto no aceptable}
inspect(rules.unacc.byconf[1])

# Regla {Numero de personas = 2} => {Auto no aceptable}
inspect(rules.unacc.byconf[2])

# Regla {Precio muy alto, Costo mantencion muy alto} => {Auto no aceptable} 
inspect(rules.unacc.byconf[3])

############ AUTOS ACEPTABLES ######################

# Se obtienen las reglas para autos aceptables
rules.acc <- apriori(cars, 
                     parameter=list(supp=0.001,conf = 0.08), 
                     appearance=list(default="lhs",rhs="decision=acc"), control=list(verbose=F))

# Se ordena por confianza
rules.acc.byconf<-sort(rules.acc, by="confidence", decreasing=TRUE)

# Se ordena por soporte
rules.acc.bysupport<-sort(rules.acc, by="support", decreasing=TRUE)

# Se inspeccionan las primeras 15 con mayor confianza
inspect(rules.acc.byconf[1:15])

# Se inspeccionan las primeras 15 con mayor soporte
inspect(rules.acc.bysupport[1:15])

############ AUTOS BUENOS #################

# Se obtienen las reglas para autos buenos
rules.good<-apriori(cars, 
                     parameter=list(supp=0.001,conf = 0.08), 
                     appearance=list(default="lhs",rhs="decision=good"), 
                     control=list(verbose=F))

# Se ordena por confianza
rules.good.byconf <- sort(rules.good, by="confidence", decreasing=TRUE)

# Se ordena por soporte
rules.good.bysupport<-sort(rules.good, by="support", decreasing=TRUE)

# Se obtienen las primeras 15 reglas ordenadas por confianza
inspect(rules.good.byconf[1:15])

# Se obtienen las primeras 15 reglas ordenadas por soporte
inspect(rules.good.bysupport[1:15])

################ AUTOS MUY BUENOS ######################

# Se obtienen las reglas para autos muy buenos
rules.vgood<-apriori(cars, 
                     parameter=list(supp=0.001,conf = 0.08), 
                     appearance=list(default="lhs",rhs="decision=vgood"), 
                     control=list(verbose=F)) 

# Se ordenan por confianza
rules.vgood.byconf<-sort(rules.vgood, by="confidence", decreasing=TRUE)

# Se ordena por soporte
rules.vgood.bysupport<-sort(rules.vgood, by="support", decreasing=TRUE)

# Se obtienen las primeras 15 reglas ordenadas por confianza
inspect(rules.vgood.byconf[1:15])

# Se obtienen las primeras 15 reglas ordenadas por soporte
inspect(rules.vgood.bysupport[1:15])





