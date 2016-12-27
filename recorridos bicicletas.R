----#librerias----

library(dplyr)
library(igraph)
library(ggplot2)
library(lubridate)

options(scipen = 999)

----#filtros----

recorrido.bicis.2016 <- read.csv("~/Dropbox/Bicicletas BA/recorrido-bicis-2016.csv", sep=";", stringsAsFactors=FALSE)

recorrido.bicis.2016$hora <- hm(substr(recorrido.bicis.2016$FECHA_HORA_RETIRO,11,16))

recorrido.bicis.2016$franja_horaria <- ifelse(recorrido.bicis.2016$hora >= '9H 0M 0S' & recorrido.bicis.2016$hora < '11H 0M 0S',"9-11",ifelse(recorrido.bicis.2016$hora >= '17H 0M 0S' & recorrido.bicis.2016$hora < '19H 0M 0S',"17-19","otros"))

recorrido.bicis.2016$n <- 1

recorrido.bicis.2016 <- recorrido.bicis.2016[complete.cases(recorrido.bicis.2016),]

recorrido.bicis.2016$hora_sola <- factor(as.integer(unlist(strsplit(substr(recorrido.bicis.2016$FECHA_HORA_RETIRO,11,13), split=':', fixed=TRUE))))

ggplot(recorrido.bicis.2016,aes(x=hora_sola,y=n))+geom_bar(stat = "identity")

write.csv(summary(recorrido.bicis.2016$hora_sola),file = "cant_retirosxhora.csv")
