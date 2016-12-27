----#librerias----
devtools::install_github("mattflor/chorddiag")
library(chorddiag)
library(psych)

----#armado---
  
estaciones <- recorrido.bicis.2016 %>% group_by(DESTINO_ESTACION,NOMBRE_DESTINO=trimws(NOMBRE_DESTINO))%>% summarise(tot=n())
estaciones <- estaciones[-33,]
estaciones[29,2] <- "PENA"
estaciones$DESTINO_ESTACION <- as.character(estaciones$DESTINO_ESTACION)
cant_retiros$ORIGEN_ESTACION <- as.character(cant_retiros$ORIGEN_ESTACION)

colnombres <- c("estacionid","nombre_estacion","cant")
names(estaciones) <- colnombres
names(cant_retiros) <- colnombres

estaciones <- rbind.data.frame(cant_retiros[,1:2],estaciones[,1:2])
estaciones <- estaciones %>% group_by(estacionid,estacion_nombre=trimws(nombre_estacion)) %>% summarise(tot=n())
estaciones <- estaciones[-27,]
estaciones$estacionid <- as.integer(estaciones$estacionid)
estaciones <- sort(estaciones)

recorridos_grafo$estacion_origen[recorridos_grafo$estacion_origen==522] <- 22
m <-   table(recorridos_grafo)
m <- as.data.frame.matrix(m)

colnames(m)[match(estaciones$estacionid, as.integer(colnames(m)))] <- estaciones$estacion_nombre[match(estaciones$estacionid, as.integer(colnames(m)))]


rownames(m)[!is.na(match(as.integer(rownames(m)),estaciones$estacionid))] <- estaciones$estacion_nombre[!is.na(match(estaciones$estacionid, as.integer(rownames(m))))]

dimnames(m) <- list(Origen = estaciones,
                    Destino = estaciones)

match(as.integer(rownames(m)),estaciones$estacionid)
estaciones$estacion_nombre[match(as.integer(rownames(m)),estaciones$estacionid)]
View(data.frame(estaciones$estacion_nombre[!is.na(match(estaciones$estacionid, as.integer(rownames(m))))]))
ncol(m)


----#correccion datos---
  
  recorridos_grafov2 <- cbind.data.frame(trimws(recorrido.bicis.2016$NOMBRE_ORIGEN),trimws(recorrido.bicis.2016$NOMBRE_DESTINO))

names(recorridos_grafov2) <- c("estacion_origen","estacion_destino")
levels(recorridos_grafov2$estacion_origen)[63:64] <- 'PENA'
levels(recorridos_grafov2$estacion_origen)[recorridos_grafov2$estacion_origen=="GALERIAS PACIFICO"] <- 'PACIFICO'
levels(recorridos_grafov2$estacion_destino)[65] <- 'PENA'

levels(recorridos_grafov2$estacion_destino)[35] <- 'GALERIAS PACIFICO'

levels(recorridos_grafov2$estacion_destino)[recorridos_grafov2$estacion_destino=="GALERIAS PACIFICO"] <- 'PACIFICO'


top30 <- arrange(cant_retiros[,2:3],desc(cant))[1:30,]

recorridos_grafov2$estacion_origen <- as.character(recorridos_grafov2$estacion_origen)
recorridos_grafov2$estacion_destino <- as.character(recorridos_grafov2$estacion_destino)

recorridos_grafov2$estacion_origen[!(recorridos_grafov2$estacion_origen %in% top30$nombre_estacion)] <- 'OTROS'
recorridos_grafov2$estacion_destino[!(recorridos_grafov2$estacion_destino %in% top30$nombre_estacion)] <- 'OTROS'

recorridos_grafov2 <- recorridos_grafov2[!(recorridos_grafov2$estacion_origen=='OTROS'),]
recorridos_grafov2 <- recorridos_grafov2[!(recorridos_grafov2$estacion_destino=='OTROS'),]

m <-   table(recorridos_grafov2)
m <- as.data.frame.matrix(m)

m <- as.matrix(m)

estaciones <- unique(recorridos_grafov2$estacion_destino)

library(chorddiag)
library(RColorBrewer)
### Show all the colour schemes available
display.brewer.all()
colchor <- colorRampPalette(brewer.pal(100,"Blues"))(100)
chorddiag(m,groupnameFontsize=10,groupnamePadding = 2,tickInterval=1500)


