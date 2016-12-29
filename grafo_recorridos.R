----#librerias----

library(igraph)
library(sp)
library(rworldmap)
library(rworldxtra)
library(ggplot2)
library(dplyr)

----#analisis---- 

recorridos_grafo <- cbind.data.frame(recorrido.bicis.2016$ORIGEN_ESTACION,recorrido.bicis.2016$DESTINO_ESTACION)

names(recorridos_grafo) <- c("estacion_origen","estacion_destino")

## me quedo unicamente con casos que no tengan datos faltantes

recorridos_grafo <- recorridos_grafo[complete.cases(recorridos_grafo),]

red.bici <- graph.edgelist(as.matrix(recorridos_grafo), directed = T)

summary(red.bici)

unique_estaciones <- unique(recorrido.bicis.2016$ORIGEN_ESTACION)

red_alq_bici <- induced_subgraph(red.bici, vids = unique_estaciones)

summary(red_alq_bici)

red_simple <- simplify(red_alq_bici,remove.multiple = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))


plot(red_simple, layout=layout.fruchterman.reingold,edge.arrow.size=.1, vertex.color="gold", vertex.size=5, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)

summary( recorrido.bicis.2016$n)

# Set node size based on retiros:

cant_retiros <- recorrido.bicis.2016 %>% group_by(ORIGEN_ESTACION,NOMBRE_ORIGEN=trimws(NOMBRE_ORIGEN)) %>% summarise(cant_ret= n())
cant_retiros <- cant_retiros[-29,]
cant_retiros

V(red_simple)$retiros <- cant_retiros$cant_ret
V(red_simple)$size <- log10(cant_retiros$cant_ret)

summary(red_simple)

E(red_simple)$arrow.size <- .2


plot(red_simple, layout=layout.fruchterman.reingold,edge.arrow.size=.1, vertex.color="gray50", 
     
     vertex.frame.color="gray", vertex.label.color="black", edge.color="orange",
     
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)

tkplot(red_simple, layout=layout.fruchterman.reingold,edge.arrow.size=.1, vertex.color="gray50", 
       
       vertex.frame.color="gray", vertex.label.color="black", edge.color="orange",
       
       vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)


----#otra manera de representar----


netm <- as_adjacency_matrix(red_simple)
colnames(netm) <- V(red_simple)$
rownames(netm) <- V(red_simple)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )


----#interactivo----

install.packages('ndtv', dependencies=T)
library('ndtv')
library('visNetwork') 
library('network')

links <- recorridos_grafo
names(links) <- c("from","to")
links$weight <- 1

nodes <- cbind.data.frame(id=cant_retiros$ORIGEN_ESTACION,estacion=cant_retiros$NOMBRE_ORIGEN,retiros=cant_retiros$cant_ret)

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[3] <- "weight"
rownames(links) <- NULL
nodes$estacion <- as.character(nodes$estacion)

net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist", 
                loops=F, multiple=F, ignore.eval = F)

summary(net3)

net3 %n% "net.name" <- "BiciBA Network" #  network attribute
net3 %v% "estacion"    # Node attribute
#net3 %e% "type"     # Node attribute

plot(net3, vertex.cex=scale(net3 %v% "retiros"))

plot(net3, vertex.cex=scale(net3 %v% "retiros"), interactive=T)

visNetwork(nodes, links, width="100%", height="400px", main="Network!")

par(mar=c(0,0,0,0))

render.d3movie(red_simple, usearrows = F, displaylabels = F, bg="#111111", 
               vertex.border="#ffffff", vertex.col =  net3 %v% "col",
               vertex.cex = (net3 %v% "retiros")/8, 
               edge.lwd = (net3 %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3 %v% 'retiros') , "<br>",
                                      "<b>Type:</b>", (net3 %v% 'type.label')),
               edge.tooltip = paste("<b>Edge type:</b>", (net3 %e% 'type'), "<br>", 
                                    "<b>Edge weight:</b>", (net3 %e% "weight" ) ),
               launchBrowser=F, filename="bicis.html" )  
