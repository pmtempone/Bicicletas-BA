library(R.utils)
library(RCurl)
library(httr)
library(XML)
library(dplyr)
library(plyr)
  
  rawFilePath <- "../data/raw";
  

  estadoEstacionesBicisUrl <- "https://recursos-data.buenosaires.gob.ar/ckan2/ecobici/estado-ecobici.xml";
  estadoEstacionesBicisRawFile <- paste(rawFilePath, "estado-ecobici.xml", sep = "/");
  #downloadFile(estadoEstacionesBicisUrl, estadoEstacionesBicisRawFile)
  download.file(estadoEstacionesBicisUrl, estadoEstacionesBicisRawFile)
  
  estadoEstacionesBicisDoc <- xmlParse(file=estadoEstacionesBicisRawFile);
  
  defaultNamespace <- c( ns="http://bicis.buenosaires.gob.ar/ServiceBicycle.asmx" );
  estacionId <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:EstacionId", xmlValue, namespaces = defaultNamespace)
  estacionNombre <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:EstacionNombre", xmlValue, namespaces = defaultNamespace)
  bicicletaDisponibles <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:BicicletaDisponibles", xmlValue, namespaces = defaultNamespace)
  estacionDisponible <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:EstacionDisponible", xmlValue, namespaces = defaultNamespace)
  latitud <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:Latitud", xmlValue, namespaces = defaultNamespace)
  longitud <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:Longitud", xmlValue, namespaces = defaultNamespace)
  numero <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:Numero", xmlValue, namespaces = defaultNamespace)
  lugar <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:Lugar", xmlValue, namespaces = defaultNamespace)
  piso <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:Piso", xmlValue, namespaces = defaultNamespace)
  anclajesTotales <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:AnclajesTotales", xmlValue, namespaces = defaultNamespace)
  anclajesDisponibles <- xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion/ns:AnclajesDisponibles", xmlValue, namespaces = defaultNamespace)
  
  estadoEstacionesBicisDf <- data.frame(estacionId, estacionNombre, bicicletaDisponibles, estacionDisponible, latitud, longitud, numero, lugar, piso, anclajesTotales, anclajesDisponibles, stringsAsFactors = F)
  
  if (nrow(estadoEstacionesBicisDf) != length(xpathSApply(estadoEstacionesBicisDoc, "//ns:Estacion", xmlValue, namespaces = defaultNamespace)) ) {
    warning("Error al leer el archivo de estado de estaciones: la cantidad de registros leída no coincide con la del archivo.")
  }
  
  str(estadoEstacionesBicisDf)
  
  idColumnasConNA <- sapply(estadoEstacionesBicisDf, anyNA);
  names(estadoEstacionesBicisDf)[idColumnasConNA]
  
  estadoEstacionesBicisDf$bicicletaDisponibles <- as.numeric(estadoEstacionesBicisDf$bicicletaDisponibles)
  estadoEstacionesBicisDf$fecha <- Sys.time()
  
  ifelse(exists("bicishistorico")==FALSE,(bicishistorico=data.frame()),"no hago nada")
  
  bicishistorico <- rbind(bicishistorico,estadoEstacionesBicisDf)
  