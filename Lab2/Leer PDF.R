# Leer de un PDF
install.packages("tabulizer")
library(tabulizer)
library(dplyr)
library(stringr)
pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf")#datos2020
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar

datosImp <- datosImp[-c(46,96,146,196),]
datosImp <- subset(datosImp, select = c(Anio, Mes, GasSuperior, GasRegular, Diesel, DieselLS))

datosImp$Diesel[datosImp$Diesel=="-"] <- NA
datosImp$DieselLS[datosImp$DieselLS=="-"] <- NA

datosImp$Diesel[is.na(datosImp$Diesel)] <- datosImp$DieselLS[is.na(datosImp$Diesel)]
datosImp <- subset(datosImp, select = -c(DieselLS))

datosImp$Diesel <- gsub(",", "", datosImp$Diesel)
datosImp$GasSuperior <- gsub(",", "", datosImp$GasSuperior)
datosImp$GasRegular <- gsub(",", "", datosImp$GasRegular)

datosImp$Anio <- as.factor(gsub(",", "", datosImp$Anio))
datosImp$Mes <- as.factor(gsub(",", "", datosImp$Mes))
datosImp$Diesel <- as.numeric(gsub(",", "", datosImp$Diesel))
datosImp$GasSuperior <- as.numeric(gsub(",", "", datosImp$GasSuperior))
datosImp$GasRegular <- as.numeric(gsub(",", "", datosImp$GasRegular))
