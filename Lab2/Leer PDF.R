# Leer de un PDF
install.packages("tabulizer")
library(tabulizer)
library(dplyr)
library(stringr)
pages<-extract_tables("C01-Importación-de-combustibles-VOLUMEN-2020-03.pdf",method = "lattice")#datos2020
pages[2]<-as.data.frame(pages[2]:nrow(pages(2)),)
datosImp <- do.call(rbind, pages)
nombresVar<-datosImp[1,]
datosImp<-as.data.frame(datosImp[2:nrow(datosImp),])
nombresVar[c(1,4,5,6,8,10,11,15,16,21,23,24)]<-c("Anio","GasAviacion","GasSuperior","GasRegular","rTurboJet","DieselLS","DieselULS","AceitesLub","GrasasLub","PetroleoReconst","Orimulsion","MezclasOleosas")
names(datosImp)<-nombresVar
