#DOnde yo hago mis pruebas
a<-datosImp[-c(46,96,146,196),]

hist(as.numeric(a$GLP),xlim=c(0,300),ylim = c(0,1000000))

install.packages("ggplot2")  
library(ggplot2)
a$GLP<-as.numeric(levels(a$GLP))[a$GLP]
qplot(as.numeric(a$GLP),geom = "histogram",ylim = c(0,10))


a$GLP <- as.numeric(gsub(",", "", a$GLP))
a$GasAviacion <- as.numeric(gsub(",", "", a$GasAviacion))
a$GasSuperior <- as.numeric(gsub(",", "", a$GasSuperior))
a$GasRegular <- as.numeric(gsub(",", "", a$GasRegular))
a$Kerosina <- as.numeric(gsub(",", "", a$Kerosina))
a$rTurboJet <- as.numeric(gsub(",", "", a$rTurboJet))
a$Diesel <- as.numeric(gsub(",", "", a$Diesel))
a$DieselLS <- as.numeric(gsub(",", "", a$DieselLS))
a$DieselULS <- as.numeric(gsub(",", "", a$DieselULS))
a$Bunker <- as.numeric(gsub(",", "", a$Bunker))
a$Asfalto <- as.numeric(gsub(",", "", a$Asfalto))
a$PetCoke <- as.numeric(gsub(",", "", a$PetCoke))
a$AceitesLub <- as.numeric(gsub(",", "", a$AceitesLub))
a$GrasasLub <- as.numeric(gsub(",", "", a$GrasasLub))
a$Solventes <- as.numeric(gsub(",", "", a$Solventes))
a$Naftas <- as.numeric(gsub(",", "", a$Naftas))
a$Ceras <- as.numeric(gsub(",", "", a$Ceras))
a$Butano <- as.numeric(gsub(",", "", a$Butano))
a$MTBE <- as.numeric(gsub(",", "", a$MTBE))
a$GrasasLub <- as.numeric(gsub(",", "", a$GrasasLub))
a$Orimulsion <- as.numeric(gsub(",", "", a$Orimulsion))
a$MezclasOleosas <- as.numeric(gsub(",", "", a$MezclasOleosas))
a$Total <- as.numeric(gsub(",", "", a$Total))



