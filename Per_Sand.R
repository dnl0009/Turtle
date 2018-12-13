## BRING IN SOIL DATA -- Copy-Paste from NRCS WSS ##

setwd("C:/Users/dnl0009/Desktop/")
SOIL <- read.table("WSS.txt",header=TRUE,sep="\t")
SOIL <- SOIL[,c(1,3)]
colnames(SOIL) <- c("Soil_Type","Per_Sand")
SOIL$Per_Sand[is.na(SOIL$Per_Sand)] <- 0

setwd("C:/Users/dnl0009/Documents/ArcGIS/_THESIS/Soil/Types/")
library(foreign)
soilFiles <- list.files(pattern="*.dbf$")
psf <- {}

for (k in 1:length(soilFiles)) {
  psf[[k]] <- read.dbf(soilFiles[k])
} 

# Separate data tables
# S is for sand... or soil

APS <- psf[[1]]
Born1S <- psf[[2]]
Born2S <- psf[[3]]
Born3S <- psf[[4]]
Born4S <- psf[[5]]
Farm1S <- psf[[6]]
Farm2S <- psf[[7]]
Farm3S <- psf[[8]]
Farm4S <- psf[[9]]
FPR1S <- psf[[10]]
FPR2S <- psf[[11]]
FPR3S <- psf[[12]]
FPR4S <- psf[[13]]
FPR5S <- psf[[14]]
FPR6S <- psf[[15]]
FPR7S <- psf[[16]]
FPR8S <- psf[[17]]
FPR9S <- psf[[18]]
HPS <- psf[[19]]
JesseS <- psf[[20]]
Keener1S <- psf[[21]]
Keener2S <- psf[[22]]
LaurelS <- psf[[23]]
ReservoirS <- psf[[24]]
RPS <- psf[[25]]
RRS <- psf[[26]]
UDC1S <- psf[[27]]
UDC2S <- psf[[28]]
WPS <- psf[[29]]

# Create lists of soil types
APS <- APS[,1]
Born1S <- Born1S[,1]
Born2S <- Born2S[,1]
Born3S <- Born3S[,1]
Born4S <- Born4S[,1]
Farm1S <- Farm1S[,1]
Farm2S <- Farm2S[,1]
Farm3S <- Farm3S[,1]
Farm4S <- Farm4S[,1]
FPR1S <- FPR1S[,1]
FPR2S <- FPR2S[,1]
FPR3S <- FPR3S[,1]
FPR4S <- FPR4S[,1]
FPR5S <- FPR5S[,1]
FPR6S <- FPR6S[,1]
FPR7S <- FPR7S[,1]
FPR8S <- FPR8S[,1]
FPR9S <- FPR9S[,1]
HPS <- HPS[,1]
JesseS <- JesseS[,1]
Keener1S <- Keener1S[,1]
Keener2S <- Keener2S[,1]
LaurelS <- LaurelS[,1]
ReservoirS <- ReservoirS[,1]
RPS <- RPS[,1]
RRS <- RRS[,1]
UDC1S <- UDC1S[,1]
UDC2S <- UDC2S[,1]
WPS <- WPS[,1]

Per_Sand <- function(site) {
  site %in% psf
  Percent_Sand <- mean(SOIL$Per_Sand[SOIL$Soil_Type %in% site])
  return(Percent_Sand)
}


RRsand <- mean(SOIL$Per_Sand[SOIL$Soil_Type %in% RRsoil]); RRsand # IT WORKS!!
