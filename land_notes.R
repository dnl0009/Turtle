####### LANDSCAPE COMPARISONS #########

# SET THE STAGE #
library(foreign)
setwd("C:/Users/dnl0009/Documents/ArcGIS/_THESIS/Landscape/")
RP_landuse <- read.dbf("RP_1km.dbf") # read in files
FPR3_ls <- read.dbf("FPR3_1km.dbf")
FPR3_ls$PERCENT_LA <- (FPR3_ls$PERCENT_LA)*100 # I forgot to do this in GIS
#######################################

PLUS <- cbind(RP_landuse[,c(1,4)],FPR3_ls[,4]); PLUS   ## Bind percent values as one
## Don't forget the value column because this will be needed for merging other data together
##### Make sure the value columns are the EXACT SAME as well
colnames(PLUS) <- c("VALUE","RP","FPR_3") 
class(PLUS) # Matrix columns cannot be called using $
PLUS <- as.data.frame(PLUS,row.names= 
                        c("Forested","Grass/Past/Ag","Barren/Dev",
                          "OpenWater","MineGrass","MineBarren",
                          "SMCRAForest","PreSMCRAGrass","PreSMCRABarren",
                          "PreSMCRAForested","HerbaceousWetland",
                          "WoodyWetland","Road")) # Landscape types per row 
PLUS$RP <- round(PLUS$RP,3) # Round values
PLUS$FPR_3 <- round(PLUS$FPR_3,3)
PLUS # TADA!

##### READ IN ALL THE DBF?! ######
myFiles <- list.files(pattern="*.dbf$") # the $ is important because there 
## are some files that end in .dbf.xml and read.dbf can't read those
ldf <- {}
for (k in 1:length(myFiles)) {
  ldf[[k]] <- read.dbf(myFiles[k])
} # this brings all files in as one object with k values
# Separate files.. I bet I can loop this somehow... 
Farm1L <- ldf[[1]]
Farm2L <- ldf[[2]]
Farm3L <- ldf[[3]]
FPR1L <- ldf[[4]]
FPR2L <- ldf[[5]]
FPR4L <- ldf[[7]]
FPR5L <- ldf[[8]]
FPR6L <- ldf[[9]]
FPR7L <- ldf[[10]]
FPR8L <- ldf[[11]]
FPR9L <- ldf[[12]]
HPL <- ldf[[13]]
JesseL <- ldf[[14]]
Keener1L <- ldf[[15]]
LaurelL <- ldf[[16]]
RRL <- ldf[[19]]
UDC1L <- ldf[[20]]
UDC2L <- ldf[[21]]
WPL <- ldf[[22]]

#####
# Fix different length columns by filling NA values
# THIS DID NOT WORK
n <- max(length(PLUS),length(Farm1L[,4]),length(Farm2L[,4]),length(Farm3L[,4]),length(FPR1L[,4]),length(FPR2L[,4]),length(FPR4L[,4]),length(FPR5L[,4]),length(FPR6L[,4]),length(FPR7L[,4]),length(FPR8L[,4]),length(FPR9L[,4]),length(HPL[,4]),length(JesseL[,4]),length(Keener1L[,4]),length(LaurelL[,4]),length(RRL[,4]),length(UDC1L[,4]),length(UDC2L[,4]),length(WPL[,4])); n
length(PLUS)
length(Farm1L) <- n; length(Farm1L); Farm1L
length(Farm2L) <- n
length(Farm3L) <- n
length(FPR1L) <- n
length(FPR2L) <- n
length(FPR4L) <- n
length(FPR5L) <- n
length(FPR6L) <- n
length(FPR7L) <- n
length(FPR8L) <- n
length(FPR9L) <- n
length(HPL) <- n
length(JesseL) <- n
length(Keener1L) <- n
length(LaurelL) <- n
length(RRL) <- n
length(UDC1L) <- n
length(UDC2L) <- n
length(WPL) <- n

# Combine everything ### THIS IS CLOSE ###
PLUS <- rowr::cbind.fill(PLUS,Farm1L[,4],Farm2L[,4],Farm3L[,4],FPR1L[,4],FPR2L[,4],FPR4L[,4],FPR5L[,4],FPR6L[,4],FPR7L[,4],FPR8L[,4],FPR9L[,4],HPL[,4],JesseL[,4],Keener1L[,4],LaurelL[,4],RRL[,4],UDC1L[,4],UDC2L[,4],WPL[,4],fill=NA); PLUS

PLUS <- merge(PLUS,Farm1L[,4],Farm2L[,4],Farm3L[,4],FPR1L[,4],FPR2L[,4],FPR4L[,4],FPR5L[,4],FPR6L[,4],FPR7L[,4],FPR8L[,4],FPR9L[,4],HPL[,4],JesseL[,4],Keener1L[,4],LaurelL[,4],RRL[,4],UDC1L[,4],UDC2L[,4],WPL[,4],by="VALUE",all.x=TRUE,all.y=TRUE); PLUS
