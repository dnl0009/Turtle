####### LANDSCAPE COMPARISONS #########

# SET THE STAGE #
library(foreign)
setwd("C:/Users/dnl0009/Documents/ArcGIS/_THESIS/Landscape/")
RP_landuse <- read.dbf("RP_1km.dbf") # read in files
FPR3_ls <- read.dbf("FPR3_1km.dbf")
#######################################

PLUS <- cbind(RP_landuse[,4],FPR3_ls[,4])   ## Bind percent values as one
colnames(PLUS) <- c("RP","FPR_3") 
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

#############################################
FPR5L <- ldf[[8]]
FPR6L <- ldf[[9]]
FPR7L <- ldf[[10]]
FPR8L <- ldf[[11]]
FPR9L <- ldf[[12]]
HPL <- ldf[[13]]
JesseL <- ldf[[14]]
Keener1L <- ldf[[15]]
LaurelL <- lbf[[16]]
RRL <- lbf[[19]]
UDC1L <- lbf[[20]]
UDC2L <- lbf[[21]]
WPL <- lbf[[22]]