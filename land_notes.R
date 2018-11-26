####### LANDSCAPE COMPARISONS #########

# SET THE STAGE #
library(foreign)
setwd("G:/My Drive/Survey Data/_THESIS/Landscape/")
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
Born1 <- ldf[[1]]
Farm1L <- ldf[[2]]
Farm2L <- ldf[[3]]
Farm3L <- ldf[[4]]
Farm4L <- ldf[[5]]
FPR1L <- ldf[[6]]
FPR2L <- ldf[[7]]
FPR4L <- ldf[[9]]
FPR5L <- ldf[[10]]
FPR6L <- ldf[[11]]
FPR7L <- ldf[[12]]
FPR8L <- ldf[[13]]
FPR9L <- ldf[[14]]
HPL <- ldf[[15]]
JesseL <- ldf[[16]]
Keener1L <- ldf[[17]]
Keener2L <- ldf[[18]]
LaurelL <- ldf[[19]]
ReservoirL <- ldf[[20]]
RRL <- ldf[[23]]
UDC1L <- ldf[[24]]
UDC2L <- ldf[[25]]
WPL <- ldf[[26]]

####################################
## I'll combine the data with similar lengths.

PLUS13 <- cbind(PLUS,FPR1L[,4],FPR2L[,4],FPR4L[,4],FPR5L[,4],FPR6L[,4],FPR7L[,4],FPR9L[,4],RRL[,4],UDC1L[,4]); PLUS13
colnames(PLUS13) <- c("VALUE","RP","FPR3","FPR1","FPR2","FPR4","FPR5","FPR6","FPR7","FPR9","RR","UDC1"); PLUS13
PLUS13[,4:12] <- round(PLUS13[,4:12],3); PLUS13 ## This works

PLUS12 <- cbind(Farm2L[,c(1,4)],HPL[,4],UDC2L[,4],WPL[,4])
colnames(PLUS12) <- c("VALUE","Farm2","HPL","UDC2","WPL")
PLUS12[,2:5] <- round(PLUS12[,2:5],3)
PLUS12

PLUS1 <- merge(PLUS13,PLUS12,by="VALUE",all=TRUE)

PLUS9 <- cbind(Farm1L[,c(1,4)],Keener1L[,4],LaurelL[,4],Keener2L[,4],Born1[,4])
colnames(PLUS9) <- c("VALUE","Farm1","Keener1","Laurel","Keener2","Born1")
PLUS9[,2:6] <- round(PLUS9[,2:6],3) # Don't forget the ,3; otherwise it'll round to the nearest 1
PLUS9

PLUS1 <- merge(PLUS1,PLUS9,by="VALUE",all=TRUE); PLUS1 # Don't forget the ALL=TRUE
#Otherwise, it'll cut off the data to the shortest dataframes

PLUS10 <- cbind(FPR8L[,c(1,4)],JesseL[,4])
colnames(PLUS10) <- c("VALUE","FPR8","Jesse")
PLUS10[,2:3] <- round(PLUS10[,2:3],3)
PLUS10

PLUS1 <- merge(PLUS1,PLUS10,by="VALUE",all=T); PLUS1

PLUS8 <- cbind(Farm3L[,c(1,4)],Farm4L[,4],ReservoirL[,4])
colnames(PLUS8) <- c("VALUE","Farm3","Farm4","Reservoir")
PLUS8[,2:4] <- round(PLUS8[,2:4],3)
PLUS8

PLUS1 <- merge(PLUS1,PLUS8,by="VALUE",all=T); PLUS1
PLUS <- PLUS1[,c(1,4,5,3,6,7,8,9,22,10,12,15,11,2,26,23,14,16,21,18,20,19,17,13,24,25)]; PLUS
colnames(PLUS) <- c("VALUE","FPR1","FPR2","FPR3","FPR4","FPR5","FPR6","FPR7","FPR8","FPR9","UDC1","UDC2","RR","RP","Reservoir","Jesse","Hodges","Wolfe","Born1","Keener1","Keener2","Laurel","Farm1","Farm2","Farm3","Farm4")
rownames(PLUS) <- c("Forested","Grass/Past/Ag","Barren/Dev","OpenWater",
                    "MineGrass","MineBarren","SMCRAForest","PreSMCRAGrass",
                    "PreSMCRABarren","PreSMCRAForested","HerbaceousWetland",
                    "WoodyWetland","Road")
PLUS <- PLUS[,2:26] # Don't need the value column anymore 

###################################################################
###################################################################
#################### MOVING ON ####################################
####################  TO THE   ####################################
#################### GRAPHING  ####################################
###################################################################
###################################################################

install.packages("colorRamps") # Make sure package is in quotes
library(colorRamps) # Call package
PLUS <- as.matrix(PLUS) # Need to get rid of the NA values I worked so hard to get -- HA!
PLUS[is.na(PLUS)] <- 0; PLUS # Because the bars cannot be drawn with NA values

plot1 <- barplot(prop.table(PLUS,2),main="Landscape Mosaics within 1 km of Each Site",
                 font.main=4,xlab="Site",ylab="Proportion of Hectares (%)",
                 col=colorRamps::primary.colors(13),las=2,cex.names = 0.8,font.lab=2)
legend("bottomleft",bg="white",legend=c("Forested","Grass/Past/Ag","Barren/Dev",
                                        "OpenWater","MineGrass","MineBarren",
                                        "SMCRAForest","PreSMCRAGrass","PreSMCRABarren",
                                        "PreSMCRAForested","HerbaceousWetland",
                                        "WoodyWetland","Road"),cex=0.8, inset= c(0.08,0.02), fill=colorRamps::primary.colors(13))

## EXACTEMENT! :D
## cex = 0.5, inset = c(0.175,0.01) for laptop 
## I think I have different versions on my different computers.

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

survey <- PLUS[,c(1:3,12:14)] # Pick out sites that have been surveyed for presab comparison
t(survey) # Flip data frame for adding presab column (for both CP and CS) 
## T stands for TRANSPOSE, which means "to trade places"


